#* @apiTitle Analytics - Northern Ireland
#*
#* @apiDescription Manage and display relevant data based on tourism
#*   finance, etc.
#*
#* @apiTag gardens Responsible for returning information based on
#*   the locations of historic parks and gardens.

library(magrittr)
library(purrr)
library(ggmap)

API_LINK <- "api.postcodes.io/postcodes/"

#* Log some information about the incoming request
#*
#* @filter dbconnection


function(req) {

  # Create redis connection for query
  req$dbr <- aniR::red()

  # Log the request
  cat(
    as.character(Sys.time()), "-",
    req$REQUEST_METHOD, req$PATH_INFO, "-",
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
  )

  # Forward on
  plumber::forward()
}

#* Echo the parameter that was sent in
#*
#* @param req The incoming request details.
#*
#* @get /


function(req) {
  allKeys <- '*' %>%
    req$dbr$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = '/') %>%
    purrr::map(1) %>%
    purrr::flatten_chr()

  allKeys %>% table %>% as.list
}

#* Return historic parks and gardens, if _postcode_ is supplied
#* then return the closest results
#*
#* @param postcode:character An NI postcode
#* @param returnItems:int The number of results the API should return
#*
#* @get /gardens
#*
#* @response 200 Success
#* @response 400 Invalid input provided
#*
#* @serializer unboxedJSON
#*
#* @tag gardens


function(req, res, postcode = "", returnItems = 1) {

  # Make sure values are integers
  returnItems %<>% as.integer

  # Rename database connection
  dbr <- req$dbr

  # Get all the keys back
  allKeys <- "park+garden/id" %>%
    paste0("*") %>%
    dbr$KEYS() %>%
    purrr::flatten_chr()

  # Set returnItems
  returnItems <- if (returnItems == 0) {
    allKeys %>% length
  } else {
    returnItems %>% min(allKeys %>% length)
  }

  # Sort by the ID
  allKeys %<>% aniR::order_keys()

  # If postcode exists then search for it
  if (postcode %>% `!=`("")) {

    # Query using the supplied postcode
    results <- API_LINK %>%
      paste0(postcode) %>%
      httr::GET()

    # If postcode is successful then look up database
    if (results$status_code == 200) {

      # Convert the results to a data frame
      my.data <- results$content %>%
        rawToChar() %>%
        jsonlite::fromJSON()

      # Create a vector of the current location via long + lat
      currentLoc <- c(my.data$result$longitude, my.data$result$latitude) %>%
        as.double

      # Collect ALL distances!
      allDists <- c()
      for (i in 1:(allKeys %>% length)) {
        newLoc <- allKeys[i] %>%
          dbr$HMGET(field = c('CENT_LONG', "CENT_LAT")) %>%
          purrr::flatten_chr() %>%
          as.double

        # Calculate distance based on long + lat's
        res <- geosphere::distm(
          x = currentLoc,
          y = newLoc,
          fun = geosphere::distHaversine
        )

        # Distance in Km
        allDists %<>% c(res %>% as.double %>% `/`(1000))
      }

      # Sort by size
      allKeys %<>% `[`(
        allDists %>%
          sort.int(
            decreasing = FALSE,
            index.return = TRUE
          ) %>%
          `$`('ix') %>%
          head(returnItems)
      )

      # Get all
    } else {
      res$status <- 400 # Bad request
    }
  }

  # Return table list
  if (postcode %>% `==`("")) {
    bigList <- allKeys %>% table %>% as.list
  } else {
    bigList <- list()
    for (j in 1:returnItems) {
      res <- allKeys[j] %>% dbr$HGETALL() %>% purrr::flatten_chr()
      newres <- res[c(FALSE, TRUE)] %>% as.list
      names(newres) <- res[c(TRUE, FALSE)]
      bigList %<>% c(newres %>% list)
    }
  }

  # Return bigList
  bigList
}

#* Return historic parks and gardens, if _postcode_ is supplied
#* then return the closest results
#*
#* @param postcode:character An NI postcode
#* @param returnItems:int The number of results the API should return
#*
#* @get /gardens/map
#*
#* @response 200 Success
#* @response 400 Invalid input provided
#*
#* @png (width = 800, height=800)


function(req, postcode = "") {

  # Rename db connection from incoming request
  dbr <- req$dbr

  # Calculate center from postcode
  results <- API_LINK %>%
    paste0(postcode) %>%
    httr::GET()

  # Convert the results to a data frame
  my.data <- results$content %>%
    rawToChar() %>%
    jsonlite::fromJSON()

  # Create a vector of the current location via long + lat
  currentCenter <- c(my.data$result$longitude, my.data$result$latitude) %>%
    as.double

  # Create a flattened numeric vector
  as_vec <- function(x) x %>% purrr::flatten_chr() %>% as.numeric

  # Just pick these at random for testing
  vals <- c(73, 69, 72, 77, 66)

  # Collect all the data (still need to account for multiple polygons)
  all.data <- list()
  for (i in 1:5) {
    test.data <- data.frame(
      long = dbr$LRANGE(
        key = paste0("park+garden/long/", vals[i]),
        start = 0,
        stop = -1
      ) %>%
        as_vec(),
      lat = dbr$LRANGE(
        key = paste0("park+garden/lat/", vals[i]),
        start = 0,
        stop = -1
      ) %>%
        as_vec(),
      stringsAsFactors = FALSE
    )

    # Always check the extremeties..
    if (test.data$long %>% min %>% `<`(longMinMax[1]) || i == 1) longMinMax[1] <- test.data$long %>% min
    if (test.data$long %>% max %>% `>`(longMinMax[2]) || i == 1) longMinMax[2] <- test.data$long %>% max
    if (test.data$lat %>% min %>% `<`(latMinMax[1]) || i == 1) latMinMax[1] <- test.data$lat %>% min
    if (test.data$lat %>% max %>% `>`(latMinMax[2]) || i == 1) latMinMax[2] <- test.data$lat %>% max

    # Save all the data in a list for later!
    all.data %<>% c(test.data %>% list)
  }

  # Add the center into min and max..
  if (currentCenter[1] < longMinMax[1]) longMinMax[1] <- currentCenter[1]
  if (currentCenter[1] > longMinMax[2]) longMinMax[2] <- currentCenter[1]
  if (currentCenter[2] < latMinMax[1]) latMinMax[1] <- currentCenter[2]
  if (currentCenter[2] > latMinMax[2]) latMinMax[2] <- currentCenter[2]

  # Spread out just a little (to be on the safe side)
  longMinMax[1] %<>% `-`(0.01)
  longMinMax[2] %<>% `+`(0.01)
  latMinMax[1] %<>% `-`(0.01)
  latMinMax[2] %<>% `+`(0.01)

  # Calculate zoom factor here
  zoomVal <- list(
    long = longMinMax,
    lat = latMinMax
  ) %>%
    aniR::calculate_zoom()

  # Calculate new center position
  newCenter <- c(
    longMinMax[2] %>% `-`(longMinMax[1]) %>% `/`(2) %>% `+`(longMinMax[1]),
    latMinMax[2] %>% `-`(latMinMax[1]) %>% `/`(2) %>% `+`(latMinMax[1])
  )

  # Define the map from google maps
  g <- ggmap::get_googlemap(
    center = newCenter,
    maptype = 'roadmap',
    zoom = zoomVal,
    scale = 2
  ) %>%
    ggmap::ggmap()

  # Add in all the polygons
  for (i in 1:5) {
    single.data <- all.data[[i]]
    g %<>% `+`(
      ggplot2::geom_polygon(
        data = ggplot2::fortify(single.data),
        mapping = ggplot2::aes(long, lat),
        fill = "orange",
        colour = "red",
        alpha = 0.2
      )
    )
  }

  # Return the plot back from the endpoint
  plot(g)
}
