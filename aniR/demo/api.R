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
library(ggplot2)

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

  # Get all the sorted keys back
  allKeys <- dbr %>%
    aniR::order_keys(
      keyType = "park+garden/id"
    )

  # Set returnItems
  returnItems <- if (returnItems == 0) {
    allKeys %>% length
  } else {
    returnItems %>% min(allKeys %>% length)
  }

  # If postcode exists then search for it
  if (postcode %>% `!=`("")) {

    # Get postcode information
    postC <- API_LINK %>%
      aniR::get_postcode(
        postcode = postcode
      )

    if (postC$status == 200) {

      # Return the closest locations from the postcode
      closest <- dbr %>% aniR::return_closest(
        allKeys = allKeys,
        currentLoc = postC$location,
        returnItems = returnItems
      )
    } else {
      res$status <- postC$status
    }
  }

  # Return table list
  if (postcode %>% `==`("")) {
    bigList <- allKeys %>% table %>% as.list
  } else {
    bigList <- list()
    for (j in 1:returnItems) {
      # Get all hash results
      res <- closest[j] %>%
        dbr$HGETALL() %>%
        purrr::flatten_chr()

      # Convert to named list
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
#* @param num:int The number of results the API should return
#* @param dark:bool True or False for light or dark map
#*
#* @get /gardens/map
#*
#* @response 200 Success
#* @response 400 Invalid input provided
#*
#* @serializer htmlwidget


function(req, res, postcode = "", num = 1, dark = FALSE) {

  # Need to make sure this is an int!
  num %<>% as.integer

  # Rename db connection from incoming request
  dbr <- req$dbr

  # Get all the sorted keys back
  allKeys <- dbr %>%
    aniR::order_keys(
      keyType = "park+garden/id"
    )

  # Calculate longitutde and latitude from postcode
  if (postcode %>% `!=`("")) {
    postC <- API_LINK %>%
      aniR::get_postcode(
        postcode = postcode
      )

    if (postC$status == 200) {
      currentCenter <- postC$location

      # Create a flattened numeric vector
      as_vec <- function(x) x %>% purrr::flatten_chr() %>% as.numeric

      # Get the closest indexes
      closest <- dbr %>% aniR::return_closest(
        allKeys = allKeys,
        currentLoc = currentCenter,
        returnItems = num
      ) %>%
        strsplit(split = "/") %>%
        purrr::map(3) %>%
        purrr::flatten_chr() %>%
        as.integer

      # Get geometry keys + collect all the data
      results <- dbr %>%
        aniR::geoms_from_redis(
          geomType = "park+garden",
          closest = closest
        )

      # Rename here for ease
      all.data <- results$data
      longMinMax <- results$long
      latMinMax <- results$lat
      oids <- results$oids

      # Add the center into min and max..
      if (currentCenter[1] < longMinMax[1]) longMinMax[1] <- currentCenter[1]
      if (currentCenter[1] > longMinMax[2]) longMinMax[2] <- currentCenter[1]
      if (currentCenter[2] < latMinMax[1]) latMinMax[1] <- currentCenter[2]
      if (currentCenter[2] > latMinMax[2]) latMinMax[2] <- currentCenter[2]

      # Calculate zoom factor here
      zoomVal <- list(
        long = longMinMax,
        lat = latMinMax,
        center = currentCenter
      ) %>%
        aniR::calculate_zoom()

      # Calculate new center position
      newCenter <- c(
        longMinMax[2] %>% `-`(longMinMax[1]) %>% `/`(2) %>% `+`(longMinMax[1]),
        latMinMax[2] %>% `-`(latMinMax[1]) %>% `/`(2) %>% `+`(latMinMax[1])
      )

      # Set up data frame of postcode query for the plot
      myLocation <- data.frame(
        x = currentCenter[1],
        y = currentCenter[2],
        stringsAsFactors = FALSE
      )

      # Get all data and get it ready for leaflet
      new.data <- data.frame()
      uniqGroups <- all.data$group %>% unique
      for (k in 1:(uniqGroups %>% length)) {
        subs.data <- all.data %>% subset(all.data$group %>% `==`(uniqGroups[k]))
        new.data %<>% rbind(subs.data[ , 1:2])
        if (k != (uniqGroups %>% length)) new.data %<>% rbind(NA)
      }

      # Create the widget here
      m <- new.data %>%
        as.matrix %>%
        leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(color = "red", fillColor = "red") %>%
        leaflet::addMarkers(lng = myLocation$x, lat = myLocation$y, popup = "Current Location")

      if (dark) m %<>% leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter)
    } else {
      res$status <- postC$status
    }
  } else {
    m <- leaflet::leaflet() %>%
      leaflet::addTiles()
  }

  # Return the plot back from the endpoint
  return(m)
}
