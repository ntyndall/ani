#* @apiTitle Analytics - Northern Ireland
#*
#* @apiDescription Manage and display relevant data based on tourism
#*   finance, etc.
#*
#* @apiTag gardens Responsible for returning information based on
#*   the locations of historic parks and gardens.

library(magrittr)
library(purrr)

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
  allKeys %<>% order_keys()

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
