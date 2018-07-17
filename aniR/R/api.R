library(magrittr)
library(purrr)

API_LINK <- "api.postcodes.io/postcodes/"

#' Log some information about the incoming request
#'
#' @filter dbconnection


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

#' Echo the parameter that was sent in
#'
#' @param req The incoming request details.
#'
#' @get /


function(req) {
  allKeys <- '*' %>%
    req$dbr$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = '/') %>%
    purrr::map(1) %>%
    purrr::flatten_chr()

  allKeys %>% table %>% as.list
}

#' Echo the parameter that was sent in
#'
#' @param msg The message to echo back.
#'
#' @get /gardens


function(req, res, postcode = NULL, returnVals = NULL) {


  # Get all the keys back
  allKeys <- "park+garden/id" %>%
    paste0("*") %>%
    dbr$KEYS() %>%
    purrr::flatten_chr()

  # Set returnVals
  returnVals <- if (returnVals %>% is.null) {
    allKeys %>% length
  } else {
    returnVals %>% min(allKeys %>% length)
  }

  # Sort by the ID
  allKeys %<>% order_keys()

  # If postcode exists then search for it
  if (postcode %>% is.null %>% `!`()) {
    results <- API_LINK %>%
      paste0(postcode) %>%
      httr::GET()
    if (results$status_code == 200) {
      my.data <- results$content %>%
        rawToChar() %>%
        jsonlite::fromJSON()
      currentLoc <- c(my.data$result$longitude, my.data$result$latitude) %>% as.double

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

      allDists %>%
        sort %>%
        tail(returnVals = 5) %>%
        rev

      # Get all
    } else {
      res$status <- 400 # Bad request
    }
  }

  # Return table list
  allKeys %>% table %>% as.list
}

