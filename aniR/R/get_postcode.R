#' @title Get Postcode
#'
#' @export


get_postcode <- function(API_LINK, postcode) {
  # Query using the supplied postcode
  results <- API_LINK %>%
    paste0(postcode) %>%
    httr::GET()

  # Return a list of status + results
  return(
    if (results$status_code == 200) {

      # Convert the results to a data frame
      dat <- results$content %>%
        rawToChar() %>%
        jsonlite::fromJSON() %>%
        `$`('result')

      # Create a vector of the current location via long + lat
      list(
        status = results$status_code,
        location = c(dat$longitude, dat$latitude) %>% as.double
      )
    } else {
      list(status = 400, location = c())
    }
  )
}
