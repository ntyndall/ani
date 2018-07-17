#' @title Calculate Centroid
#'
#' @description Calculates the centroid from a vector of
#'  longitdues and latitudes and updates the appropriate
#'  redis key.
#'
#' @export


calculate_centroid <- function(dbr, rKey, geoms) {

  # Calculate centroid from geometries
  result <- geoms %>%
    matrix(ncol = 2) %>%
    geosphere::centroid() %>%
    as.double

  # Update the redis key with centroid results
  rKey %>% dbr$HMSET(
    field = c('CENT_LONG', "CENT_LAT"),
    value = result
  )
}
