#' @title Calculate Centroid
#'
#' @description Calculates the centroid from a vector of
#'  longitdues and latitudes and updates the appropriate
#'  redis key.
#'
#' @export


calculate_centroid <- function(dbr, rKey, geoms) {

  # Calculate centroid from multiple geometries
  result <- c(0, 0)
  for (i in 1:(geoms %>% length)) {
    result %<>% `+`(
      geoms[[i]] %>%
        matrix(ncol = 2) %>%
        geosphere::centroid() %>%
        as.double
    )
  }

  # Update the redis key with centroid results
  rKey %>% dbr$HMSET(
    field = c('CENT_LONG', "CENT_LAT"),
    value = result %>% `/`(geoms %>% length)
  )
}
