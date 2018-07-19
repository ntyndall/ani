#' @title Calculate Zoom
#'
#' @description A function to calculate the correct zoom
#'  parameter from a list of longitude and latitude extremeties.
#'
#' @param zoomExtremes A named list containing
#'   \itemize{
#'     \item{long: A vector of length two containing the min and max longitudes}
#'     \item{lat: A vector of length two containing the min and max latitudes}
#'   }
#'
#' @return An appropriate zooming factor to encapsulate all co-ordinates.
#'
#' @export


calculate_zoom <- function(zoomExtremes) {
  startingInd <- 10
  zoomBoundaries <- c(0.42, 0.22, 0.12, 0.052, 0.025, 0.0125)
  type <- c("long", "lat")

  return(
    sapply(
      X = 1:(type %>% length),
      FUN = function(i) {
        zoomExtremes[[type[i]]][2] %>%
          `-`(zoomExtremes[[type[i]]][1]) %>%
          `/`(2) %>%
          `<`(zoomBoundaries) %>%
          as.logical %>%
          sum %>%
          `+`(startingInd) %>%
          `-`(1)
       }
    ) %>% min
  )
}

