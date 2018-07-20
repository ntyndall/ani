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

  # This is only true for longitude (need to fix for latitude as well!)
  startingInd <- 10
  type <- c("long", "lat")

  # These are the half tick max length allowed
  newVals <- aniR::values

  bothZooms <- c()
  for (i in 1:(type %>% length)) {
    # Predict lat / long fraction
    frac <- predict(aniR::centerFit, data.frame(x = zoomExtremes$center[i] %>% abs)) %>%
      as.numeric %>%
      min(1)

    # Get zooms
    finalZoom <- zoomExtremes[[type[i]]][2] %>%
      `-`(zoomExtremes$center[i]) %>%
      `<`(newVals * frac) %>%
      sum %>%
      `-`(1) %>%
      `+`(startingInd)

    bothZooms %<>% c(finalZoom)
  }

  return(bothZooms %>% min)
}
