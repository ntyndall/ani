#' @title Return Closest
#'
#' @description A function returning the closest by distance
#'  from a single location.
#'
#' @details This function takes a vector of redis keys that
#'  should contain the following fields in the hash,
#'  \itemize{
#'    \item{CENT_LONG: The longitute of the centroid}
#'    \item{CENT_LAT: The latitude of the centroid}
#'  }
#' then returns the keys that are closest.
#'
#' @export


return_closest <- function(dbr, allKeys, currentLoc, returnItems) {
  # Set up a redis pipeline
  rPipe <- redux::redis

  # Get all Centroid data
  newLoc <- dbr$pipeline(
    .commands = lapply(
      X = 1:(allKeys %>% length),
      FUN = function(x) rPipe$HMGET(
        key = allKeys[x],
        field = c('CENT_LONG', "CENT_LAT")
      )
    )
  )

  # Create vectors
  c_vec <- function(x, y) x %>% purrr::map(y) %>% purrr::flatten_chr() %>% as.numeric

  # Create a matrix
  distMatrix <- c(newLoc %>% c_vec(1), newLoc %>% c_vec(2)) %>%
    matrix(ncol = 2)

  # Calculate distances using the haversine distance method (in km)
  allDist <- geosphere::distm(
    x = currentLoc,
    y = distMatrix,
    fun = geosphere::distHaversine
  ) %>%
    as.double %>%
    `/`(1000)

  # Sort by size
  return(
    allKeys %>% `[`(
      allDists %>%
        sort.int(
          decreasing = FALSE,
          index.return = TRUE
        ) %>%
        `$`('ix') %>%
        head(returnItems)
    )
  )
}
