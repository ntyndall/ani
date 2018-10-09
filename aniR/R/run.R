#' @title Run
#'
#' @importFrom magrittr %>% %<>%
#'
#' @export


run <- function(portNum = 8003) {
  # Source the API
  p <- system.file("extdata", "api.R", package = "aniR") %>%
    plumber::plumb() %>%
    `$`("run")

  # Run on designated port
  p(port = portNum)
}
