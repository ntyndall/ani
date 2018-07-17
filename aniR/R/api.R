library(magrittr)
library(purrr)

#' Echo the parameter that was sent in
#'
#' @param msg The message to echo back.
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
