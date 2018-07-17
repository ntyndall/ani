#' @title Order Keys
#'
#' @export

order_keys <- function(allKeys, mapBy = 3) {
  return(
    allKeys %>% `[`(
      1:(allKeys %>% length) %>% match(
        allKeys %>%
          strsplit(split = '/') %>%
          purrr::map(mapBy) %>%
          purrr::flatten_chr() %>%
          as.integer
      )
    )
  )
}
