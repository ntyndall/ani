#' @title Order Keys
#'
#' @export

order_keys <- function(dbr, keyType, mapBy = 3) {
  allKeys <- keyType %>%
    paste0("*") %>%
    dbr$KEYS() %>%
    purrr::flatten_chr()

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
