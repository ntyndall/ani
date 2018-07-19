#' @title Geoms From Redis
#'
#' @export


geoms_from_redis <- function(dbr, geomType, closest) {

  # Create longitude redis key search first
  matcher <- geomType %>%
    paste0("/long/") %>%
    paste0(closest, "*")

  # Only need to check one of them!
  longFields <- dbr$pipeline(
    .commands = lapply(
      X = 1:(closest %>% length),
      FUN = function(x) matcher[x] %>% rPipe$KEYS()
    )
  ) %>%
    purrr::flatten() %>%
    purrr::flatten_chr()

  # Now create the matching latitude fields
  latFields <- geomType %>%
    paste0("/lat/") %>%
    paste0(
      longFields %>%
        strsplit(
          split = geomType %>% paste0("/long/"),
          fixed = TRUE
        ) %>%
        purrr::map(2) %>%
        purrr::flatten_chr()
    )

  # Get all co-ordinates from redis
  all.data <- list()
  for (i in 1:(longFields %>% length)) {
    fr <- data.frame(
      long = dbr$LRANGE(
        key = longFields[i],
        start = 0,
        stop = -1
      ) %>%
        as_vec(),
      lat = dbr$LRANGE(
        key = latFields[i],
        start = 0,
        stop = -1
      ) %>%
        as_vec(),
      stringsAsFactors = FALSE
    )

    # Always check the extremeties..
    if (fr$long %>% min %>% `<`(longMinMax[1]) || i == 1) longMinMax[1] <- fr$long %>% min
    if (fr$long %>% max %>% `>`(longMinMax[2]) || i == 1) longMinMax[2] <- fr$long %>% max
    if (fr$lat %>% min %>% `<`(latMinMax[1]) || i == 1) latMinMax[1] <- fr$lat %>% min
    if (fr$lat %>% max %>% `>`(latMinMax[2]) || i == 1) latMinMax[2] <- fr$lat %>% max

    # Save all the data in a list for later!
    all.data %<>% c(fr %>% list)
  }

  # Due to multiple geometries, need to recalculate Object IDs
  oids <- longFields %>%
    strsplit(split = "/") %>%
    purrr::map(3) %>%
    purrr::flatten_chr() %>%
    as.integer

  # Return a named list of important information
  return(
    list(
      data = all.data,
      long = longMinMax,
      lat = latMinMax,
      oids = oids
    )
  )
}
