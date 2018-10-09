#' @title Geoms From Redis
#'
#' @export


geoms_from_redis <- function(dbr, geomType, closest) {

  # Create a flattened numeric vector
  as_vec <- function(x) x %>% purrr::flatten_chr() %>% as.numeric

  # Define rpipeline
  rPipe <- redux::redis

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

  # Due to multiple geometries, need to recalculate Object IDs
  oids <- longFields %>%
    strsplit(split = "/") %>%
    purrr::map(3) %>%
    purrr::flatten_chr() %>%
    as.integer

  # Initialise value storer + minmax's
  stored <- c()
  colInd <- 0

  # Get all co-ordinates from redis
  all.data <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:(longFields %>% length)) {

    # ...
    if (oids[i] %in% stored %>% `!`()) {
      colInd %<>% `+`(1)
      stored %<>% c(oids[i])

      # Get Item name
      itemName <- geomType %>%
        paste0("/id/") %>%
        paste0(oids[i]) %>%
        dbr$HGET("SITE") %>%
        tolower %>%
        Hmisc::capitalize()
    }

    # Build data frame from redis list calls
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

    # Assign a factor of group
    fr$group <- paste0(colInd, ' : ', itemName)

    # Save all the data in a list for later!
    all.data %<>% rbind(fr)
  }

  # Return a named list of important information
  return(
    list(
      data = all.data,
      long = c(all.data$long %>% min, all.data$long %>% max),
      lat = c(all.data$lat %>% min, all.data$lat %>% max),
      oids = oids
    )
  )
}
