#' @title Park and Garden Data
#'
#' @description A function to insert park and garden data,
#'  original data source from \code{https://www.opendatani.gov.uk/dataset/c6529642-8fa0-4205-bd56-734e9fda2cb0/resource/bbb60928-d39b-4660-8bef-280f17e70bd8/download/hpag.geojson}.
#'
#' @param dbr A redis environment containing all redis
#'  functionality.
#'
#' @export

park_garden_data <- function(dbr, update = FALSE) {

  # Define the root redis key
  root <- "park+garden"

  # Check if the keys already exist
  keysExist <- root %>%
    paste0("*") %>%
    dbr$KEYS() %>%
    length %>%
    `>`(0) %>%
    `!`()

  # Load data from file and insert to redis
  if (update || keysExist) {
    raw.data <- getwd() %>%
      paste0('/external_data/park_garden_data.json') %>%
      jsonlite::fromJSON()

    raw.data %<>% `$`('features')
    geometry <- raw.data$geometry$coordinates

    # Loop over each data object
    for (i in 1:(raw.data$type %>% length)) {
      current <- raw.data$properties[i, ]
      rKey <- root %>% paste0('/', current$OBJECTID)
      rKey %>% dbr$HMSET(
        field = current %>% names,
        value = current %>% as.character
      )

      # Flatten to double, split in half and store in long / lat
      geoms <- geometry[[i]] %>% as.double

      # Create the long + lat list names
      keyLoc <- rKey %>%
        paste0('/', c("long", "lat"))

      # Define the halfway point of the long list
      halfway <- geoms %>%
        length %>%
        `/`(2)

      # Calculate the centroid and push to redis key
      dbr %>% calculate_centroid(
        rKey = rKey,
        geoms = geoms,
        halfway = halfway
      )

      # Push the longitudes + latitudes to appropriate lists
      for (j in 1:2) {

        # Get the lower index
        loInd <- j %>%
          `-`(1) %>%
          `*`(halfway) %>%
          `+`(1)

        # Get the upper index
        hiInd <- j %>%
          `*`(halfway)

        # Push the values to the appropriate key
        keyLoc[j] %>% dbr$RPUSH(
          value = geoms[loInd:hiInd]
        )
      }
    }
  }
}
