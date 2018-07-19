#' @title Get Hex Codes
#'
#' @export


get_hex_codes <- function(n) {

  # This library only produces 74, if asking for more then we are in trouble!
  if (n > 74) stop("Need to ask for less items")

  # Get the unique colours
  bPal <- RColorBrewer::brewer.pal.info
  qual_pals <- bPal[bPal$category == "qual", ]

  # Get a vector of hex codes
  cols <- mapply(RColorBrewer::brewer.pal, qual_pals$maxcolors, qual_pals %>% rownames) %>%
    unlist

  # Return the number of colours required
  return(cols[1:n])
}
