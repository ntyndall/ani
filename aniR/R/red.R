#' @title Create Redis Connection
#'
#' @param db An integer value defining the database number
#'  to store data.
#'
#' @return A redux redis connection
#'
#' @export

red <- function(db = 9) {
  return(
    redux::hiredis(db = db)
  )
}
