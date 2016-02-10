#' Create initial definitions table
#'
#' @param id A string with the variable that contains the id identifier
#' @return A data.table that has zeros and blanks
#' @export
#'

initDefs <- function(id="id") {

  dt <- data.table::data.table()
  attr(dt,"id") <- id

  return(dt)

}
