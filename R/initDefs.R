#' Create initial definitions table
#'
#' @param id A string with the variable that contains the id identifier
#' @return A data.table that has zeros and blanks
#' @export
#'

init.defs <- function(id="id") {

  data.table::data.table(id=id,
             varname="",
             formula="",
             variance=0,
             min=0,
             max=0,
             dist="",
             link="",
             cMethod="",
             nIperC = 0,
             nClust = 0,
             nTrt = 0,
             varcat="",
             missType="")
}