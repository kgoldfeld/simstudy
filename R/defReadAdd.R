#' Read external csv data set definitions for adding columns
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @return A data.table with data set definitions
#' @export
#'

defReadAdd <- function(filen) {

  if (! file.exists(filen)) stop("No such file")

  read.df <- utils::read.csv(filen, header=TRUE, as.is = TRUE) # do not read as factors
  read.dt <- data.table::data.table(read.df)

  if (! all(names(read.dt) == c("varname",
                                "formula", "variance", "dist", "link"))) {

    stop("Field names do not match")

  }

  return(read.dt)

}
