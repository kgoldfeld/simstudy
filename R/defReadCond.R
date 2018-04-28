#' Read external csv data set definitions for adding columns
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @return A data.table with data set definitions
#' @examples
#' # Create temporary external "csv" files
#'
#' test1 <- c("varname,formula,variance,dist,link",
#'            "x,0.3;0.4;0.3,0,categorical,identity"
#'           )
#'
#' tfcsv1 <- tempfile()
#' writeLines(test1, tfcsv1)
#'
#' test2 <- c("condition,formula,variance,dist,link",
#'            "x == 1, 0.4,0,binary,identity",
#'            "x == 2, 0.6,0,binary,identity",
#'            "x >= 3, 0.8,0,binary,identity"
#'           )
#'
#' tfcsv2 <- tempfile()
#' writeLines(test2, tfcsv2)
#'
#' # Generate data based on external definitions
#'
#' defs <- defRead(tfcsv1)
#' dt <- genData(2000, defs)
#' dt
#'
#' # Add column based on
#'
#' defsCond <- defReadCond(tfcsv2)
#' dt <- addCondition(defsCond, dt, "y")
#' dt
#'
#' dt[, mean(y), keyby = x]
#'
#' unlink(tfcsv1)
#' unlink(tfcsv2)
#' @export
#'

defReadCond <- function(filen) {

  if (! file.exists(filen)) stop("No such file")

  read.df <- utils::read.csv(filen, header=TRUE, as.is = TRUE) # do not read as factors
  read.dt <- data.table::data.table(read.df)

  if (! all(names(read.dt) == c("condition", "formula", "variance", "dist", "link"))) {

    stop("field names do not match")

  }

  return(read.dt[])

}
