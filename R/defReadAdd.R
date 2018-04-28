#' Read external csv data set definitions for adding columns
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @return A data.table with data set definitions
#' @examples
#' # Create temporary external "csv" files
#'
#' test1 <- c("varname,formula,variance,dist,link",
#'            "nr,7, 0,nonrandom,identity"
#'           )
#'
#' tfcsv1 <- tempfile()
#' writeLines(test1, tfcsv1)
#'
#' test2 <- c("varname,formula,variance,dist,link",
#'            "x1,.4, 0,binary,identity",
#'            "y1,nr + x1 * 2,8,normal,identity",
#'            "y2,nr - 0.2 * x1,0,poisson, log"
#'           )
#'
#' tfcsv2 <- tempfile()
#' writeLines(test2, tfcsv2)
#'
#' # Generate data based on external definitions
#'
#' defs <- defRead(tfcsv1)
#' dt <- genData(5, defs)
#' dt
#'
#' # Add additional data based on external definitions
#'
#' defs2 <- defReadAdd(tfcsv2)
#' dt <- addColumns(defs2, dt)
#' dt
#'
#' unlink(tfcsv1)
#' unlink(tfcsv2)
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

  return(read.dt[])

}
