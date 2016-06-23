#' Read external csv data set definitions
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @param id string that includes name of id field. Defaults to "id"
#' @return A data.table with data set definitions
#' @export
#'

defRead <- function(filen, id = "id") {

  # 'declare var

  varname = NULL
  formula = NULL
  dist = NULL

  #

  if (! file.exists(filen)) stop("No such file")

  read.df <- utils::read.csv(filen, header=TRUE, as.is = TRUE) # do not read as factors
  read.dt <- data.table::data.table(read.df)

  if (! all(names(read.dt) == c("varname",
                                "formula", "variance", "dist", "link"))) {

    stop("Field names do not match")

  }

  # check validity of data set

  suppressWarnings(test <- as.numeric(unlist(strsplit(as.character(read.dt[1, formula]),
                                                      split=";",
                                                      fixed = TRUE)))
  )

  if (sum(is.na(test))) {
    stop("First defined formula must be scalar", call. = FALSE)
  }

  if (nrow(read.dt) > 1){
    for (i in 2:nrow(read.dt)) {
      evalDef(read.dt[i,varname], read.dt[i,formula], read.dt[i,dist], read.dt[1:(i-1), varname])
    }
  }


  attr(read.dt,"id") <- id
  return(read.dt)

}
