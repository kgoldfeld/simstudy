library(data.table)
fun.names <- ls(getNamespace("simstudy"), all.names=TRUE)

getRfile <- function(fn) {
  
  fun <- tryCatch({

    body(getAnywhere(x = fn)$objs[[1]])

  } , error = function(e) {
    NULL
  }
  )
  
  if (is.null(fun)) f <- "NA"
  else f <- attr(fun, "srcfile")$filename

  data.table(func = fn, file = f)
  
}


files <- lapply(fun.names, function(x) getRfile(x))
files <- rbindlist(files)

fwrite(files, "File_management/File_structure.csv")
       