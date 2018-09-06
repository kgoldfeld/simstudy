#### .iserror ####

# Internal function
#
# @param tryObject the result of a try() call.
# @return TRUE or FALSE
#

.iserror <- function(tryobject) {
  if (class(tryobject)[1] == "try-error") {
    return (TRUE)
  } else {
    return(FALSE)
  }
}
