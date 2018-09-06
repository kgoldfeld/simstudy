#### Add strata code - internal function ####

# Assign treatment
#
# @param dt data table
# @param strata vector of string names representing strata
# @return An integer (group) ranging from 1 to length of the probability vector

.addStrataCode <- function(dt, strata) {

  # 'Declare' var

  stratum = NULL

  #

  dtWork <- copy(dt)

  strataOnly <- dtWork[, eval(strata) , with=FALSE]
  data.table::setkeyv(strataOnly, names(strataOnly))

  uniqueStrata <- unique(strataOnly)
  uniqueStrata[, stratum := (1 : .N)]

  data.table::setkeyv(dtWork, names(strataOnly))
  dtWork <- uniqueStrata[dtWork]

  data.table::setkeyv(dtWork, key(dt))

  dtWork
}
