rm(list = setdiff(names(.GlobalEnv),freeze),pos = .GlobalEnv)
detach("package:hedgehog", unload=TRUE)