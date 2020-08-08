
addVar <- command(
  "Add Variable",
  generator = function(state) {
    gen.and_then(gen_dist, function(dist)
      list(
        def = state$def,
        varname = gen.int(length(state$namelist)),
        dist = dist,
        formula = get(reg[name == dist,]$formula)(state$varnames),
        variance = get(reg[name == dist,]$variance)(state$varnames),
        link = get(reg[name == dist,]$link),
        namelist = state$namelist)
      )
    
  },
  #preconditions
  # require = function(state, def , varname, ...) {
  #   if (is.null(state$def))
  #     TRUE
  #   else
  #     ! varname %in% state$varnames
  # },
  
  execute = function(def, varname, formula, variance, dist, link,namelist) {
    def <- defData(def, namelist[varname], formula, variance, dist, link)
  },
  #postconditions
  ensure = function(state, output, ...) {
    if (is.null(state$def))
      nVars <- 0
    else
      nVars <- length(state$varnames)
    expect_s3_class(output, "data.table")
    expect_equal(nrow(output), nVars)
  },
  update = function(state, output, def, varname, ...) {
    state$def <- output
    state$varnames <- c(state$varnames, state$namelist[varname])
    state$namelist <-  state$namelist[-varname]
    state
  }
)

genDat <- command("Generate Data",
                  generator = function(state){
                    if( length(state$varnames) == 0 ) return(NULL) #|| state$dataDone
                    list(n = gen.int(50),
                         def = state$def)     },
                  require = function(state,...){
                    if(length(state$varnames) == 0) F  #|| state$dataDone
                    else T}, #preconditions
                  execute = function(state,n,def) genData(n,def),
                  ensure = function(state,output,n,def){expect_equal(nrow(output),n)}, #postconditions
                  update = function(state, output, ...) {state$data <- output
                  state$dataDone <- TRUE
                  state}
                  )

freeze_def <- names(.GlobalEnv)
unique_names <- unique(gen.run(gen_varnames(50))$root)
initialmodel <- list(def = NULL, varnames = character(0) , data= NULL, dataDone = FALSE , namelist = unique_names)
test_that("something", {
  #skip("not finished yet")
  skip_on_cran()
  forall(gen.actions(initialmodel, list(addVar,genDat)), function (actions) {
    initialmodel <-
      list(
        def = NULL,
        varnames = character(0) ,
        data = NULL ,
        dataDone = FALSE ,
        namelist = unique_names
      )
    assign("lastActions",actions,pos = .GlobalEnv)
    expect_sequential(initialmodel, actions)
  }, tests  = 100, shrink.limit = 0)
})


rm(list = setdiff(names(.GlobalEnv),freeze_def),pos = .GlobalEnv)
