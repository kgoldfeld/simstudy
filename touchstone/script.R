# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

touchstone::refs_install() # installs branches to benchmark

# benchmark a function call from your package (two calls per branch)
touchstone::benchmark_run_ref(
  expr_before_benchmark = "
  library(simstudy)
  gen.school <- defData(varname = \"s0\", dist = \"normal\", formula = 0, variance = 3,
    id = \"idSchool\")
gen.school <- defData(gen.school, varname = \"nClasses\", dist = \"noZeroPoisson\", formula = 3)

set.seed(282721)
  ",
  clustered = "dtSchool <- genData(1000, gen.school)
               dtSchool <- trtAssign(dtSchool, n = 2)", #
  n = 5
)
touchstone::benchmark_run_ref(
  expr_before_benchmark = {
    library(simstudy)
    gen.school <- defData(
      varname = "s0", dist = "normal", formula = 0, variance = 3,
      id = "idSchool"
    )
    gen.school <- defData(gen.school, varname = "nClasses", dist = "noZeroPoisson", formula = 3)

    set.seed(282721)
  },
  clustered_expr = {
    dtSchool <- genData(1000, gen.school)
    dtSchool <- trtAssign(dtSchool, n = 2)
  }, #
  n = 5
)
# # benchmark any R expression (six calls per branch)
# touchstone::benchmark_run_ref(
#   more = "if (TRUE) {
#     y <- yourpkg::f2( x= 3)
#   }", #<- TODO put the call you want to benchmark here
#   n = 6
# )


# create artifacts used downstream in the GitHub Action
touchstone::benchmarks_analyze()