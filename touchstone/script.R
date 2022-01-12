# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
touchstone::pin_assets("bench")

# installs branches to benchmark
touchstone::branch_install()


seed <- 282721
n <- 5000
reps <- 5

# setup <- rlang::expr({
#   library(simstudy)
#   set.seed(!!seed)
# }) test

source(touchstone::path_pinned_asset("bench/define.R"))

# touchstone runs its benchmarks in callr subprozess so we have to do the setup
# within each benchmark
touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def_all_dists <- !!def_all_dists
  },
  define_data = def_all_dists(),
  n = reps
)



touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def_all_dists <- !!def_all_dists
    def <- def_all_dists()
  },
  gen_all_dists = genData(!!n, def),
  n = reps
)

n <- 100000

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", formula = 0.5, variance = 2, dist = "beta")
  },
  dist_beta = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", formula = 0.3, dist = "binary")
  },
  dist_binary = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", formula = 0.3, variance = 42, dist = "binomial")
  },
  dist_binomial = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", formula = genCatFormula(0.2, 0.3, 0.1, 0.4), variance = "1;2;3;4", dist = "categorical")
  },
  dist_categorical = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", dist = "exponential", formula = 42)
  },
  dist_exponential = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", dist = "gamma", formula = 42,
        variance = 1)
  },
  dist_gamma = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x1", dist = "exponential", formula = 42)
    def <- defData(def, varname = "x2", formula = 0.5, variance = 2, dist = "beta")
    def <- defData(def,varname = "x", dist = "mixture", formula = genMixFormula(c("x1","x2"), c(.65, .35)))
  },
  dist_mixture = genData(!!n, def),
  n = reps
)

touchstone::benchmark_run(
  expr_before_benchmark = {
    library(simstudy)
    set.seed(!!seed)
    def <- defData(varname = "x", formula = 2, variance = 1.5, dist = "normal")
  },
  dist_normal = genData(!!n, def),
  n = reps
)


# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
