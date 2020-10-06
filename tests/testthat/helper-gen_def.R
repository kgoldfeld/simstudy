library(hedgehog)
# Generators:
#   This file implements helper functions and generators used in property based
#   testing using the hedgehog package. Generators do not just return one value
#   but a whole data structure, see ?hedgehog for details. If you want to check a
#   generators results use gen.example(generator_to_test()). Generators that
#   just wrap one of the basic generators like gen.int(5) will only be commented if
#   usecase or intent is not clear from the generator naming.

# General Generators ----
distributions <- .getDists()
gen_dist <- gen.no.shrink(gen.element(distributions))

# beta dist variance
gen_precision <- function(...) {
  gen.int(50)
}
gen_prob <- function(x) gen.unif(0, 1)
# default variance
gen_var <- function(x) 0
# default identity
gen_id <- gen.element(c("identity"))

# Name Generators ----

gen_abc <- gen.element(letters)
gen_ABC <- gen.element(LETTERS)
gen_123 <- gen.element(0:9)
gen_azAZ09 <- gen.choice(gen_abc, gen_ABC, gen_123)
gen_azAZ09_ <- gen.choice(gen_azAZ09, "_", prob = c(0.98, 0.02))

#' Generate Name
#'
#' @param l length of the name
#' @return Name as character vector
#' @noRd
gen_name <- function(l) {
  gen.and_then(gen.element(l), function(x) {
    gen.c(of = x, gen_azAZ09_)
  })
}

#' Generate Var Name
#'
#' @return Valid R var name as string.
#' @noRd
gen_varname <- gen.map(function(x) {
  make.names(paste0(x, collapse = ""))
}, gen_name(3:12))

#' Generate Function Name
#'
#' @return Valid R function name as string.
#' @noRd
gen_fun_name <- gen.map(function(x) {
  make.names(paste0(x, collapse = ""))
}, gen_name(2:6))

# TODO does it make sense to not shrink varnames?

#' Generate Variable Names
#'
#' @param n Number of names to generate.
#' @return Vector of names.
#' @noRd
gen_varnames <- function(n) {
  gen.no.shrink(gen.map(
    function(names) {
      make.names(names, unique = TRUE)
    }, gen.c(of = n, gen_varname)
  ))
}

# Function and Variable Generators ----
#   These generators are used to create and assign numeric variables for use in
#   definition formulas.

#' Assign ..var
#'
#' @description Generates a variable name and assigns val to it.
#' @param val Value to assign.
#' @return The name of the assigned variable as ..var.
#' @noRd
gen_assign_dotdot <- function(val) {
  gen.map(function(name) {
    assign(name, val, pos = .GlobalEnv)
    paste0("..", name)
  }, gen_varname)
}

#' Generate numeric ..var
#'
#' @return The name of the assigned variable as ..var.
#' @noRd
gen_dotdot_num <- gen.and_then(gen.unif(from = -1000, to = 1000), gen_assign_dotdot)

#' Generate numeric ..var vector
#'
#' @param n Size of vector.
#' @return The name of the assigned vector with a random element selected
#'  e.g. ..var[2].
#' @noRd
gen_dotdot_vec <- function(n) {
  gen.and_then(
    gen.c(of = n, gen.unif(from = -1000, to = 1000)),
    function(val) {
      gen.map(function(name) {
        paste0(name, "[", sample(1:n, 1), "]")
      }, gen_assign_dotdot(val))
    }
  )
}

#' Sized version of gen_dotdot_vec
#'
#' @return The name of the assigned vector with a random element selected
#'  e.g. ..var[2].
#' @seealso gen.sized
#' @noRd
gen_dotdot_vec_ele <- gen.sized(gen_dotdot_vec)

#' Generate ..Var
#'
#' @description Generate and assign value to numeric var or vector
#' @return Name of generated var prefixed with ..
#' @noRd
gen_dotdot_var <- gen.choice(gen_dotdot_num, gen_dotdot_vec_ele)

#' Generate base unction
#'
#' @return Base function as string with placeholder for use in gen_wrap_fun.
#' @noRd
gen_base_func <- gen.sample(c("log(abs(%s))", "exp(%s)", "sin(%s)", "cos(%s)", "log10(abs(%s))", "log2(abs(%s))"), 1)

#' Generate arbitrary arithmetic function
#'
#' @return Function name as string with placeholder: func(%s).
#'  For use in gen_wrap_fun.
#' @noRd
gen_arb_fun <-
  gen.and_then(gen_fun_name, function(name) {
    gen.map(function(formula) {
      formula <- gsub("..", "", formula, fixed = T)
      assign(name, eval(parse(text = paste(
        "function(p)", formula
      ))), pos = .GlobalEnv)
      paste0(name, "(%s)")
    }, gen_formula("p"))
  })

#' Generate function with parameters
#'
#' @param inner Parameters for the function.
#' @return Function call as string, e.g. "log(..var1)"
#' @noRd
gen_wrap_fun <-
  function(inner) {
    gen.map(
      function(x) {
        sprintf(x, inner)
      },
      gen.choice(gen_base_func, gen_arb_fun, prob = c(.8, .2))
    )
  }

# Formula Generators ----
#
#   Generators to create an arithmetic expression with previously defined
#   variables as formula argument for many distributions. The creation of the
#   formula follows a modified EBNF grammar analog to normal expressions:
#
#   <expr> ::= <term> ("+" | "-") <expr> |  <term>
#   <term> ::= <factor> ("*" | "/" |"^" | "%%" | "%/%") <term> | <factor>
#   <factor> ::= "(" <expr> ")" | <function> | <const>
#   <function> ::= base_func"(" <expr> ")" | arithmetic_func"(" <expr> ")"
#   <const> ::= integer | ..var | prev_var

# ::= prev_var
gen_prev_var <- function(prev_vars) {
  gen.element(prev_vars)
}

# ::= integer
gen_const <- gen.element(0:1000)
# needed as function for mget
gen_constf <- function(x) {
  gen.element(0:1000)
}

# <factor> ::= "(" <expr> ")" | <function> | <const>
gen_factor <- function(prev_var) {
  gen.choice(
    gen_expr_br(prev_var),
    gen_expr_fun(prev_var),
    gen_prev_var(prev_var),
    gen_dotdot_var,
    gen_const
  )
}

# <factor> ("*" | "/" |"^" | "%%" | "%/%") <term>
gen_factor_dt <- function(prev_var) {
  generate(for (x in list(
    op = gen.element(c(" * ", " / ", " ^ ", " %% ", " %/% ")),
    fac1 = gen_factor(prev_var),
    fac2 = gen_factor(prev_var)
  )) {
    list(x$fac1, x$op, x$fac2)
  })
}

# <term> ::= <factor> ("*" | "/" |"^" | "%%" | "%/%") <term> | <factor>
gen_term <- function(prev_var) {
  gen.choice(gen_factor(prev_var), gen_factor_dt(prev_var))
}

# ::= <term> ("+" | "-") <expr>
gen_term_pm <-
  function(prev_var) {
    generate(for (x in list(
      op = gen.element(c(" + ", " - ")),
      term1 = gen_factor(prev_var),
      term2 = gen_factor(prev_var)
    )) {
      list(x$term1, x$op, x$term2)
    })
  }

# ::= "(" <expr> ")"
gen_expr_br <- function(prev_var) {
  gen.map(function(x) c("(", x, ")"), gen_expr(prev_var))
}

# <function> ::= base_func"(" <expr> ")" | arithmetic_func"(" <expr> ")"
gen_expr_fun <- function(prev_var) {
  gen.and_then(gen_formula(prev_var), function(inner) {
    gen_wrap_fun(inner)
  })
}

# <expr> ::= <term> ("+" | "-") <expr> |  <term>
gen_expr <- function(prev_var) {
  gen.choice(gen_term_pm(prev_var), gen_term(prev_var))
}

#' Generate Formula
#'
#' @description Generates a valid arithmetic formula utilizing constants, ..vars
#' and prev_var.
#' @param prev_var Character vector of previously defined variables. If missing
#' replaced with numerics.
#' @return The generated formula as string and the assigned vars in the
#' .GlobalEnv .
#' @noRd
gen_formula <-
  function(prev_var) {
    if (missing(prev_var) ||
      length(prev_var) == 0 || !all(nchar(prev_var) > 0)) {
      prev_var <- c(-100:100)
    }

    gen.map(function(x) {
      paste(unlist(x), collapse = "")
    }, gen_expr(prev_var))
  }

gen_formula_scalar <- gen_formula(-100:100)

# Mixture Generators ----

#' Generate Normalized Probabilities
#'
#' @param n Number of probabilities.
#' @return Numeric vector of normalized probabilities.
#' @noRd
gen_n_norm_Probs <- function(n) {
  gen.map(function(p) {
    p / sum(p)
  }, gen.c(of = n, gen_prob()))
}

#' Generate Mixture Formula
#'
#' @param prev_var Character vector of previously defined variables. If missing
#' a scalar mixture will be generated.
#' @return Mixture formula as string, e.g. "5 | p1 + x2 | p2".
#' @noRd
gen_mixture <- function(prev_var) {
  if (missing(prev_var) || length(prev_var) == 0 || !all(nchar(prev_var) > 0)) {
    gen_mix_scalar
  } else {
    gen.and_then(gen.element(1:(length(prev_var) + 2)), function(n) {
      gen_mix_parts(prev_var = prev_var, n = n)
    })
  }
}

#' Scalar Mixture Formula Generator
#'
#' @return Scalar mixture formula as string, e.g. "23 | p1 + 42 | p2".
#' @noRd
gen_mix_scalar <- gen.sized(function(n) {
  gen.and_then(gen.c(gen.element(-1000:1000), of = n), function(p) {
    gen_mix_parts(p, n)
  })
})

#' Generate Mixture Parts
#'
#' @description description
#' @param prev_var Character vector of previously defined variables.
#' @param n Number of parts.
#' @return Balanced mixture formula as string, e.g. "x1 | p1 + x2 | p2".
#' @noRd
gen_mix_parts <- function(prev_var, n) {
  generate(for (x in
    list(
      probs = gen_n_norm_Probs(n),
      vars = gen.list(
        of = n, gen.choice(gen.element(prev_var), gen.element(-1000:1000), prob = c(.7, .3))
      )
  )) {
    paste(x$vars, x$probs, sep = " | ", collapse = " + ")
  })
}

# Categorical Generators ----

#' Generate Categorical Probabilities
#'
#' @return Numeric vector of normalized probabilities.
#' @noRd
gen_cat_probs <- gen.and_then(gen.element(2:10), function(n) {
  gen_n_norm_Probs(n)
})

#' Generate Categorical Formula
#'
#' @description This will generate a categorical formula by passing either a
#' number of probabilities or a single integer to genCatFormula..
#' @return Formula as string, eg. ".5;.5".
#' @noRd
gen_cat_formula <- function(...) {
  gen.map(
    function(p) {
      do.call(genCatFormula, as.list(p))
    },
    gen.choice(gen_cat_probs, gen.map(
      function(x) {
        list(n = x)
      }, gen.element(2:15)
    ))
  )
}

# Uniform Generators ----

#' Generate UniformInt Formula
#'
#' @return Formula as string, e.g. "1;10"
#' @noRd
gen_uniformInt_range <- function(...) {
  gen.map(function(x) {
    paste0(sort(unlist(floor(x))) - c(1, 0), collapse = ";")
  }, gen.c(of = 2, gen.unif(-100, 100)))
}

#' Generate Uniform Formula
#'
#' @return Formula as string, e.g. "1.2;10.3"
#' @noRd
gen_uniform_range <- function(...) {
  gen.map(function(x) {
    paste0(sort(unlist(x)), collapse = ";")
  }, gen.c(of = 2, gen.unif(-100, 100)))
}

# Link Generators ----
gen_link_log <- gen.no.shrink(gen.element(c("identity", "log")))
gen_link_logit <- gen.no.shrink(gen.element(c("identity", "logit")))

# Lookup Table ----
#   This data.table is used in generating complete data definitions. New
#   Distributios need to be added here to to be included in testing.
reg <- data.table()
reg$name <- sort(.getDists())
reg$formula <- character()
reg$variance <- "gen_var"
reg$link <- "gen_id"
reg[!(name %in% c(
  "binary",
  "binomial",
  "beta",
  "categorical",
  "mixture",
  "uniform",
  "uniformInt"
)), ]$formula <- rep("gen_formula", 7)
reg[name %in% c("binary", "binomial", "beta")]$formula <- rep("gen_prob", 3)
reg[name == "binomial"]$variance <- "gen_constf"
reg[name == "categorical"]$formula <- "gen_cat_formula"
reg[name == "mixture"]$formula <- "gen_mixture"
reg[name == "uniform"]$formula <- "gen_uniform_range"
reg[name == "uniformInt"]$formula <- "gen_uniformInt_range"
reg[name %in% c("normal", "negBinomial", "gamma")]$variance <- rep("gen_formula", 3)
reg[name == "beta"]$variance <- "gen_precision"
reg[name %in% c("beta", "binary", "binomial")]$link <- rep("gen_link_logit", 3)
reg[name %in% c("exponential", "gamma", "negBinomial", "noZeroPoisson", "poisson")]$link <- rep("gen_link_log", 5)

# Generators for complete Data Definitions ----
# WIP
gen_dists <-
  function(n) {
    gen.map(
      function(dists) {
        unlist(dists)
      },
      gen.and_then(gen_dist_fst, function(dist) {
        list(dist, gen.list(of = n - 1, gen_dist))
      })
    )
  }

gen_def_dt <-
  function(n) {
    gen.map(function(list) {
      dt <- as.data.table(list)
      names(dt) <- c("varname", "dist")
      dt$formula <- character()
      dt$variance <- character()
      dt$link <- character()
      dt
    }, gen.and_then(gen_varnames(n), function(vars) {
      list(vars, gen_dists(n))
    }))
  }

gen_def_dt_n <- gen.sized(gen_def_dt)

gen_defs <- function(dt, n, i = 1) {
  if (i > n) {
    return(dt)
  }

  generate(for  (x in list(
    formula = get(reg[name == dt[i, dist]]$formula)(dt[seq_len(i - 1), varname]),
    variance = get(reg[name == dt[i, dist]]$variance)(dt[seq_len(i - 1), varname]),
    link = get(reg[name == dt[i, dist]]$link)
  )) {
    dt$formula[i] <- x$formula
    dt$variance[i] <- x$variance
    dt$link[i] <- x$link
    gen_defs(dt, n, i + 1)
  })
}

gen_def <- gen.and_then(gen.int(20), function(n) {
  gen.and_then(gen_def_dt(n), function(dt) {
    gen_defs(dt, n, 1)
  })
})
