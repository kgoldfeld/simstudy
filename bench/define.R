def_all_dists <- function() {
    def <- defData(varname = "age", dist = "uniformInt", formula = "22;75")
    def <- defData(def, varname = "rating", dist = "uniform", formula = "0;10")
    def <- defData(def,
        varname = "female", dist = "binary",
        formula = "-2 + age * 0.1", link = "logit"
    )
    def <- defData(def,
        varname = "baseDBP", dist = "normal",
        formula = 70, variance = 40
    )
    def <- defData(def,
        varname = "nClasses", dist = "noZeroPoisson", formula = 3
    )
    def <- defData(def,
        varname = "visits", dist = "poisson",
        formula = "1.5 - 0.2 * age + 0.5 * female", link = "log"
    )
    def <- defData(def,
        varname = "Y0", dist = "normal", formula = 10, variance = 1
    )
    def <- defData(def,
        varname = "Y1", dist = "normal", formula = "Y0 + 5 + 5",
        variance = 1
    )
    def <- defData(def,
        varname = "deterministic", dist = "nonrandom", formula = "25 + age"
    )
    def <- defData(def,
        varname = "binom", dist = "binomial", formula = .4,
        variance = 10
    )
    def <- defData(def,
        varname = "cat", dist = "categorical", formula = genCatFormula(n = 5),
        variance = "a;b;c;d;e"
    )
    def <- defData(def,
        varname = "exp", dist = "exponential", formula = "42"
    )

    def <- defData(def,
        varname = "gamma", dist = "gamma", formula = "exp/age",
        variance = 1
    )

    def <- defData(def,
        varname = "mix", dist = "mixture",
        formula = genMixFormula(
            c("age", "exp", "Y1", "baseDBP"),
            c(0.3, .2, .4, .1)
        )
    )

    def <- defData(def,
        varname = "negBin", dist = "negBinomial", formula = "Y0 + 10 + 5",
        variance = 1
    )
    def <- defData(def,
        varname = "beta", dist = "beta", formula = .6,
        variance = .5
    )

    def
}

def_short <- function() {
    def <- defData(varname = "age", dist = "uniformInt", formula = "22;75")
    def <- defData(def, varname = "rating", dist = "uniform", formula = "0;10")
    def <- defData(def,
        varname = "female", dist = "binary",
        formula = "-2 + age * 0.1", link = "logit"
    )
    def <- defData(def,
        varname = "baseDBP", dist = "normal",
        formula = 70, variance = 40
    )
    def <- defData(def,
        varname = "deterministic", dist = "nonrandom", formula = "25 + age"
    )
    def <- defData(def,
        varname = "cat", dist = "categorical", formula = genCatFormula(n = 5),
        variance = "a;b;c;d;e"
    )

    
    def
}
