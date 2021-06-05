library(bench)
set.seed(42)

bench::mark(
    {def <- defData(varname = "x", formula = 2)
     def <- defData(def, "y", formula = "2*x*.3")
    }
)
def <- defData(varname = "x", formula = 2)
     def <- defData(def, "y", formula = "2*x*.3")

res <- bench::press(
    n = c(50,500,5000,50000, 500000, 5000000),
    bench::mark(
        genData(n,def),
        relative = TRUE
    )
)
