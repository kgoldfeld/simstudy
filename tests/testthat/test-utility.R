# catProbs ----
roundTrip <- function(args) as.numeric(.splitFormula(do.call(catProbs, as.list(args))))

test_that("probabilities stay the same after catProbs", {
  skip_on_cran()
  forall(gen_cat_probs, function(ps) {
    rt <- roundTrip(ps)
    expect_equal(sum(ps), 1)
    expect_equal(length(ps), length(rt))
    expect_equal(ps, rt)
  })
})

test_that("cat probs are generated correctly", {
  skip_on_cran()
  forall(gen.element(2:15), function(n) {
    rt <- roundTrip(list(n = n))
    expect_equal(length(rt), n)
    expect_equal(sum(rt), 1)
  })

  forall(gen.unif(0, 1), function(p) {
    expect_equal(sum(roundTrip(p)), sum(p, 1 - p))
    expect_length(roundTrip(p), 2)
  })
})

test_that("probabilities are adjusted as documented.", {
  skip_on_cran()
  forall(gen.and_then(gen.element(2:15), function(n)
    gen_n_norm_Probs(n)), function(p) {
    over <- p / .9
    under <- p / 1.1
    expect_warning(catProbs(over), "will be normalized")
    expect_warning(catProbs(under), "Adding category")
    expect_equal(sum(roundTrip(over)), 1)
    expect_equal(sum(roundTrip(under)), 1)
    expect_length(roundTrip(over), length(over))
    expect_length(roundTrip(under), length(under) + 1)
  })
})

test_that("catProbs throws errors.", {
  expect_error(catProbs(), "Need to specify")
  expect_error(catProbs(1, 2, 3, n = 5), "or n, not both")
  expect_error(catProbs(1.1), "must be less than 1")
  expect_error(catProbs(n = 1.1), "must be a whole number")
  expect_error(catProbs(n = -3), "Negative values")
})
