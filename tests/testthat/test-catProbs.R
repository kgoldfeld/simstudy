test_that("probabilities stay the same", {
  forall(gen_cat_probs,function(ps){ 
    expect_equal(sum(ps),1)
    expect_equal(ps,as.numeric(.splitFormula(catProbs(ps))))
    })
})
