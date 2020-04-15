#newvar, newform, newdist, defVars
test_that("Valid definitions produce no error.", {
  gen_args <-
    gen.and_then(gen.int(8), function(n) {
      generate(for (x in list(
    
        vars = gen_varnames(n),
        dist = gen_dist,
        i = gen.int(n)
      ))
         list(
          newvar = x$vars[x$i],
          newform = get(reg[name == x$dist]$formula)(x$vars[seq_len(x$i - 1)]),
          newdist = x$dist,
          defVars = x$vars[seq_len(x$i - 1)]
          )
        )
    }) 
  
  forall(gen_args, function(x) expect_silent(do.call(".evalDef",x)))
  
})

