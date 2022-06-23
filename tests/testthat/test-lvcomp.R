library(ecoevoapps)
test_that("LV comp breaks with wrong parameters", {
  expect_error(run_lvcomp_model(params = c(r1 = .1, r2 = .1, K1 = 50, a = .5, b = .5))) # no K2
  expect_error(run_lvcomp_model(params = c(r1 = .1,  K1 = 50, K2 = 60, a = .5, b = .6))) # no K2
  expect_error(run_lvcomp_model(params = c(r1 = .1, r2 = .2, a11 = .05, a12 = .04, a22 = .05))) # no K2
})

test_that("LV comp returns coexistence when expected, and exclusion when not", {
  # result should be coexistence
  lvout_coex <- run_lvcomp_model(params = c(r1 = .1, r2 = .1, K2 = 60,
                                       K1 = 50, a = .5, b = .5))
  lvout_coex2 <- run_lvcomp_model(params = c(r1 = .1, r2 = .1,
                                            a11 = 1/50, a12 = 1/90,
                                            a22 = 1/60, a21 = 1/100))

  # Species 1 should run to exclusion
  lvout_excl <- run_lvcomp_model(params = c(r1 = .1, r2 = .1, K2 = 60,
                                            K1 = 5, a = .8, b = .5))
  lvout_excl2 <- run_lvcomp_model(params = c(r1 = .1, r2 = .1,
                                             a11 = 1/5, a12 = 1/10,
                                             a22 = 1/60, a21 = 1/100))

  # both species have final abundances > 1
  expect_gt(lvout_coex[100,2], 1)
  expect_gt(lvout_coex[100,3], 1)
  expect_gt(lvout_coex2[100,2], 1)
  expect_gt(lvout_coex2[100,3], 1)


  # only one species has final abundance > 1
  expect_equal(unname(lvout_excl[100,2]), expected = 0)
  expect_gt(lvout_excl[100,3], expected = 1)
  expect_equal(unname(lvout_excl2[100,2]), expected = 0)
  expect_gt(lvout_excl2[100,3], expected = 1)

})

test_that("LV comp plots are generated as expected", {
  # result should be coexistence
  param_vec <- c(r1 = .1, r2 = .1, K2 = 60,
                          K1 = 50, a = .5, b = .5)
  lvout_coex <- run_lvcomp_model(params = param_vec)

  lvcomp_time <- plot_lvcomp_time(lvout_coex)
  lvcomp_port <- plot_lvcomp_portrait(lvout_coex, params = param_vec)

  # Just a way to check that the plotting worked as expected
  expect_equal(lvcomp_time$labels$colour, "Species")
  expect_equal(lvcomp_port$labels$colour, "ZNGI for")

})

