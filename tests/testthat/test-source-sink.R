# library(ecoevoapps)

test_that("Run source sink works", {
  ss_params <- c(pa = 0.6, pj = 0.15, betaSource = 3, betaSink = 1, NSource = 300)
  ss_out <- run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
  params = ss_params)

  # Both populations should exist at an equilibrium greater than zero
  expect_gt(ss_out[[2]][50], 0)
  expect_gt(ss_out[[3]][50], 0)

  # Running the simulation with bad params should result in a warning
  invalid_parms <- c(pa = 0.6, pj = 0.15, betaSource = .15, betaSink = 1, NSource = 300)
  expect_warning(run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
                                    params = invalid_parms))


  })

test_that("Source sink assumption checker works", {
  valid_parms <- c(pa = 0.6, pj = 0.15, betaSource = 3, betaSink = 1, NSource = 300)
  expect_true(assumption_check(valid_parms))

  # Invalid because there is negative growth rate in "source"
  invalid_parms1 <- c(pa = 0.6, pj = 0.15, betaSource = .5, betaSink = 1, NSource = 300)
  expect_false(assumption_check(invalid_parms1))


  # Invalid because there is positive growth rate in "sink"
  invalid_parms2 <- c(pa = 0.6, pj = 3, betaSource = 4, betaSink = 1, NSource = 300)
  expect_false(assumption_check(invalid_parms2))
})

test_that("Source sink plot works as expected", {

  valid_parms <- c(pa = 0.6, pj = 0.15, betaSource = 3, betaSink = 1, NSource = 300)
  ss_out <- run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
                            params = valid_parms)
  ss_plot <- plot_source_sink(ss_out, assumption_status = assumption_check(valid_parms))

  # There should only be 1 layer (no warning layer)
  expect_length(ss_plot$layers, 1)

  invalid_parms <- c(pa = 0.6, pj = 0.15, betaSource = .15, betaSink = 1, NSource = 300)
  ss_out_invalid <- suppressWarnings(
    run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
                            params = invalid_parms))
  ss_plot_invalid <- plot_source_sink(ss_out_invalid,
                                      assumption_status = assumption_check(invalid_parms))

  # There should now be 2 layers (an extra warning layer)
  expect_length(ss_plot_invalid$layers, 2)
})
