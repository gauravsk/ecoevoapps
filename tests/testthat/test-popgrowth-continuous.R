test_that("run_ functions work as expected, and return errors with wrong parameters", {
  # exponential growth - check that population is big by the end
  exp_growth <-  run_exponential_model(time = 0:10, init = c(N1 = 1),
                                   params = c(r = 1))
  expect_gt(exp_growth[10,2], 100)

  # logistic growth with K = 100- check that final pop size < 100
  log_growth <- run_logistic_model(time = 0:100, init = c(N1 = 1),
                                   params = c(r = .5, K = 100))
  expect_lte(log_growth[100, 2], 100)

  # check that errors arise as necessary
  expect_error(run_exponential_model(time = 0:10, init = c(N1 = 1), params = c(b = 1))) # b not r
  expect_error(run_logistic_model(time = 0:10, init = c(N1 = 1), params = c(r = 1))) # no K

})




test_that("LV comp plots are generated as expected", {
  # result should be coexistence
  exp_growth <-  run_exponential_model(time = 0:10, init = c(N1 = 1),
                                       params = c(r = 1))
  exp_plot <- plot_continuous_population_growth(exp_growth)


  log_growth <-  run_logistic_model(time = 0:10, init = c(N1 = 1),
                                       params = c(r = 1, K = 5))
  log_plot <- plot_continuous_population_growth(log_growth)

  # Just a way to check that the plotting worked as expected
  expect_equal(exp_plot$labels$x, "time")
  expect_equal(log_plot$labels$x, "time")

})


