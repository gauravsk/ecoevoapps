# library(ecoevoapps)

test_that("Discrete exponential model works with proper parameters", {
  # Growing population (lambda > 1)
  discrete_exp <- run_discrete_exponential_model(time = 100, lambda = 1.5)
  expect_gt(discrete_exp[100,2], 1000)

  # Shrinking population (lambda < 1)
  discrete_shrink <- run_discrete_exponential_model(time = 100, lambda = 0.5)
  expect_lt(discrete_shrink[100,2], 1)

})

test_that("Ricker model goes to defined carrying capacity", {
 ricker_out <- run_ricker_model(params = c(rd = 1, K = 200), time = 100)
 expect_equal(ricker_out[100,2], 200)

 # shrinking population - negative rd
 ricker_out2 <- run_ricker_model(params = c(rd = -.2, K = 200), time = 100)
 expect_equal(ricker_out2[100,2], 0)
})

test_that("BH model goes to defined carrying capacity", {
  bh_out <- run_beverton_holt_model(params = c(rd = 1.5, K = 200), time = 100)
  expect_equal(bh_out[100,2], 200)

  # shrinking population - rd less than 1
  bh_out2 <- run_beverton_holt_model(params = c(rd = .5, K = 200), time = 100)
  expect_equal(bh_out2[100,2], 0)
})

test_that("Discrete logistic model goes to below defined carrying capacity", {
  dl_out <- run_discrete_logistic_model(params = c(rd = 1.5, K = 200), time = 100)
  expect_lt(dl_out[100,2], 200)

  # shrinking population - rd less than 1
  dl_out2 <- run_discrete_logistic_model(params = c(rd = 0.5, K = 200), time = 100)
  expect_equal(dl_out2[100,2], 0)
})

test_that("Discrete population growth plots generate", {
  # Ricker model
  params_vec <- c(rd = 1.2, K = 200)
  ricker_out <- run_ricker_model(params = params_vec, time = 100)
  ricker_plot <- plot_discrete_population_growth(ricker_out)
  ricker_cobw <- plot_discrete_population_cobweb(ricker_out, param_vec, "ricker")

  expect_equal(ricker_cobw$labels$slope, "slope")
  expect_equal(ricker_plot$labels$x, "time")

  # Beverton-Holt
  bh_out <- run_beverton_holt_model(params = params_vec, time = 100)
  bh_plot <- plot_discrete_population_growth(bh_out)
  bh_cobw <- plot_discrete_population_cobweb(bh_out, param_vec, "ricker")

  expect_equal(bh_cobw$labels$slope, "slope")
  expect_equal(bh_plot$labels$x, "time")

  # Discrete logistic
  dl_out <- run_discrete_logistic_model(params = params_vec, time = 100)
  dl_plot <- plot_discrete_population_growth(dl_out)
  dl_cobw <- plot_discrete_population_cobweb(dl_out, param_vec, "ricker")

  expect_equal(dl_cobw$labels$slope, "slope")
  expect_equal(dl_plot$labels$x, "time")

})
