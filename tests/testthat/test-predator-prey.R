# library(ecoevoapps)
test_that("Check that predator-prey dynamics function evaluates as expected", {
  params_lv <- c(r = .1, a = .01, e = .01, d = .001)
  lvpp_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_lv)
  last_row <- lvpp_out[nrow(lvpp_out), c("H", "P")]
  # Check that both are greater than zero
  expect_true(all(last_row > 0))

  # LVPP with prey carrying capacity
  params_lvK <- c(r = .1, a = .01, e = .01, d = .001, K = 100)
  lvppK_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_lvK)
  last_row2 <- lvppK_out[nrow(lvppK_out), c("H", "P")]
  # Check that both are greater than zero
  expect_true(all(last_row2 > 0))

  # LVPP with Type 2 carrying capacity
  params_lvT2 <- c(r = .1, a = .01, e = .01, d = .001, T_h = 0.1)
  lvppT2_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_lvT2)
  last_row3 <- lvppT2_out[nrow(lvppT2_out), c("H", "P")]
  # Check that both are greater than zero
  expect_true(all(last_row3 > 0))

  # Macarthur Rosenszweig model
  params_RM <- c(r = .1, a = .01, e = .01, d = .001, T_h = 0.1, K = 100)
  lvppRM_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_RM)
  last_row4 <- lvppRM_out[nrow(lvppRM_out), c("H", "P")]
  # Check that both are greater than zero
  expect_true(all(last_row4 > 0))

  })


test_that("Predator Prey plots are generated as expected", {
  params_lv <- c(r = .1, a = .01, e = .01, d = .001)
  lvpp_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_lv)
  lvpp_port <- plot_predprey_portrait(lvpp_out, params = params_lv)
  lvpp_traj <- plot_predprey_time(lvpp_out)
  # Check that the plot is made as expected
  expect_equal(lvpp_port$labels$y, "Number of Predators")
  expect_equal(lvpp_traj$labels$colour, "Population")

  # LVPP with prey carrying capacity
  params_lvK <- c(r = .1, a = .01, e = .01, d = .001, K = 100)
  lvppK_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_lvK)
  lvppK_port <- plot_predprey_portrait(lvppK_out, params = params_lvK)
  lvppK_traj <- plot_predprey_time(lvppK_out)
  # Check that the plot is made as expected
  expect_equal(lvppK_port$labels$y, "Number of Predators")
  expect_equal(lvppK_traj$labels$colour, "Population")

  # LVPP with Type 2 carrying capacity
  params_lvT2 <- c(r = .1, a = .01, e = .01, d = .001, T_h = 0.1)
  lvppT2_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_lvT2)
  lvppT2_port <- plot_predprey_portrait(lvppT2_out, params = params_lvT2)
  lvppT2_traj <- plot_predprey_time(lvppT2_out)
  # Check that the plot is made as expected
  expect_equal(lvppT2_port$labels$y, "Number of Predators")
  expect_equal(lvppT2_traj$labels$colour, "Population")

  # Macarthur Rosenszweig model
  params_RM <- c(r = .1, a = .01, e = .01, d = .001, T_h = 0.1, K = 100)
  lvppRM_out <- run_predprey_model(5, init = c(H = 10, P = 5), params = params_RM)
  lvppRM_port <- plot_predprey_portrait(lvppRM_out, params = params_RM)
  lvppRM_traj <- plot_predprey_time(lvppRM_out)
  # Check that the plot is made as expected
  expect_equal(lvppRM_port$labels$y, "Number of Predators")
  expect_equal(lvppRM_traj$labels$colour, "Population")

})
