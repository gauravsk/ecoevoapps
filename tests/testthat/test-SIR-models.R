# library(ecoevoapps)
test_that("SIR model works and plots", {
  params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0.01)
  init_vec <- c(S = 100, I = 1, R = 0)
  time_vec <- seq(0, 10, 0.1)
  sir_out <- run_infectiousdisease_model(time = time_vec,
                                         init = init_vec,
                                         params = params_vec,
                                         model_type = "SIR")
  last_row <- sir_out[nrow(sir_out),c("S", "I", "R")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  sir_plot <- plot_infectiousdisease_time(sir_out, "SIR")
  expect_equal(sir_plot$labels$colour, "group")
})

test_that("SIRD model works and plots", {
  params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0.01, mu = .01)
  init_vec <- c(S = 100, I = 1, R = 0, D = 0)
  time_vec <- seq(0, 10, 0.1)
  sird_out <- run_infectiousdisease_model(time = time_vec,
                                         init = init_vec,
                                         params = params_vec,
                                         model_type = "SIRD")
  last_row <- sird_out[nrow(sird_out),c("S", "I", "R", "D")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  sird_plot <- plot_infectiousdisease_time(sird_out, "SIRD")
  expect_equal(sird_plot$labels$colour, "group")

  # expect error if I don't specify mu (mortality rate)
  expect_error(run_infectiousdisease_model(time = time_vec,
                                           init = init_vec,
                                           params = c(m = .1, beta = .01,
                                                      v = .2, gamma = 0),
                                           model_type = "SIRD"))
})

test_that("SIS model works and plots", {
  params_vec <- c(m = .1, beta = .01, gamma = 0.01)
  init_vec <- c(S = 100, I = 1)
  time_vec <- seq(0, 10, 0.1)
  sis_out <- run_infectiousdisease_model(time = time_vec,
                                          init = init_vec,
                                          params = params_vec,
                                          model_type = "SIS")
  last_row <- sis_out[nrow(sis_out),c("S", "I")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  sis_plot <- plot_infectiousdisease_time(sis_out, "SIS")
  expect_equal(sis_plot$labels$colour, "group")

  # expect error if I don't specify gamma (recovery rate)
  expect_error(run_infectiousdisease_model(time = time_vec,
                                           init = init_vec,
                                           params = c(m = .1, beta = .01),
                                           model_type = "SIS"))
})



test_that("SEIR model works and plots", {
  params_vec <- c(m = .1, beta = .01, gamma = 0.01, v = 0, a = .02)
  init_vec <- c(S = 100, E = 0, I = 1, R = 0)
  time_vec <- seq(0, 10, 0.1)
  seir_out <- run_infectiousdisease_model(time = time_vec,
                                         init = init_vec,
                                         params = params_vec,
                                         model_type = "SEIR")
  last_row <- seir_out[nrow(seir_out),c("S", "E", "I", "R")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  seir_plot <- plot_infectiousdisease_time(seir_out, "SEIR")
  expect_equal(seir_plot$labels$colour, "group")

  # expect error if I don't specify gamma (recovery rate)
  expect_error(run_infectiousdisease_model(time = time_vec,
                                           init = init_vec,
                                           params = c(m = .1, beta = .01),
                                           model_type = "SEIR"))
})




# REPEAT ALL THE TESTS FOR FREQUENCY-DEPENDENT TRANSMISSION
test_that("SIR model works and plots", {
  params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0.01)
  init_vec <- c(S = 100, I = 1, R = 0)
  time_vec <- seq(0, 10, 0.1)
  sir_out <- run_infectiousdisease_model(time = time_vec,
                                         init = init_vec,
                                         params = params_vec,
                                         model_type = "SIR_ft")
  last_row <- sir_out[nrow(sir_out),c("S", "I", "R")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  sir_plot <- plot_infectiousdisease_time(sir_out, "SIR_ft")
  expect_equal(sir_plot$labels$colour, "group")
})

test_that("SIRD model works and plots", {
  params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0.01, mu = .01)
  init_vec <- c(S = 100, I = 1, R = 0, D = 0)
  time_vec <- seq(0, 10, 0.1)
  sird_out <- run_infectiousdisease_model(time = time_vec,
                                          init = init_vec,
                                          params = params_vec,
                                          model_type = "SIRD_ft")
  last_row <- sird_out[nrow(sird_out),c("S", "I", "R", "D")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  sird_plot <- plot_infectiousdisease_time(sird_out, "SIRD_ft")
  expect_equal(sird_plot$labels$colour, "group")

  # expect error if I don't specify mu (mortality rate)
  expect_error(run_infectiousdisease_model(time = time_vec,
                                           init = init_vec,
                                           params = c(m = .1, beta = .01,
                                                      v = .2, gamma = 0),
                                           model_type = "SIRD_ft"))
})

test_that("SIS model works and plots", {
  params_vec <- c(m = .1, beta = .01, gamma = 0.01)
  init_vec <- c(S = 100, I = 1)
  time_vec <- seq(0, 10, 0.1)
  sis_out <- run_infectiousdisease_model(time = time_vec,
                                         init = init_vec,
                                         params = params_vec,
                                         model_type = "SIS_ft")
  last_row <- sis_out[nrow(sis_out),c("S", "I")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  sis_plot <- plot_infectiousdisease_time(sis_out, "SIS_ft")
  expect_equal(sis_plot$labels$colour, "group")

  # expect error if I don't specify gamma (recovery rate)
  expect_error(run_infectiousdisease_model(time = time_vec,
                                           init = init_vec,
                                           params = c(m = .1, beta = .01),
                                           model_type = "SIS_ft"))
})



test_that("SEIR model works and plots", {
  params_vec <- c(m = .1, beta = .01, gamma = 0.01, v = 0, a = .02)
  init_vec <- c(S = 100, E = 0, I = 1, R = 0)
  time_vec <- seq(0, 10, 0.1)
  seir_out <- run_infectiousdisease_model(time = time_vec,
                                          init = init_vec,
                                          params = params_vec,
                                          model_type = "SEIR_ft")
  last_row <- seir_out[nrow(seir_out),c("S", "E", "I", "R")]
  # Check that the final row has a product of > 0, which means
  # no single element was 0
  expect_gt(prod(last_row), 0)

  seir_plot <- plot_infectiousdisease_time(seir_out, "SEIR_ft")
  expect_equal(seir_plot$labels$colour, "group")

  # expect error if I don't specify gamma (recovery rate)
  expect_error(run_infectiousdisease_model(time = time_vec,
                                           init = init_vec,
                                           params = c(m = .1, beta = .01),
                                           model_type = "SEIR_ft"))
})

