test_that("The biotic resource competitin model runs with expected inputs", {
  rc_time <- seq(0,10)
  rc_init <- c(H = 30, P1 = 25, P2 = 25)
  rc_params <-  c(r = 0.2, q = .0066,
             a1 = .02, T_h1 = 0.1, e1 = 0.4, d1 = 0.1,
             a2 = .02, T_h2 = 0.1, e2 = 0.39, d2 = 0.1)
  rc_out <- run_biotic_comp_model(time = rc_time, init = rc_init, params = rc_params)
  last_row <- rc_out[nrow(rc_out), c("H", "P1", "P2")]
  expect_true(all(last_row > 0))

  # expect errors with missing parameters
  rc_params2 <-  c(r = 0.2, q = .0066,
                  a1 = .02, T_h1 = 0.1, e1 = 0.4, # d1 = 0.1,
                  a2 = .02, T_h2 = 0.1, e2 = 0.39, d2 = 0.1)
  expect_error(run_biotic_comp_model(time = rc_time,
                                     init = rc_init,
                                     params = rc_params2))

  # expect errors with missing init value
  rc_init2 <- c(H = 30, P1 = 25)
  expect_error(run_biotic_comp_model(time = rc_time,
                                     init = rc_init2,
                                     params = rc_params))

  })

test_that("Biotic resource competition plots are generated", {
  rc_time <- seq(0,10)
  rc_init <- c(H = 30, P1 = 25, P2 = 25)
  rc_params <-  c(r = 0.2, q = .0066,
                  a1 = .02, T_h1 = 0.1, e1 = 0.4, d1 = 0.1,
                  a2 = .02, T_h2 = 0.1, e2 = 0.39, d2 = 0.1)
  rc_out <- run_biotic_comp_model(time = rc_time, init = rc_init, params = rc_params)

  rc_plot <- plot_biotic_comp_time(rc_out)
  expect_equal(rc_plot$labels$colour, "Population")
})

test_that("Biotic resource competition apps' functional response plots generate as expected", {
  params <- c(r = 0.2, q = .0066,
              a1 = .02, T_h1 = 0.1, e1 = 0.4, d1 = 0.1,
              a2 = .02, T_h2 = 0.5, e2 = 0.2, d2 = 0.1)
  funresp_plot <- plot_functional_responses(params)

  expect_length(funresp_plot, 2)
  expect_equal(funresp_plot[[1]]$labels$x, "Prey Density")
  expect_equal(funresp_plot[[2]]$labels$x, "Prey Density")
})
