test_that("Abiotic resource-competition model works as expected", {
  rc_params <-  c(S1 = 12, S2 = 12, r1 = 1.6, r2 = 1,
                  k11 = 18, k12 = 4, k21 = 2, k22 = 14,
                  m1 = .2, m2 = .2,c11 = .25, c12 = .08,
                  c21 = .1, c22 = .2, a1 = .5, a2 = .5)
  rc_out <- run_abiotic_comp_model(time = seq(0,10, .1),
                                   init = c(N1 = 10, N2 = 10, R1 = 20, R2 = 20),
                                   params = rc_params)
  last_row <- rc_out[nrow(rc_out), c("N1", "N2", "R1", "R2")]
  # We should expect all values to be greater than zero in this
  expect_true(all(last_row > 0))

})

test_that("Abiotic resource-competition model plots as expected", {
  rc_params <-  c(S1 = 12, S2 = 12, r1 = 1.6, r2 = 1,
                  k11 = 18, k12 = 4, k21 = 2, k22 = 14,
                  m1 = .2, m2 = .2,c11 = .25, c12 = .08,
                  c21 = .1, c22 = .2, a1 = .5, a2 = .5)
  rc_out <- run_abiotic_comp_model(time = seq(0,10, .1),
                                   init = c(N1 = 10, N2 = 10, R1 = 20, R2 = 20),
                                   params = rc_params)
  rc_traj <- plot_abiotic_comp_time(rc_out)
  expect_equal(rc_traj$labels$colour, "species")

  rstar_vec <- run_abiotic_comp_rstar(rc_params)
  rc_port <- plot_abiotic_comp_portrait(rstar_vec = rstar_vec, rc_out)
  expect_equal(rc_port$labels$colour, "species")

})

test_that("Rstar vector generates as expected", {
  rc_params <-  c(S1 = 12, S2 = 12, r1 = 1.6, r2 = 1,
                  k11 = 18, k12 = 4, k21 = 2, k22 = 14,
                  m1 = .2, m2 = .2,c11 = .25, c12 = .08,
                  c21 = .1, c22 = .2, a1 = .5, a2 = .5)
  rstar_vec <- run_abiotic_comp_rstar(rc_params)

  # Each species draws a different resource down to a lower level
  expect_true(rstar_vec["R21"] < rstar_vec["R22"])
  expect_true(rstar_vec["R12"] < rstar_vec["R11"])

})
