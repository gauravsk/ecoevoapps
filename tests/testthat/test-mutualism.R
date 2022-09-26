test_that("mutualism model breaks with wrong time values", {
  expect_error(run_mutualism(time = "50"))
  expect_error(run_mutualism(time = 1:50))
  expect_error(run_mutualism(time = -1:50))
})

test_that("mutualism model breaks with wrong initial values", {
  expect_error(run_mutualism(init = c("20", "40")))
  expect_error(run_mutualism(init = c(20, 40)))
  expect_error(run_mutualism(init = c(N1 = 20)))
  expect_error(run_mutualism(init = c(N1 = 20, N3 = 40)))
  expect_error(run_mutualism(init = c(N1 = -20, N2 = 40)))
})

test_that("mutualism model breaks with wrong parameter values", {
  expect_error(run_mutualism(params = c("0.5", "0.5", "0.8", "0.4", "10", "10", "0.02", "0.01")))
  expect_error(run_mutualism(params = c(0.5, 0.5, 0.8, 0.4, 10, 10, 0.02, 0.01)))
  expect_error(run_mutualism(params = c(r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01)))
  expect_error(run_mutualism(params = c(r1 = 0.5, r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, a11 = 0.02, a22 = 0.01)))
  expect_error(run_mutualism(params = c(r1 = 0.5, r2 = 0.5, a12 = -0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01)))
})

test_that("mutualism model returns data frame with appropriate attributes", {
  expect_s3_class(run_mutualism(), "data.frame")
  expect_true(all(names(run_mutualism()) %in% c("time", "N1", "N2")))
  expect_true("model" %in% names(attributes(run_mutualism())))
  expect_true("params" %in% names(attributes(run_mutualism())))
  expect_true("K" %in% names(attributes(run_mutualism())))
})

test_that("mutualism model returns expected outcomes", {
  # Coexistence
  out_c <- run_mutualism(params = c(r1 = 0.5, r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01))
  expect_gt(out_c$N1[nrow(out_c)], 0)
  expect_gt(out_c$N2[nrow(out_c)], 0)
  # Exclusion of species 1
  out_e1 <- run_mutualism(params = c(r1 = -1, r2 = 0.5, a12 = 0.01, a21 = 0.4, b1 = 10, b2 = 10, d1 = 1, d2 = 0.01))
  expect_equal(out_e1$N1[nrow(out_e1)], 0)
  expect_gt(out_e1$N2[nrow(out_e1)], 0)
  # Exclusion of species 2
  out_e2 <- run_mutualism(params = c(r1 = 0.5, r2 = -1, a12 = 0.8, a21 = 0.01, b1 = 10, b2 = 10, d1 = 0.02, d2 = 1))
  expect_gt(out_e2$N1[nrow(out_e2)], 0)
  expect_equal(out_e2$N2[nrow(out_e2)], 0)
  # Exclusion of both species
  out_e12 <- run_mutualism(params = c(r1 = -1, r2 = -1, a12 = 0.01, a21 = 0.01, b1 = 10, b2 = 10, d1 = 1, d2 = 1))
  expect_equal(out_e12$N1[nrow(out_e12)], 0)
  expect_equal(out_e12$N2[nrow(out_e12)], 0)
})

test_that("N vs. time plots for mutualism model are generated as expected", {
  out <- run_mutualism()
  plot_time <- plot_mutualism_time(out)
  expect_s3_class(plot_time, c("gg", "ggplot"))
  expect_true("GeomHline" %in% class(plot_time$layers[[1]]$geom))
  expect_true("GeomLine" %in% class(plot_time$layers[[2]]$geom))
  expect_identical(plot_time$labels$colour, "Species")
})

test_that("phase portrait plots for mutualism model are generated as expected", {
  out <- run_mutualism()
  plot_portrait <- plot_mutualism_portrait(out)
  expect_s3_class(plot_portrait, c("gg", "ggplot"))
  expect_true("GeomLine" %in% class(plot_portrait$layers[[1]]$geom))
  expect_identical(plot_portrait$labels$colour, "Species")
})
