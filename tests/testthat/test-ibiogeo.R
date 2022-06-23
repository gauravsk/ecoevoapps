# library(ecoevoapps)
test_that("Check that Island Biogeography plots are rendered correctly", {
  default_ibiogeo_obj <- run_ibiogeo_model()

  expect_equal(names(default_ibiogeo_obj), c("eq_plot", "map"))
  expect_equal(default_ibiogeo_obj$eq_plot$labels$y,
               "Immigration or Extinction Rate\n(species/year)")
  expect_equal(default_ibiogeo_obj$map$labels$y,
               "Distance from mainland (km)")
  })
