# library(ecoevoapps)

test_that("Smith-Fretwell functions generate plots", {
  sf_out <- run_smith_fretwell_model(Iymin = 3, Womax = 0.6, Iymax = 1000)
  # This function just returns a plot, so we can check it has
  # the right number of layers
  expect_length(sf_out$layers, 7)

})

test_that("Adding a second species to Smith-Fretwell plots works as expected", {

  # Error if we don't provide the base (first species) plot
  expect_error(run_smith_fretwell_model_sp2(Iymin = 6, Womax = 0.6, Iymax = 1000))

  sf_out <- run_smith_fretwell_model(Iymin = 3, Womax = 0.6, Iymax = 1000)
  sf_out2 <- run_smith_fretwell_model_sp2(existing_plot = sf_out)

  # There should now be 13 layers with the second species added in
  expect_length(sf_out2$layers, 13)
})
