#' @export
run_singlepop_file <- function() {
  rmarkdown::run(
    file = "inst/single_population/single_populations_app.Rmd",
    dir = dirname(file),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}
