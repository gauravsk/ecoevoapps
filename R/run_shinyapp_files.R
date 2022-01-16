#' Run the shiny apps for various models
#'
#' @param language Two-letter vector indicating the language
#' @name shiny_XXX
NULL
#> NULL

#' @rdname shiny_XXX
#' @export
shiny_singlepop_continuous <- function() {
  rmarkdown::run(
    file = system.file("single_population/single_populations_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_singlepop_discrete <- function() {
  rmarkdown::run(
    file = system.file("single_population_discrete/single_pop_discrete_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_lvcompetition <- function() {
  rmarkdown::run(
    file = system.file("lotka_volterra_competition/lotka_volterra_competition_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_consumer_resource <- function() {
  rmarkdown::run(
    file = system.file("consumer_resource_interactions/consumer_resource_interactions.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_biotic_comp <- function() {
  rmarkdown::run(
    file = system.file("biotic_resource_competition/biotic_resource_competition.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}



#' @rdname shiny_XXX
#' @export
shiny_infectious_disease <- function() {
  rmarkdown::run(
    file = system.file("SIR/SIR_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_metapopulation <- function() {
  rmarkdown::run(
    file = system.file("source_sink_metapopulation/source_sink_shiny.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_structured_pop <- function() {
  rmarkdown::run(
    file = system.file("structured_population/structured_population.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}


#' @rdname shiny_XXX
#' @export
shiny_island_biogeo <- function() {
  rmarkdown::run(
    file = system.file("ibiogeo/island_biogeo_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_abiotic_comp <- function() {
  rmarkdown::run(
    file = system.file("abiotic_resource_comp/abiotic_rc_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_smith_fretwell <- function() {
  rmarkdown::run(
    file = system.file("smith_fretwell_model/smith_fretwell_app.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

