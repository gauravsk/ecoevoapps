#' Run the shiny apps for various models
#'
#' @param language Two-letter vector indicating the language
#' For most models, apps are available in at least
#' three languages out of the following options:
#' english (\code{language = "en"}), spanish (\code{language = "es"}),
#' chinese (\code{language = "ch"}), turkish (\code{language = "tk"}),
#' and portuguese (\code{language = "pt}).
#' @name shiny_XXX
NULL
#> NULL

#' @rdname shiny_XXX
#' @export
shiny_singlepop_continuous <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), or Chinese (ch)" =
              (language %in% c("en", "tk", "ch")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("single_population/single_population_app_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tk" ~ system.file("single_population/single_population_app_tk.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("single_population/single_population_app_ch.Rmd",
                                                               package =  "ecoevoapps"))

  rmarkdown::run(
    file = file_path,
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_singlepop_discrete <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), or Chinese (ch)" =
            (language %in% c("en", "es", "ch")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("single_population_discrete/single_pop_discrete_app_en.Rmd",
                                                         package =  "ecoevoapps"),
                           language == "es" ~ system.file("single_population_discrete/single_pop_discrete_app_es.Rmd",
                                                          package =  "ecoevoapps"),
                           language == "ch" ~ system.file("single_population_discrete/single_pop_discrete_app_ch.Rmd",
                                                          package =  "ecoevoapps"))

  rmarkdown::run(
    file = file_path,
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
shiny_island_biogeo <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tk", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("ibiogeo/ibiogeo_app_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("ibiogeo/ibiogeo_app_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("ibiogeo/ibiogeo_app_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("ibiogeo/ibiogeo_app_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tk" ~ system.file("ibiogeo/ibiogeo_app_tk.Rmd",
                                                               package =  "ecoevoapps"))

  rmarkdown::run(
    file = file_path,
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

