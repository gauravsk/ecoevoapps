#' Run the shiny apps for various models
#'
#' @param language Two-letter vector indicating the language
#' For most models, apps are available in the following languages:
#' english (\code{language = "en"}), spanish (\code{language = "es"}),
#' chinese (\code{language = "ch"}), turkish (\code{language = "tr"}),
#' and portuguese (\code{language = "pt"}).
#' @import rmarkdown
#' @name shiny_XXX
NULL
#> NULL

#' @rdname shiny_XXX
#' @export
shiny_singlepop_continuous <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), or Chinese (ch)" =
              (language %in% c("en", "tr", "ch", "es", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("single_population_continuous/single_population_app_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("single_population_continuous/single_population_app_tr.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("single_population_continuous/single_population_app_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("single_population_continuous/single_population_app_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("single_population_continuous/single_population_app_pt.Rmd",
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
shiny_population_growth_discrete <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
            (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("population_growth_discrete/population_growth_discrete_en.Rmd",
                                                         package =  "ecoevoapps"),
                           language == "es" ~ system.file("population_growth_discrete/population_growth_discrete_es.Rmd",
                                                          package =  "ecoevoapps"),
                           language == "ch" ~ system.file("population_growth_discrete/population_growth_discrete_ch.Rmd",
                                                          package =  "ecoevoapps"),
                           language == "pt" ~ system.file("population_growth_discrete/population_growth_discrete_pt.Rmd",
                                                          package =  "ecoevoapps"),
                           language == "tr" ~ system.file("population_growth_discrete/population_growth_discrete_tr.Rmd",
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
shiny_lvcomp <- function(language = "en") {
  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("lotka_volterra_competition/lotka_volterra_competition_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("lotka_volterra_competition/lotka_volterra_competition_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("lotka_volterra_competition/lotka_volterra_competition_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("lotka_volterra_competition/lotka_volterra_competition_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("lotka_volterra_competition/lotka_volterra_competition_tr.Rmd",
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
shiny_predprey <- function(language = "en") {
  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("predator_prey_dynamics/predator_prey_dynamics_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("predator_prey_dynamics/predator_prey_dynamics_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("predator_prey_dynamics/predator_prey_dynamics_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("predator_prey_dynamics/predator_prey_dynamics_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("predator_prey_dynamics/predator_prey_dynamics_tr.Rmd",
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
shiny_biotic_comp <- function(language = "en") {
  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("biotic_resource_competition/biotic_resource_competition_en",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("biotic_resource_competition/biotic_resource_competition_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("biotic_resource_competition/biotic_resource_competition_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("biotic_resource_competition/biotic_resource_competition_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("biotic_resource_competition/biotic_resource_competition_tr.Rmd",
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
shiny_infectious_disease <- function(language = "en") {
  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("infectious_diseases/infectious_disease_dynamics_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("infectious_diseases/infectious_disease_dynamics_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("infectious_diseases/infectious_disease_dynamics_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("infectious_diseases/infectious_disease_dynamics_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("infectious_diseases/infectious_disease_dynamics_tr.Rmd",
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
shiny_source_sink <- function() {
  rmarkdown::run(
    file = system.file("source_sink_dynamics/source_sink_dynamics.Rmd",
                       package =  "ecoevoapps"),
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}

#' @rdname shiny_XXX
#' @export
shiny_structured_population <- function(language = "en") {
  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("structured_population/structured_population_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("structured_population/structured_population_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("structured_population/structured_population_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("structured_population/structured_population_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("structured_population/structured_population_tr.Rmd",
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
shiny_ibiogeo_model <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tk", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("island_biogeography/ibiogeo_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("island_biogeography/ibiogeo_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("island_biogeography/ibiogeo_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("island_biogeography/ibiogeo_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tk" ~ system.file("island_biogeography/ibiogeo_tk.Rmd",
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
shiny_abiotic_comp <- function(language = "en") {
  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "tr", "pt")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("abiotic_resource_competition/abiotic_resource_competition_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("abiotic_resource_competition/abiotic_resource_competition_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("abiotic_resource_competition/abiotic_resource_competition_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("abiotic_resource_competition/abiotic_resource_competition_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("abiotic_resource_competition/abiotic_resource_competition_tr.Rmd",
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

#' @rdname shiny_XXX
#' @export
shiny_mutualism <- function(language = "en") {

  stopifnot("Your selected language is not (yet) available; please choose between English (en), Spanish (es), Portuguese (pt), Turkish (tr), or Chinese (ch)" =
              (language %in% c("en", "es", "ch", "pt", "tr")))

  file_path <- dplyr::case_when(language == "en" ~ system.file("mutualism/mutualism_en.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "es" ~ system.file("mutualism/mutualism_es.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "ch" ~ system.file("mutualism/mutualism_ch.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "pt" ~ system.file("mutualism/mutualism_pt.Rmd",
                                                               package =  "ecoevoapps"),
                                language == "tr" ~ system.file("mutualism/mutualism_tr.Rmd",
                                                               package =  "ecoevoapps"))

  rmarkdown::run(
    file = file_path,
    default_file = NULL,
    auto_reload = TRUE,
    shiny_args = NULL,
    render_args = NULL
  )
}
