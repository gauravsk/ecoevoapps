#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
#' @importFrom kableExtra linebreak
#' @importFrom patchwork wrap_plots
#' @importFrom shiny br
#' @importFrom stats anova
#' @importFrom utils alarm
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

