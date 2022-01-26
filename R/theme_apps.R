#' standard ggplot2 theme to use throughout the package
#' @import ggplot2
#' @export
theme_apps <- function() {
  theme_minimal() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.tag = element_text(face = "bold"),
          axis.title = element_text(size = 15),
          plot.caption = element_text(size = 15)
    )
}


#' Set origin of ggplot objects to 0,0
#' @param plot ggplot object
#' @import ggplot2
#' @export
originator <- function(plot) {
  plot +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
}
