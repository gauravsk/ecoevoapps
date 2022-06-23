# Smith-Fretwell Function
Wo <- function(Iy, Iymin = Iymin, Iymax = Iymax, Womax = Womax, shapeval = .02) {
  (Womax*(Iy-Iymin)) / (shapeval*(Iymax + Iymin)/2+Iy)
}

#' Make a Smith-Fretwell plot to show the optimal seed size
#' @param Iymin Minimum seed size for a viable offspring
#' @param Womax Maximum fitness that an offspring can achieve
#' @param Iymax Total amount of energy an adult has to invest into reproduction
#' @param shapeval slope of the Smith-Fretwell curve
#' @import ggplot2
#' @examples
#' run_smith_fretwell_model(Iymin = 3, Womax = 0.6, Iymax = 1000)
#' @export
run_smith_fretwell_model <- function(Iymin = 3, Womax = 0.6, Iymax = 1000, shapeval = .02) {
  aval <- shapeval*(Iymin+Iymax)/2
  xopt <- Iymin + sqrt(Iymin^2 + aval*Iymin)
  yopt <- Wo(xopt, Iymin = Iymin, Iymax = Iymax, Womax = Womax)
  lineslope <- (Womax*(aval+Iymin))/(aval+Iymin+sqrt(Iymin^2+aval*Iymin))^2
  df2 <- data.frame(xval = 0:25)

  smith_fretwell_plot <-
    ggplot(data = df2) +
    coord_cartesian(clip = "off") +
    geom_function(fun = Wo,
                  args = list(Iymin = Iymin, Iymax = Iymax, Womax = Womax),
                  n = 1000) +
    ylab("Offspring Fitness (Wo)") +
    xlab("Seed size") +
    scale_x_continuous(limits = c(0, 45), expand = c(0,0)) +
    scale_y_continuous(limits = c(-.1, 0.65), expand = c(0,0)) +
    geom_hline(yintercept = 0, linetype = 1) +
    geom_point(aes(x = xopt, y = yopt)) +
    geom_abline(intercept = 0,
                linetype = 2,
                slope = lineslope) +
    geom_segment(aes(x = xopt, xend = xopt, y = -0.04, yend = yopt), linetype = 2, size = 0.1) +
    annotate("text", x = xopt, y = -0.05, label = paste0("S_opt = ", floor(xopt)),  vjust = 1) +
    annotate("text", x = xopt, y = yopt, label = paste0(" Wp = ", floor(yopt*(Iymax/xopt))), hjust = 0, vjust = 1) +
    ecoevoapps::theme_apps() +
    NULL

  return(smith_fretwell_plot)
}

#' Add a new Smith-Fretwell curve to a ggplot object created with
#' run_smith_fretwell_model
#' @param existing_plot ggplot object created with run_smith_fretwell_model that
#'   has the first (base) curve
#' @param Iymin Minimum seed size for a viable offspring
#' @param Womax Maximum fitness that an offspring can achieve
#' @param Iymax Total amount of energy an adult has to invest into reproduction
#' @param shapeval slope of the Smith-Fretwell curve
#' @import ggplot2
#' @examples
#' sf_species1 <- run_smith_fretwell_model(Iymin = 3, Womax = 0.6, Iymax = 1000)
#' run_smith_fretwell_model_sp2(sf_species1, Iymin = 6, Womax = 0.6, Iymax = 1000)
#' @export
run_smith_fretwell_model_sp2 <- function(existing_plot, Iymin = 6, Womax = .6,
                                         Iymax = 1000, shapeval = 0.02) {

  # Define parameters
  Iymin2 <- Iymin
  Womax2 <- Womax
  Iymax2 <- Iymax
  aval2 <- shapeval*(Iymin2+Iymax2)/2
  xopt2 <- Iymin2 + sqrt(Iymin2^2 + aval2*Iymin2)
  yopt2 <- Wo(xopt2, Iymin = Iymin2, Iymax = Iymax2, Womax = Womax2)
  lineslope2 <- (Womax2*(aval2+Iymin2))/(aval2+Iymin+sqrt(Iymin2^2+aval2*Iymin2))^2

  # Add smith-fretwell plot with new parameters to existing plot
  existing_plot +
    geom_point(x = xopt2, y = yopt2, color = "#619cff") +
    geom_abline(intercept = 0,
                linetype = 2,
                slope = lineslope2,
                color = "#619cff") +
    geom_function(fun = Wo,
                  args = list(Iymin = Iymin2, Iymax = Iymax2, Womax = Womax2, shapeval = shapeval),
                  n = 1000, color = "#619cff") +
    geom_segment(aes(x = xopt2, xend = xopt2, y = 0, yend = yopt2),
                 linetype = 2, size = 0.1, color = "#619cff") +
    annotate("text", x = xopt2, y = -0.02, label = paste0("S_opt = ", floor(xopt2)),  vjust = 1) +
    annotate("text", x = xopt2, y = yopt2,
             label = paste0(" Wp = ", floor(yopt2*(Iymax/xopt2))), hjust = 0, vjust = 1)

}
