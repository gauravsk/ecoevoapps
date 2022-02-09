#' Immigration rate for Island Biogeography model
#' @param S number of species on island
#' @param D distance from mainland (km)
#' @param M number of species in the mainland
#' @param k scaling constant
#' @keywords internal
ibiogeo_I <- function(S,D,M,k){
  exp(-(k/D)*(S-M))-1
}

#' Extinction rate for Island Biogeography model
#' @param S number of species on island
#' @param A size of island (km^2)
#' @param k scaling constant
#' @keywords internal
ibiogeo_E <- function(S,A,k) {
  exp(k*S/A)-1
}

#' Equilibrium number of species on island for Island Biogeography model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @param M number of species in the mainland
#' @param k scaling constant
#' @keywords internal
ibiogeo_Sx <- function(D,A,M,k){
  A*M/(D+A)
}

#' equilibrium immigration rate for Island Biogeography model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @keywords internal
ibiogeo_Ix <- function(D,A,M,k){
  exp(-(k/D)*(ibiogeo_Sx(D,A,M,k)-M))-1
}

#' equilibrium extinction rate for Island Biogeography model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @keywords internal
ibiogeo_Ex <- function(D,A,M,k){
  exp(k*A*ibiogeo_Sx(D,A,M,k))-1
}

#' make map for ibiogeo model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @param M Mainland species richness
#' @param k scaling constant
#' @param tx,ty,txa,tya,txb,tyb constants to draw islands on map
#' @import ggplot2
#' @keywords internal
make_islands_map <- function(tx, ty, txa, tya, txb, tyb,
                             D, A, k, M) {
  # RColorBrewer::brewer.pdal(3, "Set1")
  colorpal <- c("#E41A1C", "#377EB8", "#4DAF4A")

  ggplot() +
    geom_polygon(aes(x = c(tx, rev(tx)), y = c(ty, rep(-3,length(ty)))), fill = "grey90",
        color = "grey10") +
    geom_polygon(aes(x = sqrt(A[1])*txa+4, y = sqrt(A[1])*tya+D[1]-sqrt(A[1])*min(tya)),
        color = colorpal[1], fill = "transparent") +
    geom_polygon(aes(x = sqrt(A[2])*txb+12, y = sqrt(A[2])*tyb+D[2]-sqrt(A[2])*min(tyb)),
                 color = colorpal[2], fill = "transparent") +
    annotate("text", x = 4, y = D[1]-sqrt(A[1])*min(tya), label = "island\nA",
             color = colorpal[1]) +
    annotate("text", x = 12, y = D[2]-sqrt(A[2])*min(tyb), label = "island\nB",
             color = colorpal[2]) +
    annotate("text", x = 7.5, y = -1.25 , label = paste0("Mainland\n(", M, " species)"),
             hjust = 0.5, size = 6) +

    xlab("") +
    ylab("Distance from mainland (km)") +
    scale_x_continuous(expand = c(0,0)) +
    theme_apps()+
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()
    )

}

#' make equilibrium plot for ibiogeo model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @param M Mainland species richness
#' @param k scaling constant
#' @importFrom magrittr "%>%"
#' @keywords internal
make_equilibrium_plot <- function(D, A, M, k) {
  # RColorBrewer::brewer.pdal(3, "Set1")
  colorpal <- c("#E41A1C", "#377EB8", "#4DAF4A")

  ggplot(data.frame(x = c(0, M)), aes(x)) +
    stat_function(fun = ibiogeo_I, args = list(D = D[1], M = M, k = k),
                  aes(color = "colorpal[1]", linetype = "1")) +
    stat_function(fun = ibiogeo_I, args = list(D = D[2], M = M, k = k),
                  aes(color = "colorpal[2]", linetype = "1")) +
    stat_function(fun = ibiogeo_E, args = list(A = A[1], k = k),
                  aes(color = "colorpal[1]", linetype = "2")) +
    stat_function(fun = ibiogeo_E, args = list(A = A[2], k = k),
                  aes(color = "colorpal[2]", linetype = "2")) +
    ylab("Immigration or Extinction Rate\n(species/year)") +
    xlab("\nNumber of species on island") +
    geom_text(x = ibiogeo_Sx(D = D[1], A = A[1], M = M, k = k), y = 0, vjust = 3,
              label = floor(ibiogeo_Sx(D = D[1], A = A[1], M = M, k = k)), color = colorpal[1]) +
    geom_text(x = ibiogeo_Sx(D = D[2], A = A[2], M = M, k = k), y = 0, vjust = 3,
              label = floor(ibiogeo_Sx(D = D[2], A = A[2], M = M, k = k)), color = colorpal[2]) +
    coord_cartesian(clip = "off") +
    scale_color_manual(name = " ", guide = "legend", values = colorpal,
                       labels = c("Island A", "Island B")) +
    scale_linetype_manual(name = "", values = c(1,2),
                          labels = c("Immigration\nrate", "Extinction\nrate")) +
    geom_segment(aes(x = ibiogeo_Sx(D[1], A[1], M = M, k = k),
                     xend = ibiogeo_Sx(D[1], A[1], M = M, k = k),
                     y = 0,
                     yend = ibiogeo_E(A = A[1],k = k, S = ibiogeo_Sx(D[1], A[1], M = M, k = k))),
                 color = colorpal[1], linetype = 3) +
    geom_segment(aes(x = ibiogeo_Sx(D = D[2], A = A[2], M = M, k = k),
                     xend = ibiogeo_Sx(D = D[2], A = A[2], M = M, k = k),
                     y = 0,
                     yend = ibiogeo_E(A = A[2],k = k, S = ibiogeo_Sx(D[2], A[2], M = M, k = k))),
                 color = colorpal[2], linetype = 3) +
    theme_apps() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position = "top", legend.text = element_text(size = 12))
}


#' Runs the island biogeography model.
#'
#' The function takes as inputs the size of two islands as well as their
#' distance to the mainland, and uses these paramters to illustrate the equilibrium
#' diversity of two islands.
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @param k scaling parameter (defaults to 0.015)
#' @param M species richness of mainland
#' @return Two ggplot objects: first, plot showing the equilibrium diversity of each island;
#'  second, a "map" illustrating island size and distance from mainland.
#' @examples
#' run_ibiogeo_model(D = c(1,4), A = c(1,0.5))
#' @export
run_ibiogeo_model <- function(D = c(1,4), A = c(1,0.5), k = 0.015, M = 100) {
  # set some values to make plots
  tx = c(0.6487523,1.0551068,1.5239774 ,1.8365578,2.2116542,2.5242346,3.3056856,4.2434268,5.0248778,5.4624904,6.0563931,6.6190378,7.3067147,7.3067147,7.2129406,7.5255210,8.1506818,9.1509390,9.9323900,10.2449704,15)
  ty = c(-0.002336449 ,0.207943925,0.278037383 ,0.278037383,0.067757009,-0.107476636,-0.352803738,-0.212616822,-0.037383178,0.067757009,0.137850467 ,0.242990654,0.242990654,0.242990654,-0.072429907,-0.282710280,-0.247663551,-0.107476636,-0.142523364,-0.212616822,0)
  txa = c(-1.98662207 ,-1.91715976 ,-1.63931052 ,-1.29199897, -1.04888089,-1.01414973,-0.94468742 ,-0.63210702 ,-0.38898894, -0.21533316,-0.11113970,0.09724723  ,0.34036532  ,0.68767687,  1.03498842,1.24337535,1.45176228  ,1.76434268  ,1.72961153,  1.52122459,1.34756882,1.27810651  ,1.34756882  ,1.41703113,  1.41703113,1.13918189,0.86133265  ,0.79187034  ,0.51402110,  0.34036532,0.23617185,0.09724723 ,-0.11113970 ,-0.25006432, -0.38898894,-0.56264471,-0.70156933, -0.94468742, -1.11834320, -1.56984821,-1.98662207)
  tya = c(-0.007009346,  0.273364486,  0.658878505,  0.799065421,  0.974299065, 1.184579439,  1.394859813,  1.535046729,  1.535046729,  1.394859813, 1.149532710,  1.114485981,  1.149532710,  1.184579439,  0.904205607, 0.764018692,  0.764018692,  0.553738318,  0.238317757,  0.203271028, 0.098130841, -0.042056075, -0.217289720, -0.532710280, -0.707943925,-0.988317757, -0.988317757, -0.988317757, -0.918224299, -1.058411215,-1.408878505, -1.408878505, -1.408878505, -1.268691589, -0.988317757,-0.848130841, -0.848130841, -0.883177570, -0.637850467, -0.532710280,-0.007009346)
  txb = -txa; tyb = -tya


  islands_map <- make_islands_map(tx = tx, ty = ty, txa = txa, tya = tya,
                                  txb = txb, tyb = tyb, D = D,
                                  A = A, M = M, k = k)

  eq_plot <- make_equilibrium_plot(D = D, A = A, M = M, k = k)

  return(list(eq_plot = eq_plot, map = islands_map))
}
