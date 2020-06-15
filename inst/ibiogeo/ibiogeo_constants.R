# set some default values
D = c(1,4) # distances from mainland (km)
A = c(1,.5) # island size (km^2)
M = 100 # number of species in the mainland
k = 0.015 # scaling constant

# set some values to make plots
tx = c(0.6487523,1.0551068,1.5239774 ,1.8365578,2.2116542,2.5242346,3.3056856,4.2434268,5.0248778,5.4624904,6.0563931,6.6190378,7.3067147,7.3067147,7.2129406,7.5255210,8.1506818,9.1509390,9.9323900,10.2449704,15)
ty = c(-0.002336449 ,0.207943925,0.278037383 ,0.278037383,0.067757009,-0.107476636,-0.352803738,-0.212616822,-0.037383178,0.067757009,0.137850467 ,0.242990654,0.242990654,0.242990654,-0.072429907,-0.282710280,-0.247663551,-0.107476636,-0.142523364,-0.212616822,0)
txa = c(-1.98662207 ,-1.91715976 ,-1.63931052 ,-1.29199897, -1.04888089,-1.01414973,-0.94468742 ,-0.63210702 ,-0.38898894, -0.21533316,-0.11113970,0.09724723  ,0.34036532  ,0.68767687,  1.03498842,1.24337535,1.45176228  ,1.76434268  ,1.72961153,  1.52122459,1.34756882,1.27810651  ,1.34756882  ,1.41703113,  1.41703113,1.13918189,0.86133265  ,0.79187034  ,0.51402110,  0.34036532,0.23617185,0.09724723 ,-0.11113970 ,-0.25006432, -0.38898894,-0.56264471,-0.70156933, -0.94468742, -1.11834320, -1.56984821,-1.98662207)
tya = c(-0.007009346,  0.273364486,  0.658878505,  0.799065421,  0.974299065, 1.184579439,  1.394859813,  1.535046729,  1.535046729,  1.394859813, 1.149532710,  1.114485981,  1.149532710,  1.184579439,  0.904205607, 0.764018692,  0.764018692,  0.553738318,  0.238317757,  0.203271028, 0.098130841, -0.042056075, -0.217289720, -0.532710280, -0.707943925,-0.988317757, -0.988317757, -0.988317757, -0.918224299, -1.058411215,-1.408878505, -1.408878505, -1.408878505, -1.268691589, -0.988317757,-0.848130841, -0.848130841, -0.883177570, -0.637850467, -0.532710280,-0.007009346)
txb=-txa; tyb=-tya

colorpal <- RColorBrewer::brewer.pal(2, "Set1")

make_islands_map <- function(tx, ty, txa, tya, txb, tyb,
                             D, A, k, M) {
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
    annotate("text", x = 7.5, y = -1 , label = "Mainland", hjust = 0, size = 6) +

    xlab("") +
    ylab("Distance from mainland (km)") +
    scale_x_continuous(expand = c(0,0)) +
    theme_apps()+
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x= element_blank(), axis.ticks.x = element_blank())

}


make_equilibrium_plot <- function(D, A, M, k) {
  (ggplot(data.frame(x = c(0, M)), aes(x)) +
    stat_function(fun = ibiogeo_I, args = list(D = D[1], M = M, k = k), aes(color = "colorpal[1]", linetype = "1")) +
    stat_function(fun = ibiogeo_I, args = list(D = D[2], M = M, k = k), aes(color = "colorpal[2]", linetype = "1")) +
    stat_function(fun = ibiogeo_E, args = list(A = A[1], M = M, k = k), aes(color = "colorpal[1]", linetype = "2")) +
    stat_function(fun = ibiogeo_E, args = list(A = A[2], M = M, k = k), aes(color = "colorpal[2]", linetype = "2")) +
    ylab("Immigration or Extinction Rate\n(species/year)") +
    xlab("\nNumber of species on island") +
    geom_segment(aes(x = ibiogeo_Sx(D[1],A[1], M = M, k = k), xend = ibiogeo_Sx(D[1],A[1], M = M, k = k),
                     y = 0, yend = ibiogeo_Ex(D[1],A[1])), color = colorpal[1], linetype = 3) +
    geom_text(x = ibiogeo_Sx(D[1],A[1], M = M, k = k), y = 0, vjust = 3, label = floor(ibiogeo_Sx(D[1],A[1], M = M, k = k)), color = colorpal[1]) +
    geom_text(x = ibiogeo_Sx(D[2],A[2], M = M, k = k), y = 0, vjust = 3, label = floor(ibiogeo_Sx(D[2],A[2], M = M, k = k)), color = colorpal[2]) +
    coord_cartesian(clip = "off") +
    scale_color_manual(name = " ", guide = "legend", values = colorpal, labels = c("Island A", "Island B")) +
    scale_linetype_manual(name = "", values = c(1,2), labels = c("Immigration rate", "Extinction rate")) +
    geom_segment(aes(x = ibiogeo_Sx(D[2],A[2], M = M, k = k), xend = ibiogeo_Sx(D[2],A[2], M = M, k = k),
                     y = 0, yend = ibiogeo_Ex(D[2],A[2])), color = colorpal[2], linetype = 3) +
    theme_apps() +
    theme(legend.position = "top"))  %>%
  originator +
  NULL
}
make_equilibrium_plot(D, A, M, k)
