## Basic functions
# The exact shape of the I and E functions should not alter
# the results qualitatively, as long as they are monotonic.

ibiogeo_I = function(S,D,M,k){ exp(-(k/D)*(S-M))-1 } # immigration rate: number of new species arriving per year

ibiogeo_E = function(S,A,M,k){ exp(k*S/A)-1 } # extinction rate: number of species being lost per year

ibiogeo_S = function(D,A,M,k){ A*M/(D+A) } # number of species at equilibrium


## Plotting function
mapNrates = function(D,A,M,k){

  par(mfrow=c(1,2))

  ## MAP
  plot(1:10,type="n",xaxt="n",ylab="Distance from the mainland (km)",ylim=c(-1,9),xlab="") # canvas

  # Mainland
  tx = c(0.6487523,1.0551068,1.5239774 ,1.8365578,2.2116542,2.5242346,3.3056856,4.2434268,5.0248778,5.4624904,6.0563931,6.6190378,7.3067147,7.3067147,7.2129406,7.5255210,8.1506818,9.1509390,9.9323900,10.2449704,15)
  ty = c(-0.002336449 ,0.207943925,0.278037383 ,0.278037383,0.067757009,-0.107476636,-0.352803738,-0.212616822,-0.037383178,0.067757009,0.137850467 ,0.242990654,0.242990654,0.242990654,-0.072429907,-0.282710280,-0.247663551,-0.107476636,-0.142523364,-0.212616822,0)
  polygon(x=c(tx,rev(tx)),y=c(ty,rep(-3,length(ty))),lwd=1,density=0)
  text(x=5,y=-1,"Mainland",font=1)

  # Island A
  tx = c(-1.98662207 ,-1.91715976 ,-1.63931052 ,-1.29199897, -1.04888089,-1.01414973,-0.94468742 ,-0.63210702 ,-0.38898894, -0.21533316,-0.11113970,0.09724723  ,0.34036532  ,0.68767687,  1.03498842,1.24337535,1.45176228  ,1.76434268  ,1.72961153,  1.52122459,1.34756882,1.27810651  ,1.34756882  ,1.41703113,  1.41703113,1.13918189,0.86133265  ,0.79187034  ,0.51402110,  0.34036532,0.23617185,0.09724723 ,-0.11113970 ,-0.25006432, -0.38898894,-0.56264471,-0.70156933, -0.94468742, -1.11834320, -1.56984821,-1.98662207)
  ty = c(-0.007009346,  0.273364486,  0.658878505,  0.799065421,  0.974299065, 1.184579439,  1.394859813,  1.535046729,  1.535046729,  1.394859813, 1.149532710,  1.114485981,  1.149532710,  1.184579439,  0.904205607, 0.764018692,  0.764018692,  0.553738318,  0.238317757,  0.203271028, 0.098130841, -0.042056075, -0.217289720, -0.532710280, -0.707943925,-0.988317757, -0.988317757, -0.988317757, -0.918224299, -1.058411215,-1.408878505, -1.408878505, -1.408878505, -1.268691589, -0.988317757,-0.848130841, -0.848130841, -0.883177570, -0.637850467, -0.532710280,-0.007009346)
  polygon(x=sqrt(A[1])*tx+3,y=sqrt(A[1])*ty+D[1]-sqrt(A[1])*min(ty),lwd=3,border=2)
  text(x=3,y=D[1]-sqrt(A[1])*min(ty),"A",col=2)

  # Island B
  tx = -tx; ty = -ty
  polygon(x=sqrt(A[2])*tx+8,y=sqrt(A[2])*ty+D[2]-sqrt(A[2])*min(ty),lwd=3,border=4)
  text(x=8,y=D[2]-sqrt(A[2])*min(ty),"B",col=4)


  ## RATES
  plot(1:10,xlab="Number of species",ylab="Rate (species per year)",xaxs="i",yaxs="i",ylim=c(0,4),xlim=c(0,M),type="n") # canvas

  # Immigration
  curve(I(x,D[1],M,k),from=0,to=M,col=2,add=T,lwd=2,lty=1) # immigration rate for island A
  curve(I(x,D[2],M,k),from=0,to=M,col=4,add=T,lwd=2,lty=1) # immigration rate for island B

  # Extinction
  curve(E(x,A[1],M,k),from=0,to=M,col=2,add=T,lwd=2,lty=2) # extinction rate for island A
  curve(E(x,A[2],M,k),from=0,to=M,col=4,add=T,lwd=2,lty=2) # extinction rate for island B

  # Equilibrium diversity and rates for island A
  ts = S(D[1],A[1],M,k); tr = I(ts,D[1],M,k)
  segments(x0=ts,x1=ts,y0=0,y1=tr,lty=3,col=2)
  mtext(at=ts,round(ts),side=1,font=2,col=2)

  # Equilibrium diversity and rates for island B
  ts = S(D[2],A[2],M,k); tr = I(ts,D[2],M,k)
  segments(x0=ts,x1=ts,y0=0,y1=tr,lty=3,col=4)
  if(!ts%in%seq(0,100,20)){mtext(at=ts,round(ts),side=1,font=2,col=4)}

  # Plot legend
  legend("topright",bty="n",col=c(2,4,1,1),
         pch=c(15,15,NA,NA),pt.cex=2,
         c("Island A","Island B","immigration","extinction"),
         lty=c(NA,NA,1,2),lwd=1, inset=c(-0.2,0))

  }






