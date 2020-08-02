### Simulation of structured population growth
strpop = function(
  # Arguments
  leslie, # Leslie matrix
  init, # initial number of individuals in each age class
  tt = 1000 # number of simulation time steps
  ){
  
  ## Population growth simulation
  pop = matrix(init,nrow=3,ncol=1) # start the matrix with initial population sizes
  nn = sum(init) # total population size
  lambda = numeric() # discrete population growth
  for(i in 1:tt){
    pop = cbind(pop, floor(leslie %*% pop[,i,drop=F])) # run one time-step of the model
    nn[i+1] = sum(pop[1:3,i+1]) # total population size
    lambda[i] = nn[i+1]/nn[i] # calculates discrete growth rate for the whole population
    if(nn[i+1]==0){break} # stop if population crashes
    if(i>10){if(round(lambda[i],3)==round(lambda[i-2],3)){break}} # stop when stages reached equilibrium
    }
  
  ## Simulation plots
  # Growth trajectories of each age class
  if(eigen(leslie)$values[1]<1) {tmp = .8} else {tmp = .2}
  trajs = ggplot(melt(t(pop)), aes(x=Var1,y=value,group=Var2)) +
    geom_line(aes(linetype=factor(Var2),col=factor(Var2))) + 
    theme_bw() + ggtitle("Simulation") +
    theme(panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), legend.position = "none",
          axis.line=element_line(colour = "black")) + 
    xlab("Time step") + ylab("Number of individuals") +
    scale_linetype_discrete(name="Age class") + 
    scale_colour_discrete(name="Age class") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                       expand = c(0, 0))
  
  # Proportions of each age class through time
  ageds = ggplot(melt(cbind(pop[1,]/nn,pop[2,]/nn,pop[3,]/nn)),
                 aes(x=Var1,y=value,group=Var2)) +
    geom_line(aes(linetype=factor(Var2),col=factor(Var2))) + 
    theme_bw() + ggtitle("Stable age distribution") +
    theme(panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), legend.position = "none",
          axis.line=element_line(colour = "black")) + 
    xlab("Time step") + ylab("Number of individuals") +
    scale_linetype_discrete(name="Age class") + 
    scale_colour_discrete(name="Age class") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    geom_hline(yintercept=c(eigen(leslie)$vec[,1]/sum(eigen(leslie)$vec[,1])), 
               linetype="dashed", color = 1, show.legend=F)
  
  # Total population growth
  totpg = ggplot(data.frame(tt=1:length(lambda),ll=lambda), 
                 aes(x=tt,y=lambda)) +
    geom_line(col="grey") + 
    theme_bw() + ggtitle("Discrete population growth") +
    theme(panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), legend.position = "none",
          axis.line=element_line(colour = "black")) + 
    xlab("Time step") + ylab(expression(lambda)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    geom_hline(aes(yintercept=eigen(leslie)$val[1], 
                   linetype="largest \neigenvalue"), 
               color = 1, show.legend=T) +
    scale_linetype_manual(name="",values=2)
  
  # Legends
  legen = get_legend(
    ggplot(melt(t(pop)), aes(x=Var1,y=value,group=Var2)) +
      geom_line(aes(linetype=factor(Var2),col=factor(Var2))) +
      scale_linetype_discrete(name="Age class") + 
      scale_colour_discrete(name="Age class")
    )
  
  
  return(plot_grid(trajs,totpg,ageds,legen,ncol=2,nrow=2))
    
  }
