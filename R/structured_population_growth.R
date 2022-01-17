# Leslie =
#   matrix(
#     c(0     , 8,      1, 1,
#       40/100, 0,      0, 0,
#       0,      80/100, 0, 0,
#       0,      0,      10/100, 0),
#     ncol = 4, byrow = T,
#     dimnames = list(NULL, c("Age 1","Age 2", "Age 3", "Age 4"))
#   )
#
# init = c(100,100,100)
# tt = 100

simulate_structured_population <- function(leslie_mat = Leslie,
                                           init = c(100,100,100),
                                           time = 100) {
  pop <- matrix(init, nrow = length(init), ncol = 1) # start the matrix with initial population sizes
  nn <- sum(init) # total population size
  lambda = numeric() # vector to hold discrete population growth

  # simulate dynamics in a for-loop
  for(i in 1:time) {
    # run one time-step of the model
    pop <- cbind(pop, floor(leslie_mat %*% pop[,i,drop=F]))
    nn[i+1] <- sum(pop[1:3,i+1]) # total population size
    lambda[i] <- nn[i+1]/nn[i] # calculate lambda for the whole population
    if(nn[i+1]==0) { break } # stop if population crashes
    # stop when stages reached equilibrium
    if(i > 10) {
      if(round(lambda[i],3) == round(lambda[i-2],3)){
        break
      }
    }
  }
  rownames(pop) <- paste0("Age ", 1:nrow(pop))
  return(pop)
}

popmat_to_df <- function(pop_growth_matrix) {
  popgrowth_df <-
    pop_growth_matrix %>%
    t() %>%
    as_tibble %>%
    mutate(time = row_number()) %>%
    pivot_longer(cols = -time, names_to = "Age class",
                 values_to = "Population size")
  return(popgrowth_df)
}

plot_structured_population_size <- function(pop_growth_matrix) {

  pop_growth_df <- popmat_to_df(pop_growth_matrix)
  # if(eigen(leslie)$values[1]<1) {tmp = .8} else {tmp = .2}

  trajs <-
    ggplot(pop_growth_df, aes(x = time, y = `Population size`, group = `Age class`)) +
    geom_line(aes(linetype = `Age class`, col = `Age class`)) +
    labs(title = "Population growth simulation") +
    xlab("Time step") +
    ylab("Number of individuals") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_log10(expand = c(0, 0)) +
    # scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
    # expand = c(0, 0)) +
    ecoevoapps::theme_apps()

  return(trajs)
}


plot_structured_population_agedist <- function(pop_growth_matrix, leslie_mat = NULL) {

  pop_growth_df_prop <- popmat_to_df(pop_growth_matrix) %>%
    group_by(factor(time)) %>%
    mutate(prop = `Population size`/sum(`Population size`))
  # if(eigen(leslie)$values[1]<1) {tmp = .8} else {tmp = .2}

  trajs <-
    ggplot(pop_growth_df_prop, aes(x = time, y = prop, group = `Age class`)) +
    geom_line(aes(linetype = `Age class`, col = `Age class`)) +
    labs(title = "Trajectory of age structure ") +
    xlab("Time step") +
    ylab("Proportion of individuals") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ecoevoapps::theme_apps()

  # Add stable age distribution from Leslie matrix
  # if the leslie matrix is provided to the function
  if(!is.null(leslie_mat)) {
    trajs <-
      trajs +
      geom_hline(yintercept=c(Re(eigen(leslie_mat)$vec[,1])/
                                sum(Re(eigen(leslie_mat)$vec[,1]))),
                 linetype="dashed", show.legend=F) +
      labs(caption = "Dashed black lines show stable age distribution
           calculated from the Eigenvalues of the Leslie matrix") +
      theme(plot.caption = element_text(size = 8))

  }
  return(trajs)
}


plot_structured_population_lambda <- function(pop_growth_matrix,
                                              leslie_mat = NULL) {
  pop_growth_df <-
    t(pop_growth_matrix) %>%
    as_tibble() %>%
    mutate(total_popsize = rowSums(.),
           lambda = total_popsize/lag(total_popsize),
           time = row_number()) %>%
    na.omit # eliminate the first row, as there is no lambda for time step 1

  lambda_plot <-
    ggplot(pop_growth_df, aes(x = time, y = lambda)) +
    geom_line(color = "grey50") +
    labs(title = "Overall population growth rate") +
    xlab("Time step") +
    ylab(expression("Population growth rate (" ~lambda* ")")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ecoevoapps::theme_apps()


  # Add stable age distribution from Leslie matrix
  # if the leslie matrix is provided to the function
  if(!is.null(leslie_mat)) {
    lambda_plot <-
      lambda_plot +
      geom_hline(yintercept = Re(eigen(leslie_mat)$val[1])) +
      labs(caption = "Solid black lines shows the long-term population growth
           rate calculated from the dominant eigenvalue of the Leslie matrix") +
      theme(plot.caption = element_text(size = 8))

  }
  return(lambda_plot)
}
