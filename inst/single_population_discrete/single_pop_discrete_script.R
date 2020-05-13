# Script (entirely non-Shiny) to run a discrete-time
# single population model (geometric or not)

time <- 100
N_t <- 10
lambda <- 1.1
K <- 500

exponential_pop_traj <- c(N_t,N_t*(lambda^(1:time)))
exponential_pop_df <- data.frame(time = 0:time,
                                 N_t= exponential_pop_traj,
                                 popdiff = c(NA, diff(exponential_pop_traj)),
                                 Ntm1 = c(lag(exponential_pop_traj)),
                                 popratio = exponential_pop_traj/lag(exponential_pop_traj))
(trajaectory_plot <- ggplot(data.frame(exponential_pop_df)) +
  geom_line(aes(y = N_t, x = time), size = .2) +
    geom_point(aes(y = N_t, x = time), size = 1.5, shape = 21,
               fill = "white", stroke = .8) +
    ylab("Population size (N_t)") +
  ecoevoapps::theme_apps())

(nt_over_ntm1 <- ggplot(data.frame(exponential_pop_df)) +
    geom_line(aes(y = N_t, x = Ntm1), size = .2) +
    geom_point(aes(y = N_t, x = Ntm1), size = 1.5, shape = 21,
               fill = "white", stroke = .8) +
    ylab("Population size at time t (N_t)") +
    xlab("Population size at time t-1 (N_[t-1])") +
    ecoevoapps::theme_apps())
(nt_minus_ntm1 <- ggplot(data.frame(exponential_pop_df)) +
    geom_line(aes(y = popdiff, x = time), size = .2) +
    geom_point(aes(y = popdiff, x = time), size = 1.5, shape = 21,
               fill = "white", stroke = .8) +
    ylab("Population size (N_t)") +
    ecoevoapps::theme_apps())

dlogistic <- function(K = 100, rd = 1, N0 = 2, t = 15) {
  Nvec <- c(N0, numeric(t))
  for (current_time in 2:(t+1)) {
    Nvec[current_time] <- Nvec[current_time-1] +
      rd * Nvec[current_time-1] *
      (1 - Nvec[current_time-1]/K)
  }
  return(Nvec)
}

dlog_out <- dlogistic(rd = 3, t = 1000, K = K)
dlog_df <- data.frame(Nt = dlog_out,
                      time = 0:1000)
ggplot(dlog_df) +
  geom_line(aes(x = time, y = Nt), size = .2) +
  geom_point(aes(x = time, y = Nt), size = 1.5, shape = 21,
             fill = "white", stroke = .8 ) +
  ecoevoapps::theme_apps()

ggplot(dlog_df) +
  geom_line(aes(x = lag(Nt), y = Nt), size = .2) +
  geom_point(aes(x = lag(Nt), y = Nt), size = 1.5, shape = 21,
             fill = "white", stroke = .8 ) +
  ecoevoapps::theme_apps()

# some new stuff
# some more new stuff