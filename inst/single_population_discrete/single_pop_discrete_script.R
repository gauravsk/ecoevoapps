library(tidyverse)
library(ecoevoapps)
theme_set(ecoevoapps::theme_apps())
# Script (entirely non-Shiny) to run a discrete-time
# single population model (geometric or not)

user_time <- 100
user_N0 <- 10
user_lambda <- 1.1
user_K <- 500
user_rd <- .1

exponential_pop_traj <- ecoevoapps::discrete_exponential(N0 = user_N0, lambda = user_lambda,
                                                         time = user_time)
exponential_pop_df <- data.frame(time = 0:time,
                                 N_t= exponential_pop_traj,
                                 popdiff = c(NA, diff(exponential_pop_traj)),
                                 Ntm1 = c(lag(exponential_pop_traj)),
                                 popratio = exponential_pop_traj/lag(exponential_pop_traj))
(trajaectory_plot <- ggplot(data.frame(exponential_pop_df)) +
  geom_line(aes(y = N_t, x = time), size = .2) +
    geom_point(aes(y = N_t, x = time), size = .2, shape = 21,
               fill = "white", stroke = .8) +
    ylab(latex2exp::TeX("Population size at time $t$ ($N_t$)")))

(nt_over_ntm1 <- ggplot(data.frame(exponential_pop_df)) +
    geom_line(aes(y = N_t, x = Ntm1), size = .2) +
    geom_point(aes(y = N_t, x = Ntm1), size = 1.5, shape = 21,
               fill = "white", stroke = .8) +
    ylab(latex2exp::TeX("Population size at time $t$ ($N_t$)")) +
    xlab(latex2exp::TeX("Population size at time t-1 ($N_{t-1}$)")))

(nt_minus_ntm1 <- ggplot(data.frame(exponential_pop_df)) +
    geom_line(aes(y = popdiff, x = time), size = .2) +
    geom_point(aes(y = popdiff, x = time), size = 1.5, shape = 21,
               fill = "white", stroke = .8) +
    ylab("Population size (N_t)"))


user_time <- 100
user_rd <- 2
dlog_out <- ecoevoapps::discrete_logistic(N0 = user_N0, rd = user_rd,
                                          time = user_time, K = user_K)
dlog_df <- data.frame(Nt = dlog_out,
                      time = 1:user_time)
ggplot(dlog_df) +
  geom_line(aes(x = time, y = Nt), size = .2) +
  geom_point(aes(x = time, y = Nt), size = 1.5, shape = 21,
             fill = "white", stroke = .8 )

ggplot(dlog_df) +
  geom_line(aes(x = lag(Nt), y = Nt), size = .2) +
  geom_point(aes(x = lag(Nt), y = Nt), size = 1.5, shape = 21,
             fill = "white", stroke = .8 )
