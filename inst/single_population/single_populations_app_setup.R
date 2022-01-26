knitr::opts_chunk$set(echo = TRUE)
library(tidyr)
library(ggplot2)
library(purrr)
library(deSolve)
library(ecoevoapps)
library(patchwork)
library(knitr)
library(kableExtra)

dndt <- function(params) {
  r <- params["r"]
  if(!is.na(params["K"])) {
    K <- params["K"]
    (1:K)*r*(1-(1:K)/K)
  } else {
    r*1:10000
  }
}

dnNdt <- function(params) {
  r <- params["r"]
  if(!is.na(params["K"])) {
    K <- params["K"]
    r*(1-seq(1:K)/K)
  } else {
    rep(r,10000)
  }
}
