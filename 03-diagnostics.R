library(cmdstanr)
library(tidyverse)

fit <- readRDS("model/fit.rds")

fit$cmdstan_diagnose()
