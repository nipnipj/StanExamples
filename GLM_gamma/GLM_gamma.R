library(tidyverse)
theme_set(theme_light())

library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
load("~/dataR/wafer.rda")
data <- wafer %>% as_tibble() %>% 
  mutate(x1 = as.numeric(x1))

y <- data$resist
x <- model.matrix( ~ -1 + x1, data = data)

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_gamma/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
fit$print(c("shape"))

