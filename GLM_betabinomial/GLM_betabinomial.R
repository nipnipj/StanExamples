
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
load("~/dataR/cbpp.rda")
data <- cbpp %>% as_tibble() %>% 
  mutate(herd = factor(herd))

y <- data$incidence
trials <- data$size
x <- model.matrix( ~ herd, data = data)[,-1]

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x,
  trials = trials
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_betabinomial/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
fit$print(c("phi"))
