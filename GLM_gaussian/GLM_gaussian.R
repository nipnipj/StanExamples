
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- readr::read_csv("~/dataR/radon.csv")

y <- data$log_radon
x <- model.matrix( ~ -1 + floor + pcterr, data = data)

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x
)

############
mod <- cmdstan_model("~/Desktop/GLM_gaussian/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
fit$print(c("sigma"))
