
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- readr::read_csv(file="~/dataR/fish.csv")

y <- data$count
x <- model.matrix( ~ -1 + zg, data = data)

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_poisson/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
fit$print(c("yrep"))
