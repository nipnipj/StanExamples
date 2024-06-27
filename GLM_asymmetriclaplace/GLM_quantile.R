
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- readr::read_csv("~/dataR/radon.csv")
y <- data$log_radon
x <- model.matrix( ~ -1 + pcterr, data = data)

data <- tibble(
  y = rexp(1000, 0.8),
  x = rnorm(1000)
)
y <- data$y
x <- model.matrix( ~ -1 + x, data = data)


#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  quantile = 0.99,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_quantile/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"), digits = 6)
fit$print(c("sigma"))
