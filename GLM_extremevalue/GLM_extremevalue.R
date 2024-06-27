
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- tibble(
  y = c(
    8.5, 8.9, 9.1, 8.9, 8.4, 9.7, 9.1, 9.6, 8.7, 9.3,
    9.6, 9.3, 8.7, 9.0, 8.8, 8.9, 8.9, 12.2, 7.8, 7.7,
    8.3, 8.1, 7.3, 6.8, 6.7, 7.3, 7.6, 8.2, 8.6, 9.8,
    9.5, 7.4, 7.3, 10.2, 10.3, 10.4, 8.8, 9.7, 10.0, 10.8,
    11.1, 12.7, 11.5, 11.8, 12.6, 13.0, 10.5, 10.5, 10.0, 9.4
  ),
  x = seq(1955,2004,1)
)
y <- data$y
x <- model.matrix( ~ -1 + x, data = data)

data <- tibble(
  y = c(6,3,3,3,5,5,4,3,5,5,4,3,4,4,6,5,5,4,5,2,6,4,6,5,3,3,8,3,
        4,4,6,6,6,6,6,5,6,6,5,5),
  x = 1:40
)
y <- data$y
x <- model.matrix( ~ -1 + x, data = data)

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_extremevalue/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta", "sigma", "xi"))
fit$print(c("alpha")) 


