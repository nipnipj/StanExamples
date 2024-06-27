
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
library(circular)
data <- tibble(
  x = circular(runif(50, 0, 2*pi)) %>% as.numeric(),
  y = ((atan2(0.15*cos(x) + 0.25*sin(x), 0.35*sin(x)) + 
    rvonmises(n=50, mu=circular(0), kappa=5))/3) %>% as.numeric()
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
mod <- cmdstan_model("~/dtSTAN/GLM_vonmises/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))

