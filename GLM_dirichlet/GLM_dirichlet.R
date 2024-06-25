
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
N <- 15
set.seed(2)
data <- data.frame(
  y1 = rbinom(N, 10, 0.5), y2 = rbinom(N, 10, 0.7), 
  y3 = rbinom(N, 10, 0.9), x = rnorm(N)
) %>%
  mutate(
    trials = y1 + y2 + y3,
    y1 = y1 / trials,
    y2 = y2 / trials,
    y3 = y3 / trials
  )

y <- data %>% select(starts_with("y")) %>% as.matrix()
ncat = dim(y)[2]
trials <- data$trials
x <- model.matrix( ~ -1 + x, data = data)

#####
dt <- list(
  N1=dim(y)[1],
  D1=dim(x)[2],
  y=y,
  ncat = ncat,
  x1=x,
  trials = trials
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_dirichlet/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept2", "beta2"))
fit$print(c("Intercept3", "beta3"))

