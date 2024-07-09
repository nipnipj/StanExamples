
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
load("~/dataR/GasolineYield.rda")
data <- GasolineYield 

y <- data$yield
x <- data %>% 
  select(yield, temp) %>% 
  modelr::model_matrix(yield ~ temp) %>% 
  select(-1) %>% 
  as.matrix()

##
data <- tibble(y = c(rep(0,50), runif(50), rep(1,50)), x = rnorm(150))
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
mod <- cmdstan_model("~/dtSTAN/GLM_ZOIbeta/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"), digits = 5)
fit$print(c("Intercept_phi", "beta_phi"), digits = 5)
fit$print(c("Intercept_zoi", "beta_zoi"), digits = 5)
fit$print(c("Intercept_coi", "beta_coi"), digits = 5)
