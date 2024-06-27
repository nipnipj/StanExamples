
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- readr::read_csv("~/dataR/vdem_r.csv")

y <- data$prop_fem
x <- data %>% 
  modelr::model_matrix(prop_fem ~ .) %>% 
  select(-1) %>% 
  as.matrix()

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_beta/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
fit$print(c("Intercept_phi", "beta_phi"))
fit$print(c("phi"))
