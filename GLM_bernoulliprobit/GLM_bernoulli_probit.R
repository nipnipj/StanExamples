
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- read.table("~/dataR/turnout.tab", header = T, sep = "", dec = ".") %>% 
  as_tibble() %>% 
  mutate(race = factor(race))

y <- data$vote
x <- model.matrix( ~ educate+income+race, data = data)[,-1] #needed when using dummies

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_bernoulli_probit/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
