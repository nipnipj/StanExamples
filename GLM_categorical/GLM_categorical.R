
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- readxl::read_excel("~/dataR/hsd.xlsx") %>% 
  mutate(ses = factor(ses))

y <- data$ses
ncat = length(unique(y))
x <- model.matrix( ~ -1 + write, data = data)

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  ncat = ncat,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_categorical/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept2", "beta2"))
fit$print(c("Intercept3", "beta3"))
