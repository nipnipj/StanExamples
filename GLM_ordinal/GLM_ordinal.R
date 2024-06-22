
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
data <- read.table(file="~/dataR/crab.txt", header=TRUE,dec = ".") %>% 
  as_tibble() %>% 
  mutate(
    color = ordered(
      color,
      labels=c("Oscuro","Gris","Gris claro","Claro"),
      levels=c("2","3","4","5"))
  )

y <- data$color %>% unclass
nthres <- (y %>% unique() %>% length())-1
x <- model.matrix( ~ -1 + width, data = data)

#####
dt <- list(
  N1=length(y),
  D1=dim(x)[2],
  y=y,
  nthres=nthres,
  x1=x
)

############
mod <- cmdstan_model("~/dtSTAN/GLM_ordinal/model.stan")

############
fit <- mod$sample(data = dt, chains = 2, parallel_chains = 2, adapt_delta = 0.9, 
                  iter_warmup = 1000,  iter_sampling = 500, max_treedepth = 10)
fit$print(c("Intercept", "beta"))
