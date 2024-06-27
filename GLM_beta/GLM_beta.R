
library(cmdstanr)
set_cmdstan_path("~/CMDSTAN")

############
load("~/dataR/vdem.RData")
vdem <- 
vdem_clean <- vdem %>% 
  select(country_name, country_text_id, year, region = e_regionpol_6C,
         polyarchy = v2x_polyarchy, corruption = v2x_corr, 
         civil_liberties = v2x_civlib, prop_fem = v2lgfemleg, v2lgqugen) %>% 
  filter(year >= 2010, year < 2020) %>% 
  drop_na(v2lgqugen, prop_fem) %>% 
  mutate(quota = v2lgqugen > 0,
         prop_fem = prop_fem / 100,
         polyarchy = polyarchy * 100)

vdem_2015 <- vdem_clean %>% 
  filter(year == 2015) %>% 
  mutate(polyarchy_noise = polyarchy + rnorm(n(), 0, sd = 0.01)) %>% 
  mutate(highlight = polyarchy_noise == max(polyarchy_noise) | 
           polyarchy_noise == min(polyarchy_noise)) %>% 
  select(-polyarchy_noise)

vdem_2015_fake0 <- vdem_2015 %>% 
  mutate(prop_fem = ifelse(prop_fem == 0, 0.001, prop_fem))

rec <- vdem_2015_fake0 %>% 
  recipe(prop_fem ~ quota) %>%
  step_mutate(quota = factor(quota)) %>% 
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
  prep()

data <- rec %>% bake(new_data = vdem_2015_fake0)

y <- data$prop_fem
x <- data %>% 
  select(prop_fem, starts_with("quota_")) %>% 
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
