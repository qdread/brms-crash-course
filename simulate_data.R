# Create sample dataset

# L1: plot, L2: field (in popularity data, it was pupil and class)
# Y: yield (in popularity data, it was popularity)
# Level 1 predictor, continuous: soil N (was extraversion)
# Level 1 predictor, categorical: variety (was sex)
# Level 2 predictor, continuous: rainfall (was t_exp)

# We want to have positive coefficients on soil N, variety, and rainfall
# We want to simulate from different random intercepts and slopes for soil N
# We want to simulate from different random intercepts but not random slopes for variety
# We want to simulate from different random slopes for rainfall

# Generate rainfall from a normal, and soil N from a normal as well
library(simr)
library(tidyverse)
library(mvtnorm)

# popular2data <- read_csv('https://github.com/qdread/brms-crash-course/raw/main/data/popular2data.csv')
# 
# # variance covariance matrix
# vc <- matrix(c(1.1, -.9, -.9, .2), 2)
# 
# fakelmer <- makeLmer(formula = popular ~ 1 + sex + extrav + texp + (1 + extrav | class), fixef = c(.75, 1.25, 0.5, 0.1), VarCorr = vc,  sigma = .75, data = popular2data %>% select(class,extrav,sex,texp))


# Generate new fake data
set.seed(1035)

# 20 fields, 20 measurements per field per plant
# Also include an interaction term between soil N and cultivar, and soil N and rainfall
yield_data <- expand_grid(field = 1:20, plot = 1:20, variety = 0:1)

# Generate the predictor variable data
pred_vars <- rmvnorm(n=20, mean = c(25, 10), sigma = matrix(c(5, .2, .2, 1), 2))
pred_dt <- cbind(tibble(field = 1:20), pred_vars) %>% set_names(c('field','rainfall','soilN_mean'))

yield_data <- left_join(yield_data,pred_dt) %>%
  mutate(rainfall = round(rainfall),
         soilN = round(rnorm(n=length(soilN_mean), mean = soilN_mean, sd = 2)),
         variety = factor(variety),
         field = factor(field))

fakelmer <- makeLmer(formula = yield ~ 1 + variety + soilN + rainfall + variety:soilN + soilN:rainfall + (1 + soilN | field), fixef = c(15, 2.25, 1.5, 1, 0, 0.5), VarCorr = vc,  sigma = .75, data = yield_data)

yield_data <- getData(fakelmer) %>%
  select(-soilN_mean) %>%
  group_by(field) %>%
  mutate(plot = 1:n()) %>%
  ungroup() %>%
  mutate(yield=yield/40, soilN = soilN/2)

# Test brm models
library(brms)
options(brms.backend = 'cmdstanr', mc.cores = 4)

fit1 <- brm(yield ~ 1 + (1 | field),
            data = yield_data,
            chains = 4,
            iter = 2000,
            warmup = 1000)

performance::variance_decomposition(fit1)

fit2 <- brm(yield ~ 1 + variety + soilN + (1 | field),
            data = yield_data,
            chains = 4,
            iter = 2000,
            warmup = 1000)

fit2b <- brm(yield ~ 1 + variety + soilN + (1 | field),
            data = yield_data,
            prior = c(prior(normal(0,5), class = b)),
            chains = 4,
            iter = 2000,
            warmup = 1000)

fit3 <- brm(yield ~ 1 + variety + soilN + rainfall + (1 | field),
             data = yield_data,
             prior = c(prior(normal(0,5), class = b)),
             chains = 4,
             iter = 2000,
             warmup = 1000)

fit4 <- brm(yield ~ 1 + variety + soilN + rainfall + (1 + soilN + variety | field),
            data = yield_data,
            prior = c(prior(normal(0,5), class = b)),
            chains = 4,
            iter = 2000,
            warmup = 1000)

fit5 <- brm(yield ~ 1 + variety + soilN + rainfall + (1 + soilN | field),
            data = yield_data,
            prior = c(prior(normal(0,5), class = b)),
            chains = 4,
            iter = 2000,
            warmup = 1000)

fit6 <- brm(yield ~ 1 + variety + soilN + rainfall + variety:soilN + soilN:rainfall + (1 + soilN | field),
            data = yield_data,
            prior = c(prior(normal(0,5), class = b)),
            chains = 4,
            iter = 2000,
            warmup = 1000)

fit1 <- add_criterion(fit1, 'loo')
fit2 <- add_criterion(fit2, 'loo')
fit3 <- add_criterion(fit3, 'loo')
fit4 <- add_criterion(fit4, 'loo')
fit5 <- add_criterion(fit5, 'loo')
fit6 <- add_criterion(fit6, 'loo')

loo_compare(fit1,fit2,fit3,fit4,fit5,fit6)