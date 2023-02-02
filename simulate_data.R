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

# Get the main fixed effect, interaction fixed effect, and random effect VCV matrix from the popularity data
# This can be used to reverse engineer some new data

popular2data <- read_csv('https://github.com/qdread/brms-crash-course/raw/main/data/popular2data.csv') %>% mutate(sex=factor(sex))

pop_lmer <- lmer(popular ~ 1 + sex + extrav + texp + sex:extrav + sex:texp + texp:extrav + (1 + sex + extrav | class), data = popular2data)
pop_fixef <- fixef(pop_lmer)
pop_vcv <- VarCorr(pop_lmer)

pop_fakelmer <- makeLmer(popular ~ 1 + sex + extrav + texp + sex:extrav + sex:texp + texp:extrav + (1 + sex + extrav | class), fixef = pop_fixef, VarCorr = pop_vcv, sigma = 0.75, data = popular2data %>% select(class, extrav, sex, texp))

# Correlations among predictor variables
classmeans <- popular2data %>%
  group_by(class) %>%
  summarize( sd_extrav=sd(extrav), texp=mean(texp),extrav=mean(extrav))
classmeans %>% summarize(texp=mean(texp), extrav=mean(extrav))
class_sigma <- cov(classmeans[, c('texp','extrav')]) # Negative correlation
popular2data %>% group_by(sex) %>% summarize(extrav=mean(extrav)) # Sex means are about the same

# 
# # variance covariance matrix
# vc <- matrix(c(1.1, -.9, -.9, .2), 2)
# 
# fakelmer <- makeLmer(formula = popular ~ 1 + sex + extrav + texp + (1 + extrav | class), fixef = c(.75, 1.25, 0.5, 0.1), VarCorr = vc,  sigma = .75, data = popular2data %>% select(class,extrav,sex,texp))


# Generate new fake data
set.seed(936)

# 20 fields, 20 measurements per field per plant
# Also include an interaction term between soil N and cultivar, and soil N and rainfall
n_field <- 25
plot_per_field <- 20
yield_data_in <- expand_grid(field = 1:n_field, plot = 1:plot_per_field, variety = 0:1)

# Generate the predictor variable data
#pred_vars <- rmvnorm(n=n_field, mean = c(14, 5), sigma = class_sigma)
# Break correlation between the predictors.
rainfall_values <- round(runif(n_field, 2, 25))
soilN_means <- rlnorm(n_field, meanlog = log(5), sdlog = .3)
pred_dt <- cbind(tibble(field = 1:n_field), pred_vars) %>% set_names(c('field','rainfall','soilN_mean'))

yield_data_in <- left_join(yield_data_in,pred_dt) %>%
  mutate(rainfall = round(rainfall),
         soilN = ceiling(rnorm(n=length(soilN_mean), mean = soilN_mean, sd = 1)),
         variety = factor(variety),
         field = factor(field))

yield_data_in %>% ungroup %>% summarize(across(where(is.numeric),range))

# variance covariance matrix
#vc <- matrix(c(1.1, -.7, -.7, .5), 2)
#vc <- matrix(c(.25, -.04, -.04, .1), 2)
vc <- matrix(c(.5, -.02, -.03, -.02, .004, -.002, -.03, -.002, .006), nrow = 3, byrow = TRUE)

fakelmer <- makeLmer(formula = yield ~ 1 + variety + soilN + rainfall + variety:soilN + variety:rainfall + soilN:rainfall + (1 + variety + soilN | field), fixef = c(-1, 1, 0.8, 0.25, 0.05, 0.001, -0.02), VarCorr = vc,  sigma = .75, data = yield_data_in)

yield_data <- getData(fakelmer) %>%
  select(-soilN_mean) %>%
  group_by(field) %>%
  mutate(plot = 1:n()) #%>%
  #ungroup() %>%
  #mutate(yield=yield/10, soilN = soilN/2)

yield_data %>% ungroup %>% summarize(across(where(is.numeric),range))

# Test brm models
library(brms)
options(brms.backend = 'cmdstanr', mc.cores = 4)

fit1few <- brm(yield ~ 1 + (1 | field),
               data = yield_data,
               chains = 2,
               iter = 200,
               warmup = 100,
               init = 'random')

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
            iter = 4000,
            warmup = 3000,
            seed = 777)

fit5 <- brm(yield ~ 1 + variety + soilN + rainfall + (1 + soilN | field),
            data = yield_data,
            prior = c(prior(normal(0,5), class = b)),
            chains = 4,
            iter = 4000,
            warmup = 3000,
            seed = 888)

fit6 <- brm(yield ~ 1 + variety + soilN + rainfall + variety:soilN + soilN:rainfall + (1 + soilN | field),
            data = yield_data,
            prior = c(prior(normal(0,5), class = b)),
            chains = 4,
            iter = 4000,
            warmup = 3000,
            seed = 999)

fit1 <- add_criterion(fit1, 'loo')
fit2b <- add_criterion(fit2b, 'loo')
fit3 <- add_criterion(fit3, 'loo')
fit4 <- add_criterion(fit4, 'loo')
fit5 <- add_criterion(fit5, 'loo')
fit6 <- add_criterion(fit6, 'loo')

loo_compare(fit1,fit2b,fit3,fit4,fit5,fit6)

# edit data to have text labels on the factor variables
yield_data <- yield_data %>%
  mutate(field = factor(field, levels = 1:25, labels = paste0('field', 1:25)),
         plot = paste0('plot', plot),
         variety = factor(variety, levels = 0:1, labels = c('standard', 'high-yield')))

write_csv(yield_data,file = 'data/yield_data.csv')

# edit again: we want to have alphabetical order already, as well as no hyphen in the factor name

yield_data <- read_csv('data/yield_data.csv')

yield_data <- yield_data %>%
  mutate(yield = round(yield, 3),
         variety = factor(variety, levels = c('standard', 'high-yield'), labels = c('short', 'tall')))

write_csv(yield_data,file = 'data/yield_data.csv')
