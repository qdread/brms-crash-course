############################################################################################
#### BAYESIAN MIXED MODELS WITH BRMS                                                    ####
#### ===============================                                                    ####
#### This worksheet contains an incomplete version of the code presented in the lesson. ####
#### Fill in all ... with code.                                                         ####
############################################################################################
############################################################################################

# Load packages and set options

library(brms)
library(tidyverse)
library(emmeans)
library(tidybayes)
library(easystats)
library(logspline)

theme_set(theme_minimal())

...(brms.backend = 'cmdstanr', mc.cores = 4)

yield_data <- read_csv('https://github.com/qdread/brms-crash-course/raw/main/data/yield_data.csv')

glimpse(...)

# Exploratory plots (full code provided)

(yield_vs_N <- ggplot(data = yield_data, aes(x = soilN, y = yield)) +
    geom_point(size = 1.2,
               alpha = .8,
               position = position_jitter(width = .2, height = 0)) +
    ggtitle('Yield vs. soil N'))

yield_vs_N +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle('Yield vs. soil N', subtitle = 'overall trendline')

(yield_vs_N_colored <- ggplot(data = yield_data, aes(x = soilN, y = yield, color = field, group = field)) +
    geom_point(size = 1.2,
               alpha = .8,
               position = position_jitter(width = .2, height = 0)) +
    theme(legend.position = 'none') +
    ggtitle('Yield vs. soil N', 'points colored by field'))

yield_vs_N_colored + 
  geom_smooth(method = lm, se = FALSE, linewidth = 0.7, alpha = 0.8) +
  ggtitle('Yield vs. soil N', 'trendline by field')

# Model with random intercepts only

fit_interceptonly <- ...(yield ~ ... + ...,
                         ... = yield_data,
                         chains = ...,
                         iter = ...,
                         warmup = ...,
                         ... = 'random')

summary(...)
plot(...)

fit_interceptonly_moresamples <- ...(fit_interceptonly, chains = 2, iter = ..., warmup = ...)

...(fit_interceptonly_moresamples)
summary(fit_interceptonly_moresamples)

post_samples <- ...(fit_interceptonly_moresamples)
post_samples_intercept <- post_samples$...

...(post_samples_intercept)
...(post_samples_intercept, c(0.05, 0.95))

variance_decomposition(..., ci = ...)

# Model with random intercepts and fixed slopes

fit_fixedslopes <- brm(... + (1 | field),  
                       data = yield_data, 
                       chains = 4, iter = 2000, warmup = 1000,
                       ... = 807, ... = 'fit_fixedslopes')

summary(fit_fixedslopes)
prior_summary(...)

# Same model with modified priors

fit_fixedslopes_priors <- brm(yield ~ 1 + variety + soilN + (1 | field),
                              data = yield_data, 
                              ... = c(
                                ...(normal(...), class = ...)
                              ),
                              chains = 4, iter = 2000, warmup = 1000,
                              seed = 811, file = 'fit_fixedslopes_priors')

summary(fit_fixedslopes_priors)
pp_check(...)

posterior_slopes <- ...(fit_fixedslopes_priors, b_varietytall, ...)

posterior_slopes %>%
  ...(.width = c(.66, .95, .99))

ggplot(..., aes(y = .variable, x = .value)) +
  stat_halfeye(.width = c(.8, .95)) +
  geom_vline(xintercept = 0, linetype = 'dashed', linewidth = 1)

ggplot(..., aes(y = .variable, x = .value)) +
  stat_interval() +
  stat_summary(fun = median, geom = 'point', size = 2) +
  scale_color_brewer(palette = 'Blues') +
  geom_vline(xintercept = 0, linetype = 'dashed', linewidth = 1)


# Model with random intercepts and fixed slopes for both first-level and second-level predictors

fit_fixed12 <- brm(yield ~ 1 + ... + (1 | field),  
                   data = yield_data, 
                   prior = c(
                     prior(normal(0, 5), class = b)
                   ),
                   chains = 4, iter = 2000, warmup = 1000,
                   seed = 703, file = 'fit_fixed12')

summary(fit_fixed12)

# Model with random intercepts and random slopes for the first-level predictors

fit_randomslopes <- brm(yield ~ ... + (... | ...),  
                        data = yield_data, 
                        prior = c(
                          prior(normal(0, 5), class = b)
                        ),
                        chains = 4, iter = 2000, warmup = 1000,
                        seed = 777, file = 'fit_randomslopes')

summary(fit_randomslopes)

# Examine random slopes for variety at the field level

variety_slopes <- ...(fit_randomslopes, b_varietytall, r_field[field,variable]) %>%
  filter(variable == 'varietytall') %>%
  mutate(slope = ...)

variety_slopes %>% 
  median_qi(slope, .width = c(.66, .90, .95)) %>%
  ggplot(aes(y = field, x = slope, xmin = .lower, xmax = .upper)) +
  geom_interval() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Blues')

# Model with the random slope for variety removed and only the random slope for soil N remaining

fit_soilNrandomslope <- brm(yield ~ 1 + variety + soilN + rainfall + (... | field),  
                            data = yield_data, 
                            prior = c(
                              prior(normal(0, 5), class = b)
                            ),
                            chains = 4, iter = 2000, warmup = 1000,
                            seed = 888, file = 'fit_soilNrandomslope')

summary(fit_soilNrandomslope)

# Model comparison by leave-one-out cross-validation

fit_interceptonly_moresamples <- ...(fit_interceptonly_moresamples, ...)
fit_fixedslopes_priors <- add_criterion(fit_fixedslopes_priors, 'loo')
fit_fixed12 <- add_criterion(fit_fixed12, 'loo')
fit_randomslopes <- add_criterion(fit_randomslopes, 'loo')
fit_soilNrandomslope <- add_criterion(fit_soilNrandomslope, 'loo')

...(fit_interceptonly_moresamples, fit_fixedslopes_priors, fit_fixed12, fit_randomslopes, fit_soilNrandomslope)

# Compute Bayes factor for each parameter in the final random slope model

...(fit_soilNrandomslope)

# Make plots of model predictions

...(fit_soilNrandomslope)

# Complete code is provided for the rest of the plots.

plot(conditional_effects(fit_soilNrandomslope, effects="soilN:field", re_formula = NULL), 
     line_args=list(linewidth=1.2, alpha = 0.2), theme = theme(legend.position = 'none'))

variety_emmeans <- emmeans(fit_soilNrandomslope, ~ variety)

gather_emmeans_draws(variety_emmeans) %>%
  ggplot(aes(x = .value, y = variety)) +
  stat_interval(.width = c(.66, .95, .99)) +
  stat_summary(fun = median, geom = 'point', size = 2) +
  scale_color_brewer(palette = 'Greens') +
  ggtitle('posterior expected value by variety', 'averaged across soil N and rainfall')

expand_grid(rainfall = mean(yield_data$rainfall),
            soilN = seq(2, 9, by = .5),
            variety = c('short', 'tall')) %>%
  add_epred_draws(fit_soilNrandomslope, re_formula = ~ 0) %>%
  ggplot(aes(x = soilN, y = .epred, group = variety, color = variety)) +
  stat_lineribbon(.width = c(.66, .95, .99)) +
  scale_fill_grey() +
  labs(y = 'posterior expected value of yield') +
  ggtitle('posterior expected value by soil N + variety', 'at average value of rainfall')
