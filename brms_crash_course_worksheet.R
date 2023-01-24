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

popular2data <- read_csv('https://github.com/qdread/brms-crash-course/raw/main/data/popular2data.csv')

glimpse(...)

popular2data <- mutate(popular2data, ... = ...(sex, labels = c('boy', 'girl')))

# Exploratory plots (full code provided)

(pop_vs_ext <- ggplot(data = popular2data, aes(x = extrav, y = popular)) +
  geom_point(size = 1.2,
             alpha = .8,
             position = position_jitter(width = .2, height = 0)) +
  ggtitle('Popularity vs. extraversion'))

pop_vs_ext +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle('Popularity vs. extraversion', subtitle = 'overall trendline')

(pop_vs_ext_colored <- ggplot(data = popular2data, aes(x = extrav, y = popular, color = class, group = class)) +
   geom_point(size = 1.2,
              alpha = .8,
              position = position_jitter(width = .2, height = 0)) +
   theme(legend.position = 'none') +
   scale_color_distiller(palette = 'Set1') +
   ggtitle('Popularity vs. extraversion', 'points colored by class'))

pop_vs_ext_colored + 
  geom_smooth(method = lm, se = FALSE, linewidth = 0.7, alpha = 0.8) +
  ggtitle('Popularity vs. extraversion', 'trendline by class')

popular2data %>%
  group_by(class) %>%
  mutate(slope = lm(popular ~ extrav)$coefficients[2]) %>%
  ungroup %>%
  mutate(high_or_low = case_when(
    slope >= sort(unique(slope))[98] ~ 'high',
    slope <= sort(unique(slope))[3] ~ 'low',
    TRUE ~ 'mid'
  )) %>%
  ggplot(aes(x = extrav, y = popular, group = class)) +
  geom_point(size = 1.2,
              alpha = .8,
              position = position_jitter(width = .2, height = 0)) +
  geom_smooth(aes(color = high_or_low, size = high_or_low), 
              method = lm, se = FALSE) +
  theme(legend.position = 'none') +
  scale_color_manual(values = c(high = 'blue', low = 'red', mid = 'gray50')) +
  scale_size_manual(values = c(high = 1.2, low = 1.2, mid = 0.6)) +
  ggtitle('Popularity vs. extraversion', 'highlighting 6 classes with most extreme relationship')

# Model with random intercepts only

fit_interceptonly <- ...(popular ~ ... + ...,
                         ... = popular2data,
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

fit_fixedslopes <- brm(... + (1 | class),  
                       data = popular2data, 
                       chains = 4, iter = 2000, warmup = 1000,
                       ... = 1400, ... = 'fit_fixedslopes')

summary(fit_fixedslopes)
prior_summary(...)

# Same model with modified priors

fit_fixedslopes_priors <- brm(popular ~ 1 + sex + extrav + (1 | class),  
                       data = popular2data, 
                       ... = c(
                         ...(normal(...), class = ...)
                       ),
                       chains = 4, iter = 2000, warmup = 1000,
                       seed = 1450, file = 'fit_fixedslopes_priors')

summary(fit_fixedslopes_priors)
pp_check(...)

posterior_slopes <- ...(fit_fixedslopes_priors, b_sex, ...)

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

fit_fixed12 <- brm(popular ~ 1 + ... + (1 | class),  
                   data = popular2data, 
                   prior = c(
                     prior(normal(0, 5), class = b)
                   ),
                   chains = 4, iter = 2000, warmup = 1000,
                   seed = 703, file = 'fit_fixed12')

summary(fit_fixed12)

# Model with random intercepts and random slopes for the first-level predictors

fit_randomslopes <- brm(popular ~ ... + (... | ...),  
                        data = popular2data, 
                        prior = c(
                          prior(normal(0, 5), class = b)
                        ),
                        chains = 4, iter = 2000, warmup = 1000,
                        seed = 709, file = 'fit_randomslopes')

summary(fit_randomslopes)

# Examine random slopes for sex at the class level

sex_slopes <- ...(fit_randomslopes, b_sexgirl, r_class[class,variable]) %>%
  filter(variable == 'sexgirl') %>%
  mutate(slope = ...)

sex_slopes %>% 
  median_qi(slope, .width = c(.66, .90, .95)) %>%
  ggplot(aes(y = class, x = slope, xmin = .lower, xmax = .upper)) +
  geom_interval() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Blues')


# Model with the random slope for sex removed and only the random slope for extraversion remaining

fit_extravrandomslope <- brm(popular ~ 1 + sex + extrav + texp + (... | class),  
                             data = popular2data, 
                             prior = c(
                               prior(normal(0, 5), class = b)
                             ),
                             chains = 4, iter = 2000, warmup = 1000,
                             seed = 206, file = 'fit_extravrandomslope')

summary(fit_extravrandomslope)

# Model comparison by leave-one-out cross-validation

fit_interceptonly_moresamples <- ...(fit_interceptonly_moresamples, ...)
fit_fixedslopes_priors <- add_criterion(fit_fixedslopes_priors, 'loo')
fit_fixed12 <- add_criterion(fit_fixed12, 'loo')
fit_randomslopes <- add_criterion(fit_randomslopes, 'loo')
fit_extravrandomslope <- add_criterion(fit_extravrandomslope, 'loo')

...(fit_interceptonly_moresamples, fit_fixedslopes_priors, fit_fixed12, fit_randomslopes, fit_extravrandomslope)

# Compute Bayes factor for each parameter in the final random slope model

...(fit_extravrandomslope)

# Make plots of model predictions

...(fit_extravrandomslope)

# Complete code is provided for the rest of the plots.

sex_emmeans <- emmeans(fit_extravrandomslope, ~ sex)

gather_emmeans_draws(sex_emmeans) %>%
  ggplot(aes(x = .value, y = sex)) +
  stat_interval(.width = c(.66, .95, .99)) +
  stat_summary(fun = median, geom = 'point', size = 2) +
  scale_color_brewer(palette = 'Greens') +
  ggtitle('posterior expected value by sex', 'averaged across extraversion and teacher experience')

expand_grid(texp = mean(popular2data$texp),
            extrav = seq(1, 10, by = .5),
            sex = c('boy', 'girl')) %>%
  add_epred_draws(fit_extravrandomslope, re_formula = ~ 0) %>%
  ggplot(aes(x = extrav, y = .epred, group = sex, color = sex)) +
  stat_lineribbon(.width = c(.66, .95, .99)) +
  scale_fill_grey() +
  scale_color_manual(values = c(boy = 'blue', girl = 'pink')) +
  labs(y = 'posterior expected value of popularity') +
  ggtitle('posterior expected value by extraversion + sex', 'at average value of teacher experience')

expand_grid(texp = mean(popular2data$texp),
            extrav = seq(1, 10, by = .5),
            sex = c('boy', 'girl'),
            class = 1:10) %>%
  add_epred_draws(fit_extravrandomslope, re_formula = ~ (1 + extrav|class)) %>%
  ggplot(aes(x = extrav, y = .epred, group = class, color = sex)) +
  facet_wrap(~ sex) +
  stat_lineribbon(.width = c(.95), alpha = 1/4) +
  scale_color_manual(values = c(boy = 'blue', girl = 'pink')) +
  labs(y = 'posterior expected value of popularity') +
  ggtitle('class-level predictions of popularity vs. extraversion by sex', 'at average value of teacher experience')

