
## ----load packages--------------------------------------------------------------------------------------------------------------------------------------
library(brms)
library(tidyverse)
library(emmeans)
library(tidybayes)
library(easystats)


## ----ggplot theme---------------------------------------------------------------------------------------------------------------------------------------
theme_set(theme_minimal())


## -------------------------------------------------------------------------------------------------------------------------------------------------------
options(brms.backend = 'cmdstanr', mc.cores = 4)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
popular2data <- read_csv('https://github.com/qdread/brms-crash-course/raw/main/data/popular2data.csv')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(popular2data)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
popular2data <- mutate(popular2data, sex = factor(sex, labels = c('boy', 'girl')))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
(pop_vs_ext <- ggplot(data = popular2data, aes(x = extrav, y = popular)) +
  geom_point(size = 1.2,
             alpha = .8,
             position = position_jitter(width = .2, height = 0)) +
  ggtitle('Popularity vs. extraversion'))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
pop_vs_ext +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle('Popularity vs. extraversion', subtitle = 'overall trendline')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
(pop_vs_ext_colored <- ggplot(data = popular2data, aes(x = extrav, y = popular, color = class, group = class)) +
   geom_point(size = 1.2,
              alpha = .8,
              position = position_jitter(width = .2, height = 0)) +
   theme(legend.position = 'none') +
   scale_color_distiller(palette = 'Set1') +
   ggtitle('Popularity vs. extraversion', 'points colored by class'))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
pop_vs_ext_colored + 
  geom_smooth(method = lm, se = FALSE, linewidth = 0.7, alpha = 0.8) +
  ggtitle('Popularity vs. extraversion', 'trendline by class')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---- eval = FALSE--------------------------------------------------------------------------------------------------------------------------------------
## lmer(popular ~ 1 + (1 | class), data = popular2data)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_interceptonly <- brm(popular ~ 1 + (1 | class),
                         data = popular2data,
                         chains = 2,
                         iter = 200,
                         warmup = 100,
                         init = 'random')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit_interceptonly)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
plot(fit_interceptonly)


## ---- results = 'hide'----------------------------------------------------------------------------------------------------------------------------------
fit_interceptonly_moresamples <- update(fit_interceptonly, chains = 2, iter = 2000, warmup = 1000)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
plot(fit_interceptonly_moresamples)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit_interceptonly_moresamples)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
post_samples <- as_draws_df(fit_interceptonly_moresamples)
post_samples_intercept <- post_samples$b_Intercept

median(post_samples_intercept)
quantile(post_samples_intercept, c(0.05, 0.95))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
variance_decomposition(fit_interceptonly_moresamples, ci = 0.99)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_fixedslopes <- brm(popular ~ 1 + sex + extrav + (1 | class),  
                       data = popular2data, 
                       chains = 4, iter = 2000, warmup = 1000,
                       seed = 1400, file = 'fit_fixedslopes')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit_fixedslopes)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
prior_summary(fit_fixedslopes)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_fixedslopes_priors <- brm(popular ~ 1 + sex + extrav + (1 | class),  
                       data = popular2data, 
                       prior = c(
                         prior(normal(0, 5), class = b)
                       ),
                       chains = 4, iter = 2000, warmup = 1000,
                       seed = 1450, file = 'fit_fixedslopes_priors')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit_fixedslopes_priors)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
pp_check(fit_fixedslopes_priors)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
posterior_slopes <- gather_draws(fit_fixedslopes_priors, b_sex, b_extrav)

posterior_slopes


## -------------------------------------------------------------------------------------------------------------------------------------------------------
posterior_slopes %>%
  median_qi(.width = c(.66, .95, .99))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(posterior_slopes, aes(y = .variable, x = .value)) +
  stat_halfeye(.width = c(.8, .95)) +
  geom_vline(xintercept = 0, linetype = 'dashed', linewidth = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(posterior_slopes, aes(y = .variable, x = .value)) +
  stat_interval() +
  stat_summary(fun = median, geom = 'point', size = 2) +
  scale_color_brewer(palette = 'Blues') +
  geom_vline(xintercept = 0, linetype = 'dashed', linewidth = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_fixed12 <- brm(popular ~ 1 + sex + extrav + texp + (1 | class),  
                   data = popular2data, 
                   prior = c(
                     prior(normal(0, 5), class = b)
                   ),
                   chains = 4, iter = 2000, warmup = 1000,
                   seed = 703, file = 'fit_fixed12')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit_fixed12)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_randomslopes <- brm(popular ~ 1 + sex + extrav + texp + (1 + extrav + sex | class),  
                   data = popular2data, 
                   prior = c(
                     prior(normal(0, 5), class = b)
                   ),
                   chains = 4, iter = 2000, warmup = 1000,
                   seed = 709, file = 'fit_randomslopes')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fit_randomslopes)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
sex_slopes <- spread_draws(fit_randomslopes, b_sexgirl, r_class[class,variable]) %>%
  filter(variable == 'sexgirl') %>%
  mutate(slope = b_sexgirl + r_class)

sex_slopes %>% 
  median_qi(slope, .width = c(.66, .90, .95)) %>%
  ggplot(aes(y = class, x = slope, xmin = .lower, xmax = .upper)) +
  geom_interval() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Blues')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_extravrandomslope <- brm(popular ~ 1 + sex + extrav + texp + (1 + extrav | class),  
                             data = popular2data, 
                             prior = c(
                               prior(normal(0, 5), class = b)
                             ),
                             chains = 4, iter = 2000, warmup = 1000,
                             seed = 206, file = 'fit_extravrandomslope')

summary(fit_extravrandomslope)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_interceptonly_moresamples <- add_criterion(fit_interceptonly_moresamples, 'loo')
fit_fixedslopes_priors <- add_criterion(fit_fixedslopes_priors, 'loo')
fit_fixed12 <- add_criterion(fit_fixed12, 'loo')
fit_randomslopes <- add_criterion(fit_randomslopes, 'loo')
fit_extravrandomslope <- add_criterion(fit_extravrandomslope, 'loo')

loo_compare(fit_interceptonly_moresamples, fit_fixedslopes_priors, fit_fixed12, fit_randomslopes, fit_extravrandomslope)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
bayesfactor_parameters(fit_extravrandomslope)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
conditional_effects(fit_extravrandomslope)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
sex_emmeans <- emmeans(fit_extravrandomslope, ~ sex)

gather_emmeans_draws(sex_emmeans) %>%
  ggplot(aes(x = .value, y = sex)) +
  stat_interval(.width = c(.66, .95, .99)) +
  stat_summary(fun = median, geom = 'point', size = 2) +
  scale_color_brewer(palette = 'Greens') +
  ggtitle('posterior expected value by sex', 'averaged across extraversion and teacher experience')


## -------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------
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

