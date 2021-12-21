# thiaminase

# load data
dat2 <- read.csv("AllData.csv", header = TRUE)

str(dat2)

# remove one row with no data
dat2 <- dat2[-290,]

# record as factor
dat2$Habitat2 <- as.factor(dat2$Habitat2)
dat2$Continent <- as.factor(dat2$Continent)
dat2$Marine <- as.factor(dat2$Marine)
dat2$Order <- as.factor(dat2$Order)
dat2$Tropical <- as.factor(dat2$Tropical)

# separate marine and freshwater
fresh2 <- subset(dat2, Marine == 0)
salt2 <- subset(dat2, Marine == 1)

# make separate df for non-tropical
nontrop <- subset(dat2, Tropical == 0)
nontrop2 <- subset(nontrop, Marine == 0)
saltnontrop2 <- subset(nontrop, Marine == 1)

# libraries
library(lme4)
library(rstanarm)
library(ggplot2)
library(bayesplot)
library(ggrepel)
library(bayestestR)
library(ggridges)
library(tidybayes)
library(dplyr)
library(modelr)
library(shinystan)

# get orders that are thiaminase positive
plus <- subset(dat2, Thiaminase == 1)
summary(dat2$Order)
summary(plus$Order)



# summarize data
dat2 %>%
  group_by(Climate)  %>%
  summarize(total = sum(Thiaminase == 1 | Thiaminase == 0, na.rm = TRUE),
            thiaminase_pos = sum(Thiaminase == 1, na.rm = TRUE),
            marine = sum(Marine == 1, na.rm = TRUE),
            fresh = sum(Marine == 0, na.rm = TRUE),
            marine_pos = sum(Marine == 1 & Thiaminase == 1, na.rm = TRUE),
            avg_size = mean(MaxTL, na.rm = TRUE),
            CV_size = sd(MaxTL, na.rm = TRUE)/mean(MaxTL, na.rm = TRUE)*100,
            avg_Omega3 = mean(Omega3, na.rm = TRUE),
            CV_Omega3 = sd(Omega3, na.rm = TRUE)/mean(Omega3, na.rm = TRUE)*100,
            benthic = sum(Habitat2 == "BE", na.rm = TRUE),
            benthopelagic = sum(Habitat2 == "BP", na.rm = TRUE),
            pelagic = sum(Habitat2 == "PE", na.rm = TRUE))

# habitat and relationship to trophic level?
ggplot(data = dat2) +
  #geom_jitter(height = 0.05, width = 0.05) +
  geom_density_ridges(aes(x = TL_fooditems, y = Habitat2)) +
  theme_bw(base_size = 16) +
  scale_fill_viridis_d() +
  ylab("Count") +
  xlab("Trophic level")

# age and max length
ggplot(data = dat2, aes(x = MaxAge, y = as.factor(Thiaminase), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  theme_bw(base_size = 16) +
  ylab("Thiaminase") +
  xlab("Maximum age")

ggplot(data = dat2, aes(x = MaxTL, y = as.factor(Thiaminase), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  theme_bw(base_size = 16) +
  ylab("Thiaminase") +
  xlab("Maximum total length (cm)")

# log-transform size for easier viewing
ggplot(data = dat2, aes(x = MaxTL, y = as.factor(Thiaminase), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  theme_bw(base_size = 16) +
  ylab("Thiaminase") +
  xlab("Maximum total length (cm)")


# trophic level model 
# trophic level is a significant predictor of thiaminase activity! ----
troph_mod <- aov(TL_fooditems ~ Habitat2, data = dat2)
summary(troph_mod)
TukeyHSD(troph_mod, conf.level=.95)

# total length is related to thiaminase activity! ----
size_mod <- lm(MaxTL ~ Thiaminase, data = dat2)
summary(size_mod)

# age is related to thiaminase activity! ----
age_mod <- lm(MaxAge ~ Thiaminase, data = dat2)
summary(age_mod)

# age and length ----
corr_mod <- lm(MaxAge ~ MaxTL, data = dat2)
summary(corr_mod)

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(Thiaminase ~ TL_fooditems, data = dat2,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 cores = 2, seed = 12345)

fit1_fresh <- stan_glm(Thiaminase ~ TL_fooditems, data = fresh2,
                       family = binomial(link = "logit"),
                       prior = t_prior, prior_intercept = t_prior,
                       cores = 2, seed = 12345)

fit1_salt <- stan_glm(Thiaminase ~ TL_fooditems, data = salt2,
                      family = binomial(link = "logit"),
                      prior = t_prior, prior_intercept = t_prior,
                      cores = 2, seed = 12345)

# non tropical only
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(Thiaminase ~ TL_fooditems, data = nontrop,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 cores = 2, seed = 12345)

fit1_fresh <- stan_glm(Thiaminase ~ TL_fooditems, data = nontrop2,
                       family = binomial(link = "logit"),
                       prior = t_prior, prior_intercept = t_prior,
                       cores = 2, seed = 12345)

fit1_salt <- stan_glm(Thiaminase ~ TL_fooditems, data = saltnontrop2,
                      family = binomial(link = "logit"),
                      prior = t_prior, prior_intercept = t_prior,
                      cores = 2, seed = 12345)

# explore fit
fit1_loo <- loo(fit1)
fit1_fresh_loo <- loo(fit1_fresh)
fit1_salt_loo <- loo(fit1_salt)

# describe posteriors
describe_posterior(
  fit1,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

describe_posterior(
  fit1_fresh,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

describe_posterior(
  fit1_salt,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# probability of predictor variables
summary(fit1, digits = 3)
summary(fit1_fresh, digits = 3)
summary(fit1_salt, digits = 3)
round(posterior_interval(fit1, prob = 0.95), digits = 3)
round(posterior_interval(fit1_fresh, prob = 0.95), digits = 3)
round(posterior_interval(fit1_salt, prob = 0.95), digits = 3)

# calculate probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}

logit2prob(coef(fit1))
logit2prob(coef(fit1_fresh))
logit2prob(coef(fit1_salt))

# Predicted probability as a function of x
pr_thia <- function(x, ests) plogis(ests[1] + ests[2] * x)
# A function to slightly jitter the binary data
# jitt <- function(...) {
#   geom_point(aes_string(...), position = position_jitter(height = 0.05, width = 0.1),
#              size = 2, shape = 21, stroke = 0.2)
# }
#dat2 %>%
  #data_grid(TL_fooditems = seq_range(TL_fooditems, n = 51)) %>%
  #add_predicted_draws(fit1) %>%
  ggplot(dat2, aes(x = TL_fooditems, y = Thiaminase, fill = Climate)) + #, label = ï..Common)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_jitter(height = 0.04, width = 0.02, size = 4, pch = 21, alpha = 0.7) +
  #jitt(x="TrophicLevelEst") +
  stat_function(fun = pr_thia, args = list(ests = coef(fit1)),
                size = 2, color = "#1f78b4") +
  stat_function(fun = pr_thia, args = list(ests = coef(fit1_fresh)),
                size = 2, color = "#a6cee3") +
  stat_function(fun = pr_thia, args = list(ests = coef(fit1_salt)),
                size = 2, color = "#b2df8a") +
  theme_bw(base_size = 16) +
  scale_fill_viridis_d() +
  xlab("Trophic level estimate") 
  #geom_text_repel(force = 3, max.overlaps = 20)



## add in PUFA ----
  
# make separate salty figure
fit2 <- update(fit1, formula = Thiaminase ~ TL_fooditems + Omega3)
(coef_fit2 <- round(coef(fit2), 3))

pr_thia2 <- function(x, y, ests) plogis(ests[1] + ests[2] * x + ests[3] * y)
grid <- expand.grid(TL_fooditems = seq(2, 5, length.out = 100),
                    Omega3 = seq(0, 3, length.out = 100))
grid$prob <- with(grid, pr_thia2(TL_fooditems, Omega3, coef(fit2)))

ggplot(grid, aes(x = TL_fooditems, y = Omega3)) +
  geom_tile(aes(fill = prob)) +
  #geom_point(data = salt2, aes(color = factor(Continent), label = ï..Common), size = 3) +
  geom_point(data = dat2, aes(color = factor(Thiaminase), label = ï..Common), size = 3) +
  #geom_point(data = dat2, aes(color = Tropical, label = ï..Common), size = 3) +
  scale_fill_gradient(low = "#e5f5e0", high = "#31a354") +
  theme_bw(base_size = 16) +
  scale_color_manual("Thiaminase", values = c("white", "black"), labels = c("No", "Yes")) +
  xlab("Trophic level estimate") +
  #scale_color_viridis_d() +
  ylab("PUFA concentration (g/100 g)") +
  geom_text_repel(aes(label = ï..Common), data = dat2)





# make separate freshwater figure
fit2_fresh <- update(fit1_fresh, formula = Thiaminase ~ TL_fooditems + Omega3)
(coef_fit2 <- round(coef(fit2_fresh), 3))

pr_thia2 <- function(x, y, ests) plogis(ests[1] + ests[2] * x + ests[3] * y)
grid <- expand.grid(TL_fooditems = seq(2, 5, length.out = 100),
                    Omega3 = seq(0, 3, length.out = 100))
grid$prob <- with(grid, pr_thia2(TL_fooditems, Omega3, coef(fit2_fresh)))

ggplot(grid, aes(x = TL_fooditems, y = Omega3)) +
  geom_tile(aes(fill = prob)) +
  #geom_point(data = salt2, aes(color = factor(Continent), label = ï..Common), size = 3) +
  geom_point(data = fresh2, aes(color = factor(Thiaminase), label = ï..Common), size = 3) +
  #geom_point(data = dat2, aes(color = Tropical, label = ï..Common), size = 3) +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  theme_bw(base_size = 16) +
  scale_color_manual("Thiaminase", values = c("white", "black"), labels = c("No", "Yes")) +
  xlab("Trophic level estimate") +
  #scale_color_viridis_d() +
  ylab("PUFA concentration (g/100 g)") +
  geom_text_repel(aes(label = ï..Common), data = fresh2)







# # only plot max length
# # Predicted probability as a function of x
# pr_thia <- function(x, ests) plogis(ests[1] + ests[2] * x)
# 
# ggplot(dat, aes(x = MaxTL, y = Thiaminase, fill = Thiaminase)) +
#   scale_y_continuous(breaks = c(0, 0.5, 1)) +
#   geom_point(size = 4, pch = 21) +
#   #jitt(x="TrophicLevelEst") +
#   stat_function(fun = pr_thia, args = list(ests = coef(fit2)),
#                 size = 2, color = "gray35") +
#   theme_bw(base_size = 16)
# 
# # plot max length and trophic level together
# pr_thia2 <- function(x, y, ests) plogis(ests[1] + ests[2] * x + ests[3] * y)
# grid <- expand.grid(TrophicLevelEst = seq(2, 5, length.out = 100),
#                     MaxTL = seq(0, 155, length.out = 100))
# grid$prob <- with(grid, pr_thia2(TrophicLevelEst, MaxTL, coef(fit2)))
# ggplot(grid, aes(x = TrophicLevelEst, y = MaxTL)) +
#   geom_tile(aes(fill = prob)) +
#   theme_bw(base_size = 16) +
#   geom_point(data = dat, aes(color = factor(Thiaminase)), size = 4, alpha = 0.85) +
#   scale_fill_gradient(low = "#fee6ce", high = "#e6550d") +
#   scale_color_manual("Thiaminase", values = c("white", "black"), labels = c("No", "Yes"))


## PUFAs and thiaminase ----

# PUFA is not related to trophic level
lm <- lm(Omega3 ~ TL_fooditems, data = dat2)
summary(lm)

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(Thiaminase ~ Omega3, data = dat2,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 cores = 2, seed = 12345)

fit1_fresh <- stan_glm(Thiaminase ~ Omega3, data = fresh2,
                       family = binomial(link = "logit"),
                       prior = t_prior, prior_intercept = t_prior,
                       cores = 2, seed = 12345)

fit1_salt <- stan_glm(Thiaminase ~ Omega3, data = salt2,
                      family = binomial(link = "logit"),
                      prior = t_prior, prior_intercept = t_prior,
                      cores = 2, seed = 12345)

## non tropical 
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(Thiaminase ~ Omega3, data = nontrop,
                 family = binomial(link = "logit"),
                 prior = t_prior, prior_intercept = t_prior,
                 cores = 2, seed = 12345)

fit1_fresh <- stan_glm(Thiaminase ~ Omega3, data = nontrop2,
                       family = binomial(link = "logit"),
                       prior = t_prior, prior_intercept = t_prior,
                       cores = 2, seed = 12345)

fit1_salt <- stan_glm(Thiaminase ~ Omega3, data = saltnontrop2,
                      family = binomial(link = "logit"),
                      prior = t_prior, prior_intercept = t_prior,
                      cores = 2, seed = 12345)

# explore fit
fit1_loo <- loo(fit1)
fit1_fresh_loo <- loo(fit1_fresh)
fit1_salt_loo <- loo(fit1_salt)

# describe posteriors
describe_posterior(
  fit1,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

describe_posterior(
  fit1_fresh,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

describe_posterior(
  fit1_salt,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)


# probability of predictor variables
summary(fit1, digits = 3)
summary(fit1_fresh, digits = 3)
summary(fit1_salt, digits = 3)
round(posterior_interval(fit1, prob = 0.95), digits = 3)
round(posterior_interval(fit1_fresh, prob = 0.95), digits = 3)
round(posterior_interval(fit1_salt, prob = 0.95), digits = 3)

# calculate probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}

logit2prob(coef(fit1))
logit2prob(coef(fit1_fresh))
logit2prob(coef(fit1_salt))



# Predicted probability as a function of x
pr_thia <- function(x, ests) plogis(ests[1] + ests[2] * x)
# A function to slightly jitter the binary data
# jitt <- function(...) {
#   geom_point(aes_string(...), position = position_jitter(height = 0.05, width = 0.1),
#              size = 2, shape = 21, stroke = 0.2)
# }
ggplot(dat2, aes(x = Omega3, y = Thiaminase, fill = Climate)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_jitter(height = 0.04, width = 0.02, size = 4, pch = 21, alpha = 0.7) +
  #geom_point(data = dat2, size = 3) +
  #jitt(x="TrophicLevelEst") +
  stat_function(fun = pr_thia, args = list(ests = coef(fit1)),
                size = 2, color = "#1f78b4") +
  stat_function(fun = pr_thia, args = list(ests = coef(fit1_fresh)),
                size = 2, color = "#a6cee3") +
  stat_function(fun = pr_thia, args = list(ests = coef(fit1_salt)),
                size = 2, color = "#b2df8a") +
  scale_fill_viridis_d() + 
  theme_bw(base_size = 16) +
  xlab("Omega3 concentration (g per 100 g)") 
  #geom_text_repel(force = 1.2, max.overlaps = 15, nudge.y = 0.4, direction = "y")



# Max age vs. total length ----
ggplot(aes(x = MaxAge, y = MaxTL, fill = Thiaminase), data = dat) +
  geom_point(size = 4, pch = 21) +
  # stat_function(fun = pr_thia, args = list(ests = coef(fit1)),
  #               size = 2, color = "gray35") +
  theme_bw(base_size = 16)

cor(x = dat$MaxAge, y = dat$MaxTL, use = "complete.obs")  


## invasive and thiaminase

invdat <- subset(dat2, Invasive == 1)
noninvdat <- subset(dat2, Invasive == 0)

## multiple regression ----

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
allfit <- stan_glm(
  Thiaminase ~ TL_fooditems +
    Omega3 +
    scale(MaxTL) +
    Marine +
    Tropical +
    Invasive +
    Benthic +
    Benthopelagic +
    Pelagic +
    Cata_Anad,
  data = dat2,
  family = binomial(link = "logit"),
  prior = t_prior,
  prior_intercept = t_prior,
  iter = 10000,
  chains = 4,
  cores = 3,
  seed = 12345
)

summary(allfit)

# get posteriors
describe_posterior(
  allfit,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# calculate probabilities for factors
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}
logit2prob(coef(allfit))

# check fit
loo(allfit) # good

# look at model fit with shiny stan
launch_shinystan(allfit)


# plot of posteriors
posterior <- as.matrix(allfit)

# plot it
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
multreg_plot <- mcmc_areas(posterior,
           pars = c("TL_fooditems", "Omega3", "Marine1", 
                    "Tropical1", "scale(MaxTL)", "Invasive", "Benthic", "Benthopelagic", "Pelagic", "Cata_Anad"),
           prob = 0.95) + 
  plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dashed", colour = "red") +
  scale_y_discrete(labels = c('Trophic level','Omega3',
                              'Marine', 'Tropical',
                              'Max length',
                              'Invasive',
                              'Benthic',
                              'Benthopelagic',
                              'Pelagic',
                              'Anadromous/Catadromous'))


# calculate probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}

logit2prob(coef(allfit))

# checking fit
loo(allfit) # good

# fits match y
color_scheme_set("red")
ppc_dens_overlay(y = allfit$y,
                 yrep = posterior_predict(allfit, draws = 50))


## look at climate vs. PUFA ----
common <- subset(dat2, Climate == "Tropical" | Climate == "Subtropical" | Climate == "Temperate")
ggplot(dat2, aes(x = Climate, y = Omega3)) +
  geom_boxplot() + 
  geom_jitter(width = 0.06, size = 4, pch = 21, alpha = 0.7, fill = "gray30", alpha = 0.8) +
  scale_fill_viridis_d() + 
  theme_bw(base_size = 16) +
  xlab("Climate region") +
  ylab("Omega3 (g/100 g)")

pufa_mod <- aov(Omega3 ~ Climate, data = common)
summary(pufa_mod)
TukeyHSD(pufa_mod, conf.level= 0.95)
