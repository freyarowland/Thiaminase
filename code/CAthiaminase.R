# CA marine prey survey

# load data
prey <- read.csv("data/CAprey.csv", header = TRUE, fileEncoding="UTF-8-BOM")
anch <- read.csv("data/AnchovyThiaminase.csv", header = TRUE, fileEncoding = "UTF-8-BOM")

# libraries
library(ggplot2)
library(viridis)
library(dplyr)
library(rstanarm)

# read as factor
prey$common <- as.factor(prey$common)
prey$area <- as.factor(prey$area)

anch$common <- as.factor(anch$common)
anch$area <- as.factor(anch$area)
anch$year <- as.factor(anch$year)

# summaries
# change year under filter argument if want to see yearly values
anch_summ <- anch %>%
  filter(year == 2020) %>%
  dplyr::group_by(stage, area) %>%
  summarize(count = n())

# plot thiaminase ----

# reorder by median
prey$common <- with(prey, reorder(common, -thiaminase, median, na.rm = T))


## plot
CAplot <- ggplot(aes(y = thiaminase, x = common), data = prey) +
  geom_boxplot() +
  geom_jitter(width = 0.1, aes(fill = area), alpha = 0.7, size = 2, pch = 21)+
  scale_fill_viridis_d(direction = -1, name = "Area") +
  labs(title="Prey survey 2021") +
  theme_bw(base_size = 14) + 
  xlab("Common name") +
  ylab("Thiaminase (nmol/g/min)")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))
print(CAplot)
ggsave(filename = "figures/thiaminase2021.png", dpi = 300, width = 12, height = 6)


## plot with box
CAplot2 <- ggplot(aes(y = thiaminase, x = common), data = prey) +
  geom_rect(data = prey, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = 5.5, ymin=-Inf, ymax=Inf),
            fill='#FF9999', alpha=0.9) +
  geom_boxplot() +
  geom_jitter(width = 0.1, aes(fill = area), alpha = 0.7, size = 2, pch = 21)+
  scale_fill_viridis_d(direction = -1, name = "Area") +
  labs(title="Prey survey 2021")  +
  theme_bw(base_size = 14) + 
  xlab("Common name") +
  ylab("Thiaminase (nmol/g/min)")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))
print(CAplot2)
ggsave(CAplot2, filename = "figures/thiaminase2021_redbox.png", dpi = 300, width = 12, height = 6)


## compare anchovy life stages

## plot
anchplot <- ggplot(aes(y = thiaminase, x = stage, fill = area), data = anch) +
  geom_boxplot(alpha = 0.7) +
  # geom_point(
  #   pch = 21,
  #   size = 2,
  #   alpha = 0.7
  # ) +
  scale_fill_viridis_d(direction = -1, name = "Area") +
  labs(title="Anchovy 2020-2021") +
  theme_bw(base_size = 14) + 
  xlab("Life stage") +
  ylab("Thiaminase (nmol/g/min)")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(scales = "fixed", cols = vars(year))
print(anchplot)
ggsave(filename = "figures/anchovy.png", dpi = 300, width = 9, height = 6)





## is there a difference among species?
# are there differences in species statistically?

# t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glmer(
  thiaminase ~ (1|common),
  data = prey,
  family = gaussian(link = "identity"),
  # prior = t_prior,
  # prior_intercept = t_prior, 
  cores = 2,
  seed = 12345,
  iter = 4000
)
summary(fit1)

fit1 <- stan_glmer(
  thiaminase ~ 1 + area + (1|common),
  data = prey,
  family = gaussian(link = "identity"),
  # prior = t_prior,
  # prior_intercept = t_prior, 
  cores = 2,
  seed = 12345,
  iter = 4000
)
summary(fit1)

## does it fit
loo_fit <- loo(fit1)


# describe posteriors
bayestestR::describe_posterior(
  fit1,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# posterior summary
round(posterior_interval(fit1, prob = 0.95), digits = 3)

# look at model
launch_shinystan(fit1) # no differences

# plot of posteriors
posterior <- as.matrix(fit1)

# plot it

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
bayesplot::mcmc_areas(posterior,
           prob = 0.8) + plot_title








## is there a difference among sites for anchovy?
# are there differences in sites statistically?

# t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glmer(
  thiaminase ~ stage + (1|area),
  data = anch,
  family = gaussian(link = "identity"),
  # prior = t_prior,
  # prior_intercept = t_prior, 
  cores = 2,
  seed = 12345,
  iter = 4000
)
summary(fit1)

## does it fit
loo_fit <- loo(fit1)


# describe posteriors
bayestestR::describe_posterior(
  fit1,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

# posterior summary
round(posterior_interval(fit1, prob = 0.95), digits = 3)

# look at model
launch_shinystan(fit1) # no differences

# plot of posteriors
posterior <- as.matrix(fit1)

# plot it

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
bayesplot::mcmc_areas(posterior,
                      prob = 0.8) + plot_title



