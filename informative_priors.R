# Priors
library(ggplot2)
library(dplyr)
library(rstatix)
library(ggpubr)
library(cowplot)

# 1. Summary stats and boxplots of data from STUDY A to be used as priors for STUDY B
#***************************************************************************************
# Add v_cat and a_cat columns
ratings_old %>%
  select(subject, valence, arousal)  %>% 
  mutate(v_cat = case_when(valence <= 0.33 ~ "Negative",
                           (valence > 0.33 & valence <= 0.66) ~ "Neutral",
                           valence > 0.66 ~ "Positive"),
         a_cat = case_when(arousal <= 0.33 ~ "Low",
                           (arousal > 0.33 & arousal <= 0.66) ~ "Medium",
                           arousal > 0.66 ~ "High"))  %>%
  {. ->> ratings_old_cat}

ratings_old_cat  %>%
  group_by(v_cat) %>%
  get_summary_stats(valence, type = "median_iqr")
ratings_old_cat  %>%
  group_by(a_cat) %>%
  get_summary_stats(arousal, type = "median_iqr")

# Vis
v_bxp <- ggboxplot(ratings_old_cat, x = "v_cat", y = "valence", add = "point", order=c("Negative", "Neutral", "Positive"))
v_plot <- v_hist + theme_gray() + scale_x_discrete(labels = c('negative','neutral','positive')) + 
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))

a_bxp <- ggboxplot(ratings_old_cat, x = "a_cat", y = "arousal", add = "point", order=c("Low", "Medium", "High"))
a_plot <- a_bxp + theme_gray() + scale_x_discrete(labels = c('low','medium','high')) + 
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))

plot_grid(v_plot, a_plot, labels = "AUTO", ncol = 2)
ggsave("images/va_bxp_prior.pdf", units="mm", width=150, height=80, dpi=300)


# 2. Gaussian Mixture Models with 3 groups on valence ratings of STUDY A: 
#***************************************************************************************
library(mclust, quietly=TRUE)
fit = Mclust(ratings_old$valence, G=3, model="V")
summary(fit)
plot(fit, what="density", main="", xlab="Valence")
fit$parameters
curve(dnorm(x, 0.1713, 0.4), col="green", lwd=2, add=T)
curve(dnorm(x, 0.5748577, 0.25), col="blue", lwd=2, add=T)
curve(dnorm(x, 0.9582016, 0.55), col="red", lwd=2, add=T)

#############################
# Older priors 
# Histograms of responses
par(mfrow=c(1,2))
hist(ratings_old$valence, main=NULL, xlab="Pleasure")
hist(ratings_old$arousal, main=NULL, xlab="Intensity")
ggplot(ratings_old, aes(x=valence, y=arousal)) + geom_point()

# Test different distribution fits
library(MASS)
library(fitdistrplus)
fit_v = fitdistr(ratings_old$valence, "normal") 
fit_v
hist(ratings_old$valence, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit_v$estimate[1], fit_v$estimate[2]), col="green", lwd=2, add=T)
curve(dnorm(x, 0.48, 0.14), col="red", lwd=2, add=T)
curve(dbeta(x, 2.1746469, 2.1900334), col="blue", lwd=2, add=T)

fit_a = fitdistr(ratings_old$arousal, "normal") 
fit_a
hist(ratings_old$arousal, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit_a$estimate[1], fit_a$estimate[2]), col="green", lwd=2, add=T)
curve(dnorm(x, 0.6, 0.18), col="red", lwd=2, add=T)
curve(dbeta(x, 2.27418572, 1.63491286), col="blue", lwd=2, add=T)

# Summary stats
par(mfrow=c(1,2))
descdist(ratings_old$valence)
descdist(ratings_old$arousal)

# Find possible distribution of the data
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
fit <- fitDist(ratings_tiny_trans$arousal)
summary(fit)


# prior 19 
my_prior = c(
  #prior(lkj(1), class = cor),
  prior(beta(1,1), class = coi, resp = "arousal"),
  prior(normal(0.1767,0.10), class = Intercept, resp = "arousal"),
  prior(normal(0.5398,0.10), class = b, coef=a_catr4, resp = "arousal"),
  prior(normal(0.8373,0.10), class = b, coef=a_catr5, resp = "arousal"),
  prior(gamma(0.01, 0.01), class = phi, resp = "arousal"),
  prior(student_t(3, 0, 10), class = sd, resp = "arousal"),
  prior(beta(1, 1), class = zoi, resp = "arousal"),
  prior(beta(1,1), class = coi, resp = "valence"),
  prior(normal(0.1746,0.10), class = Intercept, resp = "valence"),
  prior(normal(0.5058,0.10), class = b, coef=v_catNeu, resp = "valence"),
  prior(normal(0.8154,0.10), class = b, coef=v_catPos, resp = "valence"),
  prior(gamma(0.01, 0.01), class = phi, resp = "valence"),
  prior(student_t(3, 0, 10), class = sd, resp = "valence"),
  prior(beta(1, 1), class = zoi, resp = "valence")
)

my_prior_18 = c(
  prior(normal(0.5,0.15), class = Intercept),                                               
  prior(student_t(3, 0, 10), class = sd),
  prior(logistic(0, 1), class = Intercept, dpar = coi),            
  prior(student_t(3, 0, 10), class = sd, dpar = coi),            
  #  prior(student_t(3, 0, 10), class = Intercept, dpar = phi),            
  #  prior(student_t(3, 0, 10), class = sd, dpar =phi),            
  prior(logistic(0, 1), class = Intercept, dpar =zoi),           
  prior(student_t(3, 0, 10), class = sd, dpar = zoi)           
)

set.seed(seed = 1923840479)
b_neg <- rnorm(1, 0.1746,0.10)
b_neu <- rnorm(1, 0.5058,0.10)
b_pos <- rnorm(1, 0.8154,0.10)
