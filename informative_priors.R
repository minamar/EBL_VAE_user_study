# Priors
library(ggplot2)

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

