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

fit_a = fitdistr(ratings_old$arousal, "normal") 
fit_a
hist(ratings_old$arousal, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit_a$estimate[1], fit_a$estimate[2]), col="green", lwd=2, add=T)

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


