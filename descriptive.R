library(psych)
library(ggplot2)
library(ggpubr)
# Boxplot neg, neu, pos valence
boxplot(valence~v_cat,data=ratings, main="", xlab="Valence category", ylab="Valence scores")
# Boxplot r3=low, r4=medium, r5=high arousal
boxplot(arousal~a_cat,data=ratings, main="", xlab="arousal category", ylab="arousal scores")

# Summary statistics
str(ratings)
summary(ratings)
voi <- c("valence", "arousal", "dominance", "attention", "reaction_time")
by(ratings[voi], ratings$v_cat, summary)
by(ratings[voi], ratings$a_cat, summary)

# Histograms
ggplot(data=ratings, aes(ratings$valence)) + geom_histogram()
ggplot(data=ratings, aes(ratings$arousal)) + geom_histogram()
ggplot(data=ratings, aes(ratings$dominance)) + geom_histogram()
ggplot(data=ratings, aes(ratings$attention)) + geom_histogram()
ggplot(data=ratings, aes(ratings$reaction_time)) + geom_histogram()

# Q-Q plots
ggqqplot(ratings$valence)
ggqqplot(ratings$arousal)
ggqqplot(ratings$dominance)
ggqqplot(ratings$reaction_time)

# Normality test
shapiro.test(ratings$valence)
shapiro.test(ratings$arousal)
shapiro.test(ratings$dominance)
shapiro.test(ratings$reaction_time)

# Categorical scatter plots
ggplot(ratings, aes(x = v_cat, y = valence)) + geom_point(position = position_dodge(width = 0.4))

ggplot(ratings, aes(x = a_cat, y = arousal)) + geom_point(position = position_dodge(width = 0.4))
