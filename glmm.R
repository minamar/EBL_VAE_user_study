# Formulas
# valence   ~ v_cat + a_cat + trial + age + anim_experience + sex + (1 | subject)
# arousal   ~ v_cat + a_cat + trial + age + anim_experience + sex + (1 | subject)
# dominance ~ v_cat + a_cat + trial + age + anim_experience + sex + (1 | subject)

# reaction_time ~ v_cat + a_cat + trial + age + anim_experience + sex + (1 | subject)

# attention ~ v_cat + a_cat + trial + age + anim_experience + sex + (1 | subject)
# was_emotion ~ v_cat + a_cat + trial + age + anim_experience + sex + (1 | subject)

library("psych")
library("nlme")
library("lme4")
library(effects)
library(corrplot)
library(stargazer)
library(car)
library(ggplot2)
library(DHARMa)

# GLMER
m_valence <- glmer(valence ~ v_cat + a_cat + a_cat*v_cat + trial + (1 | idAnim) + (1 | subject), data = ratings, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
simulationOutput <- simulateResiduals(fittedModel = m_valence, n = 250)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

m_valence
anova(m_valence)

# GLMER
m_arousal <- glmer(arousal ~ v_cat + a_cat + a_cat*v_cat + trial + (1 | idAnim) + (1 | subject), data = ratings, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
simulationOutput <- simulateResiduals(fittedModel = m_valence, n = 250)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

m_valence
anova(m_valence)