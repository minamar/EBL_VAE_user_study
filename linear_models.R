library("nlme")
library("lme4")
library(DHARMa)

# LME
m_arousal <- lme(arousal ~ a_cat, random=~1|subject,data=ratings)
m_arousal
anova(m_arousal)

m_valence <- lme(mean_v ~ v_cat, random=~1|subject,data=v_ratings_aggr)
m_valence
anova(m_valence)

#LMER
Null<-lmer(valence ~ 1 # This simply means Happiness predicted by the intercept
           +(1|subject), # each school gets its own intercept 
           data=ratings, REML = FALSE)
summary(Null)

# lmer with p-values
library(lmerTest)

m_valence<-lmer(valence   ~ v_cat + a_cat + trial + age + anim_experience + sex + (1|subject), # each subject gets its own intercept 
                data=ratings, REML = FALSE)

summary(m_valence)

anova(Null, m_valence)
summary(m_valence, ddf = "Satterthwaite") # SAS method
anova(m_valence,ddf = "Kenward-Roger") # Kenny et al suggested method


# Logit transform of the response
m_valence <- lmer(log(valence/(1-valence)) ~ v_cat + a_cat + trial + age + anim_experience + sex + (1|subject), data=ratings)
