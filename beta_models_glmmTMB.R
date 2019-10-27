## Beta regression
library(glmmTMB)
library(DHARMa)
library(car)
library(emmeans)
library(effects)
library(multcomp)
library(MuMIn)
library(DHARMa)
library(dotwhisker)
library(ggplot2); theme_set(theme_bw())

# glmmTMB
beta_model_glmmTMB = glmmTMB(valence ~  v_cat * a_cat + age + anim_experience + sex, ratings_non_inflated, beta_family(link = "logit"))
simulationOutput <- simulateResiduals(fittedModel = beta_model_glmmTMB, n = 250)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
# wEB: https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.html
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#installing-loading-and-citing-the-package
