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
beta_model_glmmTMB = glmmTMB(valence   ~ v_cat + a_cat + v_cat*a_cat + trial + age + anim_experience + sex + (1 | idAnim) + (1 | subject), ratings_non_inflated, beta_family(link = "logit"))
simulationOutput <- simulateResiduals(fittedModel = beta_model_glmmTMB, n = 250)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
# wEB: https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.html
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#installing-loading-and-citing-the-package

rm(list = ls(all.names = TRUE))

library(brms)
library(ggplot2); theme_set(theme_bw())
my_prior = get_prior(valence ~ v_cat + a_cat + v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1|subject), data = ratings, family = beta_family(link = "logit"))

b_brms_m = brm(valence ~ v_cat + a_cat + v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1|subject),
               data = ratings, 
               zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"),
               prior = my_prior,
               iter = 2000,
               chains = 4,
               cores = 4,
               control = list(max_treedepth = 15),
               autocor = NULL)

summary(b_brms_m)
plot(b_brms_m)
pp = pp_check(b_brms_m)
pp + theme_bw(b_brms_m)

marginal_effects(b_brms_m)
 