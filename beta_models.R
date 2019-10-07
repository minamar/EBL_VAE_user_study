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

form0 = valence ~ v_cat 
form1 = valence ~  v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1|subject)
form2 = valence ~ v_cat + a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1|subject)
form3 = valence ~ v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1|subject)

my_prior = get_prior(form1, data = ratings, family = zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"))

zoib_model <- bf(
  form0,
  phi ~ v_cat,
  zoi ~ v_cat,
  coi ~ v_cat, 
  family = zero_one_inflated_beta()
)

b_brms_m1 = brm(form1,
               data = ratings, 
               family = zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"),
               prior = my_prior,
               iter = 2000,
               chains = 4,
               cores = 4,
               control = list(max_treedepth = 15),
               autocor = NULL)

mod = b_brms_m1

# Diagnostics
summary(mod)
plot(mod)
pp = pp_check(mod)
pp + theme_bw(mod)
marginal_effects(mod)

# Custom plot of model predictions
newdata = data.frame(valence_category = levels(ratings$v_cat))
fit = fitted(mod,
             newdata = newdata,
             re_formula = NA,    # ignore random effects
             summary = TRUE, # mean and 95% CI
             scale = "response"
             )               # convert to %
colnames(fit) = c('fit', 'se', 'lwr', 'upr')
df_plot = cbind(newdata, fit)
 