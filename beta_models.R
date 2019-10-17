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
library(sjPlot)
library(sjstats)
library(ggplot2); theme_set(theme_bw())

form1 = valence ~  v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form2 = valence ~  (1|subject)
form3 = valence ~  (1|idAnim)
form4 = valence ~ v_cat * a_cat
form5 = valence ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form6 = valence ~ v_cat * a_cat + (1|idAnim) + (1 |subject)
#form_ = valence ~ 1 + v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1 + v_cat|subject) # no diff


my_prior = get_prior(form6, data = ratings, family = zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"))

zoib_model <- bf(
  form0,
  phi ~ v_cat,
  zoi ~ v_cat,
  coi ~ v_cat, 
  family = zero_one_inflated_beta()
)

b_brms_m6 = brm(form6,
               data = ratings, 
               family = zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"),
               prior = my_prior,
               iter = 2000,
               chains = 4,
               cores = 4,
               control = list(max_treedepth = 15),
               autocor = NULL,
               save_all_pars = TRUE,
               save_model = '/home/mina/Dropbox/APRIL-MINA/EXP4_EBL_GEN_VAE_USER/r_code/stan_models/b_brms_m6')

mod = b_brms_m6

# Diagnostics
summary(mod)
plot(mod)
pp = pp_check(mod)
pp + theme_bw(mod)
marginal_effects(mod)

# Model comparisons
b_brms_m1 <- add_criterion(b_brms_m1, "waic")
b_brms_m5 <- add_criterion(b_brms_m5, "waic")
b_brms_m6 <- add_criterion(b_brms_m6, "waic")
loo_compare(b_brms_m6, b_brms_m1, criterion = "waic")

##
b_brms_m0_loo = loo(b_brms_m0, save_psis = TRUE)
b_brms_m1_loo = loo(b_brms_m1, nsamples = 4000)
b_brms_m2_loo = loo(b_brms_m2, save_psis = TRUE)
b_brms_m3_loo = loo(b_brms_m3, save_psis = TRUE)
b_brms_m4_loo = loo(b_brms_m4, save_psis = TRUE)
b_brms_m5_loo = loo(b_brms_m5, save_psis = TRUE)
b_brms_m6_loo = loo(b_brms_m6, save_psis = TRUE)

# If there is warning pareto_k > 0.7 
b_brms_m6_reloo = loo(b_brms_m6, reloo = T)
loo_compare(b_brms_m0_loo, b_brms_m1_reloo, b_brms_m2_loo, b_brms_m3_loo, b_brms_m4_loo, b_brms_m5_loo, b_brms_m6_reloo)

yrep <- posterior_predict(b_brms_m1_loo)
loo::ppc_loo_pit_overlay(ratings$valence, yrep, lw = weights(b_brms_m1_loo$psis_object))


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
coda = brms::posterior_samples(mod)
antilogit = function(x) 1 / (1 + exp(-x))   # same as plogis()
fit2 = data.frame(v_catNeg =      antilogit(coda[, 1]),
                  v_catr3 =      antilogit(coda[, 1]),
                    v_catNeu =    antilogit(coda[, 1] + coda[, 2]),
                    v_catPos =   antilogit(coda[, 1] + coda[, 3]),
                    a_catr4 = antilogit(coda[, 1] + coda[, 4]),
                    a_catr5 =    antilogit(coda[, 1] + coda[, 5]),
                    trial = antilogit(coda[, 1] + coda[, 6]),
                    age =   antilogit(coda[, 1] + coda[, 7]), 
                    anim_experience =    antilogit(coda[, 1] + coda[, 8]),
                    sexM =   antilogit(coda[, 1] + coda[, 8]),
                    
                    v_catNeg_a_catr3 = antilogit(coda[, 1]),
                    v_catNeu_a_catr3 = antilogit(coda[, 1] + coda[, 2]),
                    v_catPos_a_catr3 = antilogit(coda[, 1] + coda[, 3]),
                  
                    v_catNeg_a_catr4 = antilogit(coda[, 1] + coda[, 4]),
                    v_catNeu_a_catr4 = antilogit(coda[, 1] + coda[, 10] + coda[, 2] + coda[, 4]),
                    v_catPos_a_catr4 = antilogit(coda[, 1] + coda[, 11] + coda[, 3] + coda[, 4]),
                    
                    v_catNeg_a_catr5 = antilogit(coda[, 1] + coda[, 5]),
                    v_catNeu_a_catr5 = antilogit(coda[, 1] + coda[, 12] + coda[, 2] + coda[, 5]),
                    v_catPos_a_catr5 = antilogit(coda[, 1] + coda[, 13] + coda[, 3] + coda[, 5])
)
 
neu_vs_neg = fit2$v_catNeu - fit2$v_catNeg
hist(neu_vs_neg)
quantile(neu_vs_neg, probs = c(.5, .025, .975)) 
mean(neu_vs_neg > 0)

pos_vs_neu = fit2$v_catPos - fit2$v_catNeu
hist(pos_vs_neu)
quantile(pos_vs_neu, probs = c(.5, .025, .975)) 
mean(pos_vs_neu > 0)

pos_vs_neg = fit2$v_catPos - fit2$v_catNeg
hist(pos_vs_neg)
quantile(pos_vs_neg, probs = c(.5, .025, .975)) 
mean(pos_vs_neg > 0)

female_vs_male = fit2$v_catNeg - fit2$sexM
hist(female_vs_male)
quantile(female_vs_male, probs = c(.5, .025, .975)) 
mean(female_vs_male > 0)

# Visualization of posterion per subject or item
posterior <- as.array(b_brms_m5)
dim(posterior)
dimnames(posterior)

mcmc_intervals(posterior, pars = vars(starts_with("r_subject")))
mcmc_intervals(posterior, pars = vars(starts_with("r_idAnim")))


