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
library(loo)

form1 = valence ~  v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form2 = valence ~  (1|subject)
form3 = valence ~  (1|idAnim)
form4 = valence ~ v_cat * a_cat
form5 = valence ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form51 = valence ~  1 + v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form52 = valence ~  v_cat + a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form53 = valence ~  0 + v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form6 = valence ~ v_cat * a_cat + (1|idAnim) + (1 |subject)
form7 = valence ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (0 + v_cat |subject)
form8 = valence ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 + v_cat |subject)
#form_ = valence ~ 1 + v_cat * a_cat + trial + age + anim_experience + sex + (1|idAnim) + (1 + v_cat|subject) # no diff
form9 = arousal ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject)
form54 = valence ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1 |subject) # probit link

my_prior = get_prior(form54, data = ratings, family = zero_one_inflated_beta(link = "probit", link_phi = "log", link_zoi = "logit", link_coi = "logit"))

b_brms_m54 = brm(form54,
               data = ratings, 
               family = zero_one_inflated_beta(link = "probit", link_phi = "log", link_zoi = "logit", link_coi = "logit"),
               prior = my_prior,
               iter = 2000,
               chains = 4,
               cores = 4,
               control = list(max_treedepth = 15),
               autocor = NULL,
               save_all_pars = TRUE,
               sample_prior = TRUE,
               save_model = '/home/mina/Dropbox/APRIL-MINA/EXP4_EBL_GEN_VAE_USER/r_code/stan_models/b_brms_m54')

mod = b_brms_m54

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

## Model comparison with loo
b_brms_m0_loo = loo(b_brms_m0, save_psis = TRUE)

# If there is warning pareto_k > 0.7 
b_brms_m52_reloo = loo(b_brms_m52, reloo = T)
loo_compare(b_brms_m0_loo, b_brms_m1_reloo, b_brms_m2_loo, b_brms_m3_loo, b_brms_m4_loo, b_brms_m5_loo, b_brms_m6_reloo, b_brms_m7_reloo, b_brms_m8_loo, b_brms_m52_reloo)

# 
fitted_values <-fitted(mod)
head(fitted_values)
dat <- as.data.frame(cbind(Y = standata(mod)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))

# ??
yrep <- posterior_predict(b_brms_m5)
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

# Transform with posterior estimates with antilogit and prepare coefs for contrasts
coda = brms::posterior_samples(mod)
antilogit = function(x) 1 / (1 + exp(-x))   # same as plogis()
fit2 = data.frame(v_catNeg =      antilogit(coda$b_Intercept),
                  a_catr3 =      antilogit(coda$b_Intercept),
                    v_catNeu =    antilogit(coda$b_Intercept + coda$b_v_catNeu),
                    v_catPos =   antilogit(coda$b_Intercept + coda$b_v_catPos),
                    a_catr4 = antilogit(coda$b_Intercept + coda$b_a_catr4),
                    a_catr5 =    antilogit(coda$b_Intercept + coda$b_a_catr5),
                    #trial = antilogit(coda$b_Intercept + coda$b_trial),
                    age =   antilogit(coda$b_Intercept + coda$b_age), 
                    anim_experience =    antilogit(coda$b_Intercept + coda$b_anim_experience),
                    sexM =   antilogit(coda$b_Intercept + coda$b_sexM)
                    # v_catNeg_a_catr3 = antilogit(coda$b_Intercept),
                    # v_catNeu_a_catr3 = antilogit(coda$b_Intercept + coda$b_v_catNeu),
                    # v_catPos_a_catr3 = antilogit(coda$b_Intercept + coda$b_v_catPos),
                    # v_catNeg_a_catr4 = antilogit(coda$b_Intercept + coda$b_a_catr4),
                    # v_catNeu_a_catr4 = antilogit(coda$b_Intercept + coda$`b_v_catNeu:a_catr4` + coda$b_v_catNeu + coda$b_a_catr4),
                    # v_catPos_a_catr4 = antilogit(coda$b_Intercept + coda$`b_v_catPos:a_catr4` + coda$b_v_catPos + coda$b_a_catr4),
                    # v_catNeg_a_catr5 = antilogit(coda$b_Intercept + coda$b_a_catr5),
                    # v_catNeu_a_catr5 = antilogit(coda$b_Intercept + coda$`b_v_catNeu:a_catr5` + coda$b_v_catNeu + coda$b_a_catr5),
                    # v_catPos_a_catr5 = antilogit(coda$b_Intercept + coda$`b_v_catPos:a_catr5`  + coda$b_v_catPos + coda$b_a_catr5)
)

# Contrasts analysis
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

Pos_r5_vs_Neg_r3 = fit2$v_catPos_a_catr5 - fit2$v_catNeg_a_catr3
hist(Pos_r5_vs_Neg_r3)
quantile(Pos_r5_vs_Neg_r3, probs = c(.5, .025, .975)) 
mean(Pos_r5_vs_Neg_r3 > 0)

Pos_r5_vs_Neg_r5 = fit2$v_catPos_a_catr5 - fit2$v_catNeg_a_catr5
hist(Pos_r5_vs_Neg_r5)
quantile(Pos_r5_vs_Neg_r5, probs = c(.5, .025, .975)) 
mean(Pos_r5_vs_Neg_r5 > 0)

Pos_r5_vs_Pos_r3 = fit2$v_catPos_a_catr5 - fit2$v_catPos_a_catr3
hist(Pos_r5_vs_Pos_r3)
quantile(Pos_r5_vs_Pos_r3, probs = c(.5, .025, .975)) 
mean(Pos_r5_vs_Pos_r3 > 0)

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


