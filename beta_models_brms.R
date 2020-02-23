
rm(list = ls(all.names = TRUE))

library(brms)
library(sjPlot)
library(sjstats)
library(ggplot2); 
library(loo)
library(dplyr)
library(tibble)
library(knitr)
library(bayesplot)
theme_set(bayesplot::theme_default(base_size = 14))

prior_weak = get_prior(form7, data = ratings, family = zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"))

b_brms_m7 = brm(form7,
               data = ratings, 
               family = zero_one_inflated_beta(link = "logit", link_phi = "log", link_zoi = "logit", link_coi = "logit"),
               prior = prior_weak,
               iter = 4000,
               chains = 4,
               cores = 4,
               control = list(max_treedepth = 15, adapt_delta = 0.9999),
               autocor = NULL,
               save_all_pars = TRUE,
               sample_prior = TRUE,
               file = '/home/mina/Dropbox/APRIL-MINA/EXP4_EBL_GEN_VAE_USER/r_code/stan_models/b_brms_m7')

mod = b_brms_m3

# Diagnostics
summary(mod)
plot(mod)
pp_check(mod, resp = "valence")
pp_check(mod, resp = "arousal")
pp_check(mod, resp = "dominance")
marginal_effects(mod)


## Model comparison with loo
b_brms_m7_loo = loo(b_brms_m7, save_psis = TRUE)

# If there is warning pareto_k > 0.7 
b_brms_m7_reloo = loo(b_brms_m7, reloo = T)
loo_compare(b_brms_m1_loo, b_brms_m2_loo, b_brms_m3_reloo, b_brms_m4_loo, b_brms_m5_reloo, b_brms_m6_loo)


# 
fitted_values <-fitted(mod)
head(fitted_values)
dat <- as.data.frame(cbind(Y = standata(mod)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))

# ??
yrep <- posterior_predict(mod)
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

fit1 = data.frame(v_catNeg_zoi = antilogit(coda$b_zoi_valence_Intercept),
                 a_catr3_zoi =      antilogit(coda$b_zoi_arousal_Intercept),
                 v_catNeu_zoi =    antilogit(coda$b_zoi_valence_Intercept + coda$b_zoi_valence_v_catNeu),
                 v_catPos_zoi =   antilogit(coda$b_zoi_valence_Intercept + coda$b_zoi_valence_v_catPos),
                 a_catr4_zoi = antilogit(coda$b_zoi_arousal_Intercept + coda$b_zoi_arousal_a_catr4),
                 a_catr5_zoi =    antilogit(coda$b_zoi_arousal_Intercept + coda$b_zoi_arousal_a_catr5)
)

posterior_samples(mod, pars = "b_")[,13:24] %>% 
  #mutate_at(c("b_zoi_valence_Intercept"), plogis) %>% 
  mutate_all( plogis) %>% 
  posterior_summary() %>% 
  as.data.frame() %>% 
  rownames_to_column("Parameter") %>% 
  kable(digits = 2) 

fit2 = data.frame(v_catNeg =      antilogit(coda$b_valence_Intercept),
                  a_catr3 =      antilogit(coda$b_arousal_Intercept),
                    v_catNeu =    antilogit(coda$b_valence_Intercept + coda$b_valence_v_catNeu),
                    v_catPos =   antilogit(coda$b_valence_Intercept + coda$b_valence_v_catPos),
                    a_catr4 = antilogit(coda$b_arousal_Intercept + coda$b_valence_a_catr4),
                    a_catr5 =    antilogit(coda$b_arousal_Intercept + coda$b_valence_a_catr5),
                    v_age =   antilogit(coda$b_valence_Intercept + coda$b_valence_age), 
                    a_age =   antilogit(coda$b_arousal_Intercept + coda$b_arousal_age), 
                    v_sexM =   antilogit(coda$b_valence_Intercept + coda$b_valence_sexM), 
                    a_sexM = antilogit(coda$b_arousal_Intercept + coda$b_arousal_sexM)
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
library(bayesplot)
posterior <- as.array(b_brms_m10)
dim(posterior)
dimnames(posterior)

mcmc_intervals(posterior, pars = vars(starts_with("r_subject")))
mcmc_intervals(posterior, pars = vars(starts_with("r_idAnim")))

# Prior predictive plots
conditions <- data.frame(v_cat = unique(ratings$v_cat))
rownames(conditions) <- unique(ratings$v_cat)
me_loss_prior1 <- marginal_effects(
  mod, conditions = conditions, 
  re_formula = NULL, method = "fitted"
)
plot(me_loss_prior1, ncol = 3, points = TRUE, plot = TRUE)
p0$dev + ggtitle("Prior predictive distributions")

