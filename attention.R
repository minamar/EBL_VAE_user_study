library(likert)
library(psych)
library(ordinal)
library(tidyverse)
library(lsmeans)
library(coin)
library(MASS)
library(xtable)

select <- dplyr::select

rat_ <- ratings
# rat_$attention = ordered(rat_$attention)
# rat_$attention  = as.numeric(rat_$attention)
rat_$attention = factor(rat_$attention, levels = 1:5, ordered = TRUE)
rat_$was_emotion = factor(rat_$was_emotion, levels = 1:5, ordered = TRUE)
rat_$sex = factor(rat_$sex, levels = c("F", "M"), ordered = FALSE)
rat_$v_cat  = factor(rat_$v_cat, levels = c("Neg", "Neu", "Pos"), ordered = TRUE)
rat_$a_cat  = factor(rat_$a_cat, levels = c("r3", "r4", "r5"), ordered = TRUE)

# Attention v_cat model
model = polr(attention ~ v_cat,  data = rat_, Hess = TRUE)
summary(model)

# Goodness of model and proportional odds assumption (holds if null is not rejected)
# pchisq(deviance(model),df.residual(model))
# # brant(model)

marginal = lsmeans(model, ~v_cat)
marginal
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# With clm
model_clm = clm(attention ~ v_cat,  data = rat_, link = "logit")
nominal_test(model_clm)
scale_test(model_clm)
marginal = lsmeans(model_clm, ~v_cat)
pairs(marginal, adjust="tukey")

df_both = data.frame(v_cat = rep(c("Neg", "Neu", "Pos"), each = 5),
                     attention = rep(c(1:5), 3))

polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "v_cat", 
                                    variable.name = "attention", value.name = "probability")

# Figure 
g_att_v <- ggplot(polr_both_probs_df, aes(x = attention, y = probability, 
                                             color = v_cat, group = v_cat)) +
  scale_color_manual(name="v_cond", labels = c("Negative", "Neutral", "Positive"), values=c("#F8766D","#00BA38", "#619CFF")) + 
  geom_line() + 
  geom_point() +
  xlab("Attention")

# Attention a_cat
model = polr(attention ~ a_cat,  data = rat_, Hess = TRUE)
summary(model)

# Goodness of model and proportional odds assumption (holds if null is not rejected)
# pchisq(deviance(model),df.residual(model))
# brant(model)

marginal = emmeans(model, ~a_cat)
marginal
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 
# With clm
model_clm = clm(attention ~ a_cat,  data = rat_, link = "logit")
nominal_test(model_clm)
scale_test(model_clm)
marginal = emmeans(model_clm, ~a_cat)
pairs(marginal, adjust="tukey")

df_both = data.frame(a_cat = rep(c("r3", "r4", "r5"), each = 5),  
                     attention = rep(c(1:5), 3))

polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "a_cat", 
                                    variable.name = "attention", value.name = "probability")

# Figure 
g_att_a <- ggplot(polr_both_probs_df, aes(x = attention, y = probability, 
                                          color = a_cat, group = a_cat)) +
  scale_color_manual(name="a_cond", labels = c("Low", "Medium", "High"), values=c("#F8766D","#00BA38", "#619CFF")) + 
  geom_line() + 
  geom_point() +
  xlab("Attention")


#######################

# Emotion v_cat
model = polr(was_emotion ~ v_cat,  data = rat_, Hess = TRUE)
summary(model)

# Goodness of model and proportional odds assumption (holds if null is not rejected)
# pchisq(deviance(model),df.residual(model))
# brant(model)

marginal = lsmeans(model, ~v_cat)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 
# With clm
model_clm = clm(was_emotion ~ v_cat,  data = rat_, link = "logit")
nominal_test(model_clm)
scale_test(model_clm)
marginal = lsmeans(model_clm, ~v_cat)
pairs(marginal, adjust="tukey")

df_both = data.frame(v_cat = rep(c("Neg", "Neu", "Pos"), each = 5),  
                     was_emotion = rep(c(1:5), 3))

polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "v_cat", 
                                    variable.name = "was_emotion", value.name = "probability")

# Figure 
g_emo_v <- ggplot(polr_both_probs_df, aes(x = was_emotion, y = probability, 
                                          color = v_cat, group = v_cat)) +
  scale_color_manual(name="v_cond", labels = c("Negative", "Neutral", "Positive"), values=c("#F8766D","#00BA38", "#619CFF")) + 
  geom_line() + 
  geom_point() +
  xlab("Emotion")

# Emotion a_cat
model = polr(was_emotion ~ a_cat,  data = rat_, Hess = TRUE)
summary(model)

# Goodness of model and proportional odds assumption (holds if null is not rejected)
# pchisq(deviance(model),df.residual(model))
# brant(model)

marginal = lsmeans(model, ~a_cat)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# With clm
model_clm = clm(was_emotion ~ a_cat,  data = rat_, link = "logit")
nominal_test(model_clm)
scale_test(model_clm)
marginal = lsmeans(model_clm, ~a_cat)
pairs(marginal, adjust="tukey")

df_both = data.frame(a_cat = rep(c("r3", "r4", "r5"), each = 5),  
                     attention = rep(c(1:5), 3))

polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "a_cat", 
                                    variable.name = "was_emotion", value.name = "probability")

# Figure 
g_emo_a <- ggplot(polr_both_probs_df, aes(x = was_emotion, y = probability, 
                                          color = a_cat, group = a_cat)) +
  scale_color_manual(name="a_cond", labels = c("Low", "Medium", "High"), values=c("#F8766D","#00BA38", "#619CFF")) + 
  geom_line() + 
  geom_point() +
  xlab("Emotion")


######################
# Plots
library(cowplot)

# Plot attention v_cat a_cat
p_att_va <- plot_grid(g_att_v + theme(legend.position = "bottom"), g_att_a + theme(legend.position = "bottom"), labels = "AUTO", ncol = 2)
plot_grid(p_att_va, align = "hv")
ggsave("images/att_va.pdf", units="mm", width=200, height=80, dpi=300)

# Plot emotion v_cat a_cat
p_emo_va <- plot_grid(g_emo_v + theme(legend.position = "bottom"), g_emo_a + theme(legend.position = "bottom"), labels = "AUTO", ncol = 2)
plot_grid(p_emo_va)
ggsave("images/emo_va.pdf", units="mm", width=200, height=80, dpi=300)

################
# Descriptive statistics
# Likert plots config
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
theme_update(legend.text = element_text(size = rel(0.8)), 
             axis.text=element_text(size=12, face="bold"))

va_lev_ord = c("Negative", "Neutral", "Positive", "Low", "Medium", "High")

att_title = "The robot's behaviour attracts my attention."
df = data.frame(select(ratings, subject, v_cat, a_cat, attention))
Neutral = subset(df, v_cat == "Neu")$attention
Negative = subset(df, v_cat == "Neg")$attention
Positive = subset(df, v_cat == "Pos")$attention
Low = subset(df, a_cat == "r3")$attention
Medium = subset(df, a_cat == "r4")$attention
High = subset(df, a_cat == "r5")$attention
att_long = data.frame(Negative = as.factor(Negative), Neutral = as.factor(Neutral), Positive = as.factor(Positive), Low= as.factor(Low), Medium= as.factor(Medium), High= as.factor(High))

lik_att = likert(att_long)
att_bar = plot(lik_att, centered=FALSE, group.order = va_lev_ord) + ggtitle(att_title)  
att_heat = plot(lik_att, type= 'heat', group.order = va_lev_ord) + ggtitle(att_title)

emo_title = "The robot's expression is emotional."
df = data.frame(select(ratings, subject, v_cat, a_cat, was_emotion))
Neutral = subset(df, v_cat == "Neu")$was_emotion
Negative = subset(df, v_cat == "Neg")$was_emotion
Positive = subset(df, v_cat == "Pos")$was_emotion
Low = subset(df, a_cat == "r3")$was_emotion
Medium = subset(df, a_cat == "r4")$was_emotion
High = subset(df, a_cat == "r5")$was_emotion
emo_long = data.frame(Negative = as.factor(Negative), Neutral = as.factor(Neutral), Positive = as.factor(Positive), Low= as.factor(Low), Medium= as.factor(Medium), High= as.factor(High))


lik_emo = likert(emo_long)
emo_bar = plot(lik_emo, centered=FALSE, group.order = va_lev_ord) + ggtitle(emo_title) 
emo_heat = plot(lik_emo, type= 'heat') + ggtitle(emo_title) 
# emo_heat = emo_heat + theme(axis.text=element_text(size=12, face="bold"))
  # scale_y_discrete(drop=FALSE, limits=va_lev_ord)

library(cowplot)
# Plot attention + emo likert bar
p_bar_lik <- plot_grid(att_bar+ theme(legend.position = "none"), emo_bar+ theme(legend.position = "none"), labels = "AUTO", ncol = 2)
legend <- get_legend(
  att_bar +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
plot_grid(p_bar_lik, legend,  ncol = 1, rel_heights = c(1, .1))
ggsave("images/att_emo_bar_lik.pdf", units="mm", width=260, height=80, dpi=300)

# Plot attention + emo likert heat
p_heat_lik <- plot_grid(att_heat+ theme(legend.position = "none"), emo_heat+ theme(legend.position = "none"), labels = "AUTO", ncol = 2) 
legend <- get_legend(
  att_heat +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
plot_grid(p_heat_lik, legend,  ncol = 1, rel_heights = c(1, .1))
ggsave("images/att_emo_heat_lik.pdf", units="mm", width=300, height=120, dpi=300)

