# 28: 'des_pre',
# 29: 'gen_pre',
# 30: 'des_post',
# 31: 'gen_post'

library(likert)
library(psych)
library(ordinal)
library(tidyverse)
library(lsmeans)
library(coin)
library(MASS)
library(xtable)
select <- dplyr::select

# Function to turn counts into rows I found laying around the web somewhere
countsToCases = function(x, countcol = "Count") {
  # Get the row indices to pull from x
  idx = rep.int(seq_len(nrow(x)), x[[countcol]])
  # Drop count column
  x[[countcol]] = NULL
  # Get the rows from x
  x[idx, ]
}
# Survey plus sex
surv_ <- merge(select(dem, -X, -age, -anim_experience), surv, by = "subject")

# Load anthropomorphism only
surv_anth <- select(surv_, -X, -starts_with("animacy"))
# Aggregate with median
m_anth <- apply(select(surv_anth, -sex, -subject, -condition),1, median, na.rm = TRUE)
# Add to df with subject and condtion
m_anth <- data.frame(surv_anth$subject, surv_anth$sex, surv_anth$condition, m_anth)
names(m_anth) <- c("subject", "sex", "condition", "likert")

# Anthropomorphism- All groups
# Replace condition with names of groups
anth_all <- m_anth %>%
  mutate(condition = replace(condition, condition == 28, "designed_pre")) %>% 
  mutate(condition = replace(condition, condition == 29, "generated_pre")) %>%
  mutate(condition = replace(condition, condition == 30, "designed_post")) %>%
  mutate(condition = replace(condition, condition == 31, "generated_post"))

anth_all$likert = ordered(anth_all$likert)
anth_all$likert = as.numeric(anth_all$likert)
anth_all$likert = factor(anth_all$likert, levels = 1:5, ordered = TRUE)
anth_all$condition = factor(anth_all$condition, levels = c("designed_pre", "generated_pre", "designed_post", "generated_post"), ordered = FALSE)

model = polr(likert ~ condition,  data = anth_all, Hess = TRUE)
summary(model)

# Uncomment only to get theh nominal test
model_clm = clm(likert ~ condition,  data = anth_all, link = "logit")
nominal_test(model_clm)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# Create predictions
df_both = data.frame(condition = rep(c("designed_pre", "generated_pre", "designed_post", "generated_post"), each = 5),  
                     likert = rep(c(1:5), 2))

polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")


# Figure 
g_anth_all <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  scale_color_discrete(breaks=c("designed_pre", "generated_pre", "designed_post", "generated_post")) +
  geom_line() + 
  geom_point() +
  xlab("Anthropomorphism")



# Anthropomorphism- pre-post groups
# Replace condition with names of groups
anth_pre_post <- m_anth %>%
  mutate(condition = replace(condition, condition == 28 | condition == 29, "pretest")) %>% 
  mutate(condition = replace(condition, condition == 30 | condition == 31, "posttest")) 

anth_pre_post$likert = ordered(anth_pre_post$likert)
anth_pre_post$likert = as.numeric(anth_pre_post$likert)
anth_pre_post$likert = factor(anth_pre_post$likert, levels = 1:5, ordered = TRUE)
anth_pre_post$condition = factor(anth_pre_post$condition, levels = c("pretest", "posttest"))

model = polr(likert ~ condition,  data = anth_pre_post, Hess = TRUE)
summary(model)

# Uncomment only to get theh nominal test
# model_clm = clm(likert ~ condition,  data = anth_pre_post, link = "logit")
# nominal_test(model_clm)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# Create predictions
df_both = data.frame(condition = rep(c("pretest", "posttest"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anth_pre_post <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  scale_color_discrete(breaks=c("pretest", "posttest")) +
  geom_line() + 
  geom_point() +
  xlab("Anthropomorphism")

# Anthropomorphism- sex groups
# Replace condition with names of groups
anth_sex <- m_anth 

anth_sex$likert = ordered(anth_sex$likert)
anth_sex$likert = as.numeric(anth_sex$likert)
anth_sex$likert = factor(anth_sex$likert, levels = 1:5, ordered = TRUE)

model = polr(likert ~ sex,  data = anth_sex, Hess = TRUE)
summary(model)

# Uncomment only to get theh nominal test
model_clm = clm(likert ~ sex,  data = anth_sex, link = "logit")
nominal_test(model_clm)

marginal = lsmeans(model, ~sex)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# Create predictions
df_both = data.frame(sex = rep(c("F", "M"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "sex", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anth_sex <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = sex, group = sex)) +
  scale_color_manual(name="Gender", labels = c("Female", "Male"), values=c("#7CAE00","#C77CFF")) + 
  geom_line() + 
  geom_point() +
  xlab("Anthropomorphism")


##############################################################################
# Load animacy only
surv_anim <- select(surv_, -X, -starts_with("anth"))
# Aggregate with median
m_anim <- apply(select(surv_anim, -sex, -subject, -condition),1, median, na.rm = TRUE)
# Add to df with subject and condtion
m_anim <- data.frame(surv_anim$subject, surv_anth$sex, surv_anim$condition, m_anim)
names(m_anim) <- c("subject", "sex", "condition", "likert")

# Animacy- All groups
# Replace condition with names of groups
anim_all <- m_anim %>%
  mutate(condition = replace(condition, condition == 28, "designed_pre")) %>% 
  mutate(condition = replace(condition, condition == 29, "generated_pre")) %>%
  mutate(condition = replace(condition, condition == 30, "designed_post")) %>%
  mutate(condition = replace(condition, condition == 31, "generated_post"))

anim_all$likert = ordered(anim_all$likert)
anim_all$likert = as.numeric(anim_all$likert)
anim_all$likert = factor(anim_all$likert, levels = 1:5, ordered = TRUE)
anim_all$condition = factor(anim_all$condition, levels = c("designed_pre", "generated_pre", "designed_post", "generated_post"), ordered = FALSE)

model = polr(likert ~ condition,  data = anim_all, Hess = TRUE)
summary(model)

# Uncomment only to get theh nominal test
model_clm = clm(likert ~ condition,  data = anim_all, link = "logit")
nominal_test(model_clm)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# Create predictions
df_both = data.frame(condition = rep(c("designed_pre", "generated_pre", "designed_post", "generated_post"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anim_all <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  scale_color_discrete(breaks=c("designed_pre", "generated_pre", "designed_post", "generated_post")) +
  labs(color='Animation group')  +
  geom_line() + 
  geom_point() +
  xlab("Animacy")


# Animacy- pre-post groups
# Replace condition with names of groups
anim_pre_post <- m_anim %>%
  mutate(condition = replace(condition, condition == 28 | condition == 29, "pretest")) %>% 
  mutate(condition = replace(condition, condition == 30 | condition == 31, "posttest")) 

anim_pre_post$likert = ordered(anim_pre_post$likert)
anim_pre_post$likert = as.numeric(anim_pre_post$likert)
anim_pre_post$likert = factor(anim_pre_post$likert, levels = 1:5, ordered = TRUE)
anim_pre_post$condition = factor(anim_pre_post$condition, levels = c("pretest", "posttest"))

model = polr(likert ~ condition,  data = anim_pre_post, Hess = TRUE)
summary(model)

# Uncomment only to get theh nominal test
# model_clm = clm(likert ~ condition,  data = anim_pre_post, link = "logit")
# nominal_test(model_clm)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# Create predictions
df_both = data.frame(condition = rep(c("pretest", "posttest"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anim_pre_post <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  scale_color_discrete(breaks=c("pretest", "posttest")) +
  labs(color='Animation group')  +
  geom_line() + 
  geom_point() +
  xlab("Animacy")

# Animacy- sex groups
# Replace condition with names of groups
anim_sex <- m_anim 

anim_sex$likert = ordered(anim_sex$likert)
anim_sex$likert = as.numeric(anim_sex$likert)
anim_sex$likert = factor(anim_sex$likert, levels = 1:5, ordered = TRUE)

model = polr(likert ~ sex,  data = anim_sex, Hess = TRUE)
summary(model)

# Uncomment only to get theh nominal test
model_clm = clm(likert ~ sex,  data = anim_sex, link = "logit")
nominal_test(model_clm)

marginal = lsmeans(model, ~sex)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model)) 

# Create predictions
df_both = data.frame(sex = rep(c("F", "M"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "sex", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anim_sex <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = sex, group = sex)) +
  scale_color_manual(name="Gender", labels = c("Female", "Male"), values=c("#7CAE00","#C77CFF")) + 
  geom_line() + 
  geom_point() +
  xlab("Animacy")


###########################################################
# Plots
library(cowplot)

# Plot gen vs des 
p_all <- plot_grid(g_anth_all + theme(legend.position = "none"), g_anim_all + theme(legend.position = "none"), labels = "AUTO", ncol = 2)
legend <- get_legend(
  g_anim_all +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(p_all, legend,  ncol = 1, rel_heights = c(1, .1))
ggsave("images/anth_anim_all.pdf", units="mm", width=200, height=80, dpi=300)

# Plot pre vs post
p_pre_post <- plot_grid(g_anth_pre_post + theme(legend.position = "none"), g_anim_pre_post + theme(legend.position = "none"), labels = "AUTO", ncol = 2)
legend <- get_legend(
  g_anim_pre_post +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(p_pre_post, legend,  ncol = 1, rel_heights = c(1, .1))
ggsave("images/anth_anim_pre_post.pdf", units="mm", width=200, height=80, dpi=300)

# Plot female vs male

p_sex <- plot_grid(g_anth_sex + theme(legend.position = "none"), g_anim_sex + theme(legend.position = "none"), labels = "AUTO", ncol = 2)
legend <- get_legend(
  g_anim_sex +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(p_sex, legend,  ncol = 1, rel_heights = c(1, .1))
ggsave("images/anth_anim_sex.pdf", units="mm", width=200, height=80, dpi=300)

################################################################
# Descriptive statistics
# Likert plots config
scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
theme_update(legend.text = element_text(size = rel(0.7)))

anth_title = "Anthropomorphism"
surv_ <- merge(select(dem, -X, -age, -anim_experience), surv, by = "subject")
surv_anth <- filter(select(surv_, -X, -starts_with("animacy")), condition == 28)
surv_anth <- select(surv_anth, -sex, -subject, -condition)
designed_pre = as.numeric(t(surv_anth))
surv_anth <- filter(select(surv_, -X, -starts_with("animacy")), condition == 29)
surv_anth <- select(surv_anth, -sex, -subject, -condition)
generated_pre = as.numeric(t(surv_anth))
surv_anth <- filter(select(surv_, -X, -starts_with("animacy")), condition == 30)
surv_anth <- select(surv_anth, -sex, -subject, -condition)
designed_post = as.numeric(t(surv_anth))
surv_anth <- filter(select(surv_, -X, -starts_with("animacy")), condition == 31)
surv_anth <- select(surv_anth, -sex, -subject, -condition)
generated_post = as.numeric(t(surv_anth))
anth_long = data.frame(designed_pre = as.factor(designed_pre), generated_pre = as.factor(generated_pre), designed_post = as.factor(designed_post), generated_post= as.factor(generated_post))

lik_anth = likert(anth_long)
anth_bar = plot(lik_anth, group.order = c("designed_pre", "generated_pre", "designed_post", "generated_post")) + ggtitle(anth_title)
anth_heat = plot(lik_anth, type= 'heat', group.order = c("designed_pre", "generated_pre", "designed_post", "generated_post")) + ggtitle(anth_title)


######################
# Animacy
anim_title = "Animacy"
surv_ <- merge(select(dem, -X, -age, -anim_experience), surv, by = "subject")
surv_anim <- filter(select(surv_, -X, -starts_with("anth")), condition == 28)
surv_anim <- select(surv_anim, -sex, -subject, -condition)
designed_pre = as.numeric(t(surv_anim))
surv_anim <- filter(select(surv_, -X, -starts_with("anth")), condition == 29)
surv_anim <- select(surv_anim, -sex, -subject, -condition)
generated_pre = as.numeric(t(surv_anim))
surv_anim <- filter(select(surv_, -X, -starts_with("anth")), condition == 30)
surv_anim <- select(surv_anim, -sex, -subject, -condition)
designed_post = as.numeric(t(surv_anim))
surv_anim <- filter(select(surv_, -X, -starts_with("anth")), condition == 31)
surv_anim <- select(surv_anim, -sex, -subject, -condition)
generated_post = as.numeric(t(surv_anim))
anim_long = data.frame(designed_pre = as.factor(designed_pre), generated_pre = as.factor(generated_pre), designed_post = as.factor(designed_post), generated_post= as.factor(generated_post))

lik_anim = likert(anim_long)
anim_bar = plot(lik_anim, group.order = c("designed_pre", "generated_pre", "designed_post", "generated_post")) + ggtitle(anim_title)
anim_heat = plot(lik_anim, type= 'heat', group.order = c("designed_pre", "generated_pre", "designed_post", "generated_post")) + ggtitle(anim_title)


library(cowplot)

# Plot attention likert
p_anth_lik <- plot_grid(anth_bar, anth_heat, labels = "AUTO", ncol = 2)
plot_grid(p_anth_lik, align = "hv")
ggsave("images/anth_lik.pdf", units="mm", width=350, height=80, dpi=300)

# Plot emotional likert
p_anim_lik <- plot_grid(anim_bar, anim_heat, labels = "AUTO", ncol = 2)
plot_grid(p_anim_lik, align = "hv")
ggsave("images/anim_lik.pdf", units="mm", width=350, height=80, dpi=300)