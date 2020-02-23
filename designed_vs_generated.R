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
  mutate(condition = replace(condition, condition == 28, "des_pre")) %>% 
  mutate(condition = replace(condition, condition == 29, "gen_pre")) %>%
  mutate(condition = replace(condition, condition == 30, "des_post")) %>%
  mutate(condition = replace(condition, condition == 31, "gen_post"))

anth_all$likert = ordered(anth_all$likert)
anth_all$likert = as.numeric(anth_all$likert)
anth_all$likert = factor(anth_all$likert, levels = 1:5)
anth_all$condition = factor(anth_all$condition, levels = c("des_pre", "gen_pre", "des_post", "gen_post"))
# anth_all_count = anth_all[,2:3] %>%
#   group_by(condition, likert) %>%
#   summarize(Count = n())

model = polr(likert ~ condition,  data = anth_all, Hess = TRUE)
summary(model)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Create predictions
df_both = data.frame(condition = rep(c("des_pre", "gen_pre", "des_post", "gen_post"), each = 5),  
                     likert = rep(c(1:5), 2),
                     sex = rep(c("M", "F"), each = 5))

polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anth_all <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  geom_line() + 
  geom_point() +
  xlab("Anthropomorphism")



# Anthropomorphism- pre-post groups
# Replace condition with names of groups
anth_pre_post <- m_anth %>%
  mutate(condition = replace(condition, condition == 28 | condition == 29, "pre")) %>% 
  mutate(condition = replace(condition, condition == 30 | condition == 31, "post")) 

anth_pre_post$likert = ordered(anth_pre_post$likert)
anth_pre_post$likert = as.numeric(anth_pre_post$likert)
anth_pre_post$likert = factor(anth_pre_post$likert, levels = 1:5)
anth_pre_post$condition = factor(anth_pre_post$condition, levels = c("pre", "post"))

# anth_all_count = anth_all[,2:3] %>%
#   group_by(condition, likert) %>%
#   summarize(Count = n())

model = polr(likert ~ condition,  data = anth_pre_post, Hess = TRUE)
summary(model)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Create predictions
df_both = data.frame(condition = rep(c("pre", "post"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anth_pre_post <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  geom_line() + 
  geom_point() +
  xlab("Anthropomorphism")

# Anthropomorphism- sex groups
# Replace condition with names of groups
anth_sex <- m_anth 

anth_sex$likert = ordered(anth_sex$likert)
anth_sex$likert = as.numeric(anth_sex$likert)
anth_sex$likert = factor(anth_sex$likert, levels = 1:5)

# anth_all_count = anth_all[,2:3] %>%
#   group_by(condition, likert) %>%
#   summarize(Count = n())

model = polr(likert ~ sex,  data = anth_sex, Hess = TRUE)
summary(model)

marginal = lsmeans(model, ~sex)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

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
  mutate(condition = replace(condition, condition == 28, "des_pre")) %>% 
  mutate(condition = replace(condition, condition == 29, "gen_pre")) %>%
  mutate(condition = replace(condition, condition == 30, "des_post")) %>%
  mutate(condition = replace(condition, condition == 31, "gen_post"))

anim_all$likert = ordered(anim_all$likert)
anim_all$likert = as.numeric(anim_all$likert)
anim_all$likert = factor(anim_all$likert, levels = 1:5)
anim_all$condition = factor(anim_all$condition, levels = c("des_pre", "gen_pre", "des_post", "gen_post"))

model = polr(likert ~ condition,  data = anim_all, Hess = TRUE)

summary(model)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))


# Create predictions
df_both = data.frame(condition = rep(c("des_pre", "gen_pre", "des_post", "gen_post"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anim_all <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  geom_line() + 
  geom_point() +
  xlab("Animacy")


# Animacy- pre-post groups
# Replace condition with names of groups
anim_pre_post <- m_anim %>%
  mutate(condition = replace(condition, condition == 28 | condition == 29, "pre")) %>% 
  mutate(condition = replace(condition, condition == 30 | condition == 31, "post")) 

anim_pre_post$likert = ordered(anim_pre_post$likert)
anim_pre_post$likert = as.numeric(anim_pre_post$likert)
anim_pre_post$likert = factor(anim_pre_post$likert, levels = 1:5)
anim_pre_post$condition = factor(anim$condition, levels = c("pre", "post"))

model = polr(likert ~ condition,  data = anim_pre_post, Hess = TRUE)
summary(model)

marginal = lsmeans(model, ~condition)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Create predictions
df_both = data.frame(condition = rep(c("pre", "post"), each = 5),  
                     likert = rep(c(1:5), 2))
polr_both_probs = cbind(df_both, predict(model, newdata = df_both, type = "probs", se = TRUE))
polr_both_probs = polr_both_probs[,-2]
polr_both_probs = unique(polr_both_probs)
polr_both_probs_df = reshape2::melt(polr_both_probs, id.vars = "condition", 
                                    variable.name = "likert", value.name = "probability")

# Figure 
g_anim_pre_post <- ggplot(polr_both_probs_df, aes(x = likert, y = probability, 
                               color = condition, group = condition)) +
  geom_line() + 
  geom_point() +
  xlab("Animacy")

# Animacy- sex groups
# Replace condition with names of groups
anim_sex <- m_anim 

anim_sex$likert = ordered(anim_sex$likert)
anim_sex$likert = as.numeric(anim_sex$likert)
anim_sex$likert = factor(anim_sex$likert, levels = 1:5)


model = polr(likert ~ sex,  data = anim_sex, Hess = TRUE)
summary(model)

marginal = lsmeans(model, ~sex)
pairs(marginal, adjust="tukey")

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

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
  geom_line() + 
  geom_point() +
  xlab("Animacy")

