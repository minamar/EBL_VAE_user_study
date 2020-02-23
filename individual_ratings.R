# This is the analysis Q1 for the ratings of valence, arousal, dominance 
# and how they are predicted by v_cond and a_cond. THe ratings are aggregated for each participant 
# per level of the independent var (v_cond or a_cond)

library(dplyr) 
library(lmerTest)
library(FSA)
library(ggpubr)
library(rstatix)
library(broom)
library(tidyverse)
library(cowplot)

# TEST1: IV=v_cat

# Aggregate the data per participant and v_cat
ratings %>% 
  select(subject, valence, arousal, dominance, v_cat)  %>% 
  group_by(subject, v_cat) %>% 
  summarise(mean_v = mean(valence), mean_a = mean(arousal), mean_d = mean(dominance)) %>% 
  {. ->> v_ratings_aggr}

# Convert subject and v_cat into factor variables
v_ratings_aggr <- v_ratings_aggr %>%
  ungroup(subject) %>%
  convert_as_factor(subject, v_cat)
head(v_ratings_aggr, 3)

# Summary stats
v_ratings_aggr %>%
  group_by(v_cat) %>%
  get_summary_stats(mean_v, mean_a, mean_d, type = "mean_sd")

# Identify outliers
v_ratings_aggr %>%
  group_by(v_cat) %>%
  identify_outliers(mean_d)
  identify_outliers(mean_a)
  identify_outliers(mean_d)

# Normality
v_ratings_aggr %>%
  group_by(v_cat) %>%
  shapiro_test(mean_v) 

v_ratings_aggr %>%
  group_by(v_cat) %>%
  shapiro_test(mean_a)
  
v_ratings_aggr %>%
  group_by(v_cat) %>%
  shapiro_test(mean_d) 

# QQ plots
ggqqplot(v_ratings_aggr, "mean_v", facet.by = "v_cat", ylab = "valence", ggtheme = theme_bw())
ggqqplot(v_ratings_aggr, "mean_a", facet.by = "v_cat", ylab = "arousal", ggtheme = theme_bw())
ggqqplot(v_ratings_aggr, "mean_d", facet.by = "v_cat", ylab = "dominance", ggtheme = theme_bw())

# Boxplot Visualization
v_bxp_v <- ggboxplot(v_ratings_aggr, x = "v_cat", y = "mean_v", add = "point", merge = TRUE)
v_bxp_v
a_bxp_v <- ggboxplot(v_ratings_aggr, x = "v_cat", y = "mean_a", add = "point", merge = TRUE)
a_bxp_v
d_bxp_v <- ggboxplot(v_ratings_aggr, x = "v_cat", y = "mean_d", add = "point", merge = TRUE)
d_bxp_v

# Anova comp
v_res_v.aov <- anova_test(data = select(v_ratings_aggr, subject, mean_v, v_cat), dv = mean_v, wid = subject, within = v_cat)
get_anova_table(v_res_v.aov)

a_res_v.aov <- anova_test(data = select(v_ratings_aggr, subject, mean_a, v_cat), dv = mean_a, wid = subject, within = v_cat)
get_anova_table(a_res_v.aov)

d_res_v.aov <- anova_test(data = select(v_ratings_aggr, subject, mean_d, v_cat), dv = mean_d, wid = subject, within = v_cat)
get_anova_table(d_res_v.aov)

# Post-hoc tests
# pairwise comparisons
v_pwc_v <- v_ratings_aggr %>%
  pairwise_t_test(
    mean_v ~ v_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
v_pwc_v

a_pwc_v <- v_ratings_aggr %>%
  pairwise_t_test(
    mean_a ~ v_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
a_pwc_v

d_pwc_v <- v_ratings_aggr %>%
  pairwise_t_test(
    mean_d ~ v_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
d_pwc_v





# TEST2: IV = a_cat

# Aggregate the data per participant and a_cat
ratings %>% 
  select(subject, valence, arousal, dominance, a_cat)  %>% 
  group_by(subject, a_cat) %>% 
  summarise(mean_v = mean(valence), mean_a = mean(arousal), mean_d = mean(dominance)) %>% 
  {. ->> a_ratings_aggr}

# Convert subject and v_cat into factor variables
a_ratings_aggr <- a_ratings_aggr %>%
  ungroup(subject) %>%
  convert_as_factor(subject, a_cat)
head(a_ratings_aggr, 3)

# Summary stats
a_ratings_aggr %>%
  group_by(a_cat) %>%
  get_summary_stats(mean_v, mean_a, mean_d, type = "mean_sd")

# Identify outliers
a_ratings_aggr %>%
  group_by(a_cat) %>%
  identify_outliers(mean_v)
  identify_outliers(mean_a)
  identify_outliers(mean_d)

  
  # Normality
a_ratings_aggr %>%
  group_by(a_cat) %>%
  shapiro_test(mean_v)
a_ratings_aggr %>%
  group_by(a_cat) %>%
  shapiro_test(mean_a)
a_ratings_aggr %>%
  group_by(a_cat) %>%
  shapiro_test(mean_d)
  
# QQ plots
ggqqplot(a_ratings_aggr, "mean_v", facet.by = "a_cat", ylab = "valence")
ggqqplot(a_ratings_aggr, "mean_a", facet.by = "a_cat", ylab = "arousal")
ggqqplot(a_ratings_aggr, "mean_d", facet.by = "a_cat", ylab = "dominance")

# Boxplot Visualization
v_bxp_a <- ggboxplot(a_ratings_aggr, x = "a_cat", y = "mean_v", add = "point", merge = TRUE)
v_bxp_a
a_bxp_a <- ggboxplot(a_ratings_aggr, x = "a_cat", y = "mean_a", add = "point", merge = TRUE)
a_bxp_a
d_bxp_a <- ggboxplot(a_ratings_aggr, x = "a_cat", y = "mean_d", add = "point", merge = TRUE)
d_bxp_a

# Anova comp
v_res_a.aov <- anova_test(data = select(a_ratings_aggr, subject, mean_v, a_cat), dv = mean_v, wid = subject, within = a_cat)
get_anova_table(v_res_a.aov)

a_res_a.aov <- anova_test(data = select(a_ratings_aggr, subject, mean_a, a_cat), dv = mean_a, wid = subject, within = a_cat)
get_anova_table(a_res_a.aov)

d_res_a.aov <- anova_test(data = select(a_ratings_aggr, subject, mean_d, a_cat), dv = mean_d, wid = subject, within = a_cat)
get_anova_table(d_res_a.aov)

# Post-hoc tests
# pairwise comparisons
v_pwc_a <- a_ratings_aggr %>%
  pairwise_t_test(
    mean_v ~ a_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
v_pwc_a

a_pwc_a <- a_ratings_aggr %>%
  pairwise_t_test(
    mean_a ~ a_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
a_pwc_a

d_pwc_a <- a_ratings_aggr %>%
  pairwise_t_test(
    mean_d ~ a_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
d_pwc_a



# Visualization: box plots with p-values (V_BXB_V and A_BXP_A)
v_pwc_v <- v_pwc_v %>% add_xy_position(x = "v_cat")
v_plot <- v_bxp_v + ylab("aggregated valence ratings") + xlab("valence conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25,  hide.ns = TRUE, y.position = c(0.88, 0.92), v_pwc_v) +
  theme_gray() + scale_x_discrete(labels = c('negative','neutral','positive')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/v_bxp.pdf", units="in", width=5, height=4, dpi=300)
a_pwc_a <- a_pwc_a %>% add_xy_position(x = "a_cat")
a_plot <- a_bxp_a + ylab("aggregated arousal ratings") + xlab("arousal conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25, hide.ns = TRUE, y.position = c(0.88, 0.92), a_pwc_a) +
  theme_gray() + scale_x_discrete(labels = c('low','medium','high')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/a_bxp.pdf", units="in", width=5, height=4, dpi=300)
plot_grid(v_plot, a_plot, labels = "AUTO", ncol = 2)
ggsave("images/v_bxb_v_a_bxp_a.pdf", units="mm", width=150, height=80, dpi=300)

# Visualization: box plots with p-values (D_BXB_V and D_BXP_A)
d_pwc_v <- d_pwc_v %>% add_xy_position(x = "v_cat")
d_plot_v <- d_bxp_v + ylab("aggregated dominance ratings") + xlab("valence conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25,  hide.ns = TRUE, y.position = c(0.88, 0.92), d_pwc_v) +
  theme_gray() + scale_x_discrete(labels = c('negative','neutral','positive')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/v_bxp.pdf", units="in", width=5, height=4, dpi=300)
d_pwc_a <- d_pwc_a %>% add_xy_position(x = "a_cat")
d_plot_a <- d_bxp_a + ylab("aggregated dominance ratings") + xlab("arousal conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25, hide.ns = TRUE, y.position = c(0.88, 0.92), d_pwc_a) +
  theme_gray() + scale_x_discrete(labels = c('low','medium','high')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/a_bxp.pdf", units="in", width=5, height=4, dpi=300)
plot_grid(d_plot_v, d_plot_a, labels = "AUTO", ncol = 2)
ggsave("images/d_bxb_v_d_bxp_a.pdf", units="mm", width=150, height=80, dpi=300)

# Visualization: box plots with p-values (D_BXB_V and D_BXP_A)
a_pwc_v <- a_pwc_v %>% add_xy_position(x = "v_cat")
a_plot_v <- a_bxp_v + ylab("aggregated arousal ratings") + xlab("valence conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25,  hide.ns = TRUE,  a_pwc_v) +
  theme_gray() + scale_x_discrete(labels = c('negative','neutral','positive')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/v_bxp.pdf", units="in", width=5, height=4, dpi=300)
v_pwc_a <- v_pwc_a %>% add_xy_position(x = "a_cat")
v_plot_a <- v_bxp_a + ylab("aggregated valence ratings") + xlab("arousal conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25, hide.ns = TRUE, y.position = c(0.88, 0.92), v_pwc_a) +
  theme_gray() + scale_x_discrete(labels = c('low','medium','high')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/a_bxp.pdf", units="in", width=5, height=4, dpi=300)
plot_grid(a_plot_v, v_plot_a, labels = "AUTO", ncol = 2)
ggsave("images/a_bxb_v_v_bxp_a.pdf", units="mm", width=150, height=80, dpi=300)


##################################################################################################





# Non-parametric Kruskal-Wallis Test. Problem is non independent groups for v_cat
PT = kruskal.test(valence ~ v_cat, data = ratings)
# post hoc pairwise comparisons between group levels with corrections for multiple testing
pairwise.wilcox.test(ratings$valence, ratings$v_cat, p.adjust.method = "BH", paired=TRUE)


# Plot diagnostic plots for normality and heteroscedasticity
lmMod <- lm(mean_v ~ v_cat, data=ratings)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmMod)

# Linear model with psycho report
fit <- lmer(valence ~ v_cat + (1|subject), data=v1)
anova(fit)
results <- analyze(fit)
print(results)
results <- get_contrasts(fit, "v_cat")


# Friedman. Problem: data is Non-complete block design subjects gave more that one samples in each v_cat category
v1$v_cat = factor(v1$v_cat, ordered=TRUE)
v1$subject = factor(v1$subject, )
friedman.test(mean_v ~ v_cat | subject,
              data = v_ratings_aggr)
library(rcompanion)

PT = pairwiseSignTest(Likert ~ Instructor,
                      data   = Data,
                      method = "fdr")



# One-way repeated measures ANOVA.
res.aov <- aov(mean_v ~ v_cat + Error(subject/v_cat), data = v_ratings_aggr)
# Summary of the analysis
summary(res.aov)
library("emmeans")
# set orthogonal contrasts
options(contrasts = c("contr.sum", "contr.poly"))
emm <- emmeans(res.aov, ~ v_cat)
pairs(emm, adjust="bonferroni")

res.aov <- aov(arousal ~ a_cat + Error(subject), data = ratings)
# Summary of the analysis
summary(res.aov)
library("emmeans")
# set orthogonal contrasts
options(contrasts = c("contr.sum", "contr.poly"))
emm <- emmeans(res.aov, ~ a_cat)
pairs(emm, adjust="tukey")