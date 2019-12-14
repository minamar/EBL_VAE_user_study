# Individual accuracy rates 
library(dplyr) # includes dplyr
library(lmerTest)
library(FSA)
# Valence analysis with anova_test
library(tidyverse)
library(ggpubr)
library(rstatix)

ratings %>% 
  select(subject, valence, v_cat)  %>% 
  group_by(subject, v_cat) %>% 
  summarise(mean_v = mean(valence)) %>% 
  {. ->> v_ratings_aggr}

# Convert subject and v_cat into factor variables
v_ratings_aggr <- v_ratings_aggr %>%
  ungroup(subject) %>%
  # gather(key = "v_cat", value = "mean_v", Neg, Neu, Pos) %>%
  convert_as_factor(subject, v_cat)
head(v_ratings_aggr, 3)

# Summary stats
v_ratings_aggr %>%
  group_by(v_cat) %>%
  get_summary_stats(mean_v, type = "mean_sd")

# Visualization
v_bxp <- ggboxplot(v_ratings_aggr, x = "v_cat", y = "mean_v", add = "point")
v_bxp

# Identify outliers
v_ratings_aggr %>%
  group_by(subject) %>%
  identify_outliers(mean_v)

# Normality
v_ratings_aggr %>%
  group_by(v_cat) %>%
  shapiro_test(mean_v)

ggqqplot(v_ratings_aggr, "mean_v", facet.by = "v_cat")

# Anova comp
v_res.aov <- anova_test(data = v_ratings_aggr, dv = mean_v, wid = subject, within = v_cat)
get_anova_table(v_res.aov)

# Post-hoc tests
# pairwise comparisons
v_pwc <- v_ratings_aggr %>%
  pairwise_t_test(
    mean_v ~ v_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
v_pwc


# Arousal analysis with rstatix and anova_test
ratings %>% 
  select(subject, arousal, a_cat)  %>% 
  group_by(subject, a_cat) %>% 
  summarise(mean_a = mean(arousal)) %>% 
  {. ->> a_ratings_aggr}

# Convert subject and v_cat into factor variables
a_ratings_aggr <- a_ratings_aggr %>%
  ungroup(subject) %>%
  # gather(key = "v_cat", value = "mean_v", Neg, Neu, Pos) %>%
  convert_as_factor(subject, a_cat)
head(a_ratings_aggr, 3)

# Summary stats
a_ratings_aggr %>%
  group_by(a_cat) %>%
  get_summary_stats(mean_a, type = "mean_sd")

# Visualization
a_bxp <- ggboxplot(a_ratings_aggr, x = "a_cat", y = "mean_a", add = "point") 
a_bxp 

# Identify outliers
a_ratings_aggr %>%
  group_by(subject) %>%
  identify_outliers(mean_a)

# Normality
a_ratings_aggr %>%
  group_by(a_cat) %>%
  shapiro_test(mean_a)

ggqqplot(a_ratings_aggr, "mean_a", facet.by = "a_cat")

# Anova comp
a_res.aov <- anova_test(data = a_ratings_aggr, dv = mean_a, wid = subject, within = a_cat)
get_anova_table(a_res.aov)

# Post-hoc tests
# pairwise comparisons
a_pwc <- a_ratings_aggr %>%
  pairwise_t_test(
    mean_a ~ a_cat, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
a_pwc


# Visualization: box plots with p-values

v_pwc <- v_pwc %>% add_xy_position(x = "v_cat")
v_plot <- v_bxp + ylab("aggregated valence ratings") + xlab("valence conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25,  hide.ns = TRUE, y.position = c(0.88, 0.92), v_pwc) +
  theme_gray() + scale_x_discrete(labels = c('negative','neutral','positive')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/v_bxp.pdf", units="in", width=5, height=4, dpi=300)

a_pwc <- a_pwc %>% add_xy_position(x = "a_cat")
a_plot <- a_bxp + ylab("aggregated arousal ratings") + xlab("arousal conditioning") +
  stat_pvalue_manual(tip.length = 0.01, bracket.size = 0.25, hide.ns = TRUE, y.position = c(0.88, 0.92), a_pwc) +
  theme_gray() + scale_x_discrete(labels = c('low','medium','high')) + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#ggsave("images/a_bxp.pdf", units="in", width=5, height=4, dpi=300)

library(cowplot)
plot_grid(v_plot, a_plot, labels = "AUTO", ncol = 1)
ggsave("images/va_bxp_vert_cond.pdf", units="mm", width=80, height=150, dpi=300)








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