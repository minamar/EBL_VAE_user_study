# 28: 'des_pre',
# 29: 'gen_pre',
# 30: 'des_post',
# 31: 'gen_post'

library(likert)
library(psych)
library(tidyverse)

# Anthropomorphism 
des_pre <- select(surv, -X, -starts_with("animacy"))
des_pre <-filter(des_pre, condition == 28)
des_pre <- select(des_pre, -condition)

gen_pre <- select(surv, -X, -starts_with("animacy"))
gen_pre <-filter(gen_pre, condition == 29)
gen_pre <- select(gen_pre, -condition)

des_post <- select(surv, -X, -starts_with("animacy"))
des_post <-filter(des_post, condition == 30)
des_post <- select(des_post, -condition)

gen_post <- select(surv, -X, -starts_with("animacy"))
gen_post <-filter(gen_post, condition == 31)
gen_post <- select(gen_post, -condition)

# Cronbach's alpha for anthropomorphism
alpha(select(des_pre, -subject))
alpha(select(gen_pre, -subject))
alpha(select(des_post, -subject))
alpha(select(gen_post, -subject))


# Animacy 
des_pre <- select(surv, -X, -starts_with("anth"))
des_pre <-filter(des_pre, condition == 28)
des_pre <- select(des_pre, -condition)

gen_pre <- select(surv, -X, -starts_with("anth"))
gen_pre <-filter(gen_pre, condition == 29)
gen_pre <- select(gen_pre, -condition)

des_post <- select(surv, -X, -starts_with("anth"))
des_post <-filter(des_post, condition == 30)
des_post <- select(des_post, -condition)

gen_post <- select(surv, -X, -starts_with("anth"))
gen_post <-filter(gen_post, condition == 31)
gen_post <- select(gen_post, -condition)

# Cronbach's alpha for Animacy
alpha(select(des_pre, -subject))
alpha(select(gen_pre, -subject))
alpha(select(des_post, -subject))
alpha(select(gen_post, -subject))
