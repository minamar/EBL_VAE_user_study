# Final formulas: multivariate, interaction 
form1 = mvbind(valence, arousal, dominance) ~  v_cat * a_cat + sex + age + (1|idAnim) + (1|subject)

form2 = mvbind(valence, arousal, dominance) ~  v_cat * a_cat + age + sex

form3 = mvbind(valence, arousal, dominance) ~  v_cat + a_cat + sex + age + (1|idAnim) + (1|subject)

form4 = mvbind(valence, arousal, dominance) ~  v_cat * a_cat + age + (1|idAnim) + (1|subject)

form5 = mvbind(valence, arousal, dominance) ~  v_cat * a_cat + sex + (1|idAnim) + (1|subject)

form6 = mvbind(valence, arousal) ~  v_cat * a_cat + sex + age + (1|idAnim) + (1|subject)
form7 = valence ~  v_cat * a_cat + sex + age + (1|idAnim) + (1|subject)

# OLder models
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
form10 = mvbind(valence, arousal) ~  v_cat * a_cat + age + anim_experience + sex + (1|p|idAnim) + (1|q|subject)
form11 = mvbind(valence, arousal) ~  a_cat * v_cat + age + anim_experience + sex + (1|p|idAnim) + (1|q|subject)
form12 = mvbind(valence, arousal) ~  v_cat * a_cat + age + anim_experience + sex 
form13 = mvbind(valence, arousal) ~  v_cat * a_cat + age + anim_experience + sex + (1|idAnim) + (1|subject)
form14 = mvbind(valence, arousal) ~  v_cat * a_cat + age + anim_experience + sex + (1+v_cat+a_cat|idAnim) + (1+v_cat+a_cat|subject)
form15 = mvbind(valence, arousal) ~  v_cat * a_cat + age + anim_experience + sex + (1+v_cat+a_cat|idAnim) + (1|subject)
form16 = mvbind(valence, arousal) ~  v_cat * a_cat + age + sex +  (1|p|idAnim) + (1|q|subject)
form17 = mvbind(valence, arousal) ~  v_cat * a_cat + sex +  (1|idAnim) + (1|subject)
form18 = bf(
  mvbind(valence, arousal) ~  v_cat * a_cat + age + sex +  (1|idAnim) + (1|subject),
  zoi ~ v_cat + a_cat
)

form19 = mvbind(valence, arousal) ~  v_cat * a_cat + sex + age + (1|idAnim) + (1|subject) # with informative priors
form20 = mvbind(valence, arousal) ~  v_cat * a_cat + sex + age + (1|idAnim) + (1|subject) # with informative priors
form21 = bf(
  mvbind(valence, arousal) ~  v_cat * a_cat + age + sex +  (1|idAnim) + (1|subject),
  phi ~ v_cat + a_cat
  zoi ~ v_cat + a_cat
  coi ~ v_cat + a_cat
  family = zero_one_inflated_beta()
)