library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(stargazer)
library(effsize)
library(modelsummary)

source("CLN_PLT.R")
source("DESC_PLT.R")
source("EXPL_PLT.R")

# view cleaned data
View(pilot_clean)



# ------ MORAL DISENGAGEMENT
# linear model with no controls (sensitivity test)
lm_moral <- lm_robust(moral_disengagement_outgroup ~ exp_group, data = pilot_clean)
summary(lm_moral)

# moral disengagement ~ condition + covariates
lm_moral_cov <- lm_robust(moral_disengagement_outgroup ~ exp_group
                          + maga + ideology_selfreport + PID_strength
                          + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
summary(lm_moral_cov)

# ATE
ate_moral <- coef(lm_moral)["exp_group"]
print(ate_moral)
ate_moral_cov <- coef(lm_moral_cov)["exp_group"]
print(ate_moral_cov)

# COVARIATE HETEROGENEITY
model_strength_md <- lm_robust(moral_disengagement_outgroup ~ exp_group
                            + PID_strength + exp_group*PID_strength, data = pilot_clean)
summary(model_strength_md)
model_maga_md <- lm_robust(moral_disengagement_outgroup ~ exp_group
                        + maga + exp_group*maga, data = pilot_clean)
summary(model_maga_md)
model_strength_maga_md <- lm_robust(moral_disengagement_outgroup ~ exp_group
                                 + PID_strength + maga + exp_group*PID_strength + exp_group*maga, data = pilot_clean)
summary(model_strength_maga_md)




# ------- INDIVIDUAL SPV
# linear model with no controls (sensitivity test)
lm_indiv <- lm_robust(SPV_individual ~ exp_group, data = pilot_clean)
summary(lm_indiv)

# individual SPV ~ condition + covariates
lm_indiv_cov <- lm_robust(SPV_individual ~ exp_group
                          + maga + ideology_selfreport + PID_strength
                          + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
summary(lm_indiv_cov)

# ATE
ate_indiv <- coef(lm_indiv)["exp_group"]
print(ate_indiv)
ate_indiv_cov <- coef(lm_indiv_cov)["exp_group"]
print(ate_indiv_cov)

# COVARIATE HETEROGENEITY
model_strength_indiv <- lm_robust(SPV_individual ~ exp_group
                            + PID_strength + exp_group*PID_strength, data = pilot_clean)
summary(model_strength_indiv)
model_maga_indiv <- lm_robust(SPV_individual ~ exp_group
                        + maga + exp_group*maga, data = pilot_clean)
summary(model_maga_indiv)
model_strength_maga_indiv <- lm_robust(SPV_individual ~ exp_group
                                 + PID_strength + maga + exp_group*PID_strength + exp_group*maga, data = pilot_clean)
summary(model_strength_maga_indiv)




# ------ META-SPV OUTGROUP
# linear model with no controls (sensitivity test)
lm_meta_out <- lm_robust(SPV_meta_outgroup ~ exp_group, data = pilot_clean)
summary(lm_meta_out)

# meta-SPV outgroup ~ condition + covariates
lm_meta_out_cov <- lm_robust(SPV_meta_outgroup ~ exp_group
                             + maga + ideology_selfreport + PID_strength
                             + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
summary(lm_meta_out_cov)

# ATE
ate_meta_out <- coef(lm_meta_out)["exp_group"]
print(ate_meta_out)
ate_meta_out_cov <- coef(lm_meta_out_cov)["exp_group"]
print(ate_meta_out_cov)

# covariate heterogeneity
model_strength_meta_out <- lm_robust(SPV_meta_outgroup ~ exp_group
                            + PID_strength + exp_group*PID_strength, data = pilot_clean)
summary(model_strength_meta_out)
model_maga_meta_out <- lm_robust(SPV_meta_outgroup ~ exp_group
                        + maga + exp_group*maga, data = pilot_clean)
summary(model_maga_meta_out)
model_strength_maga_meta_out <- lm_robust(SPV_meta_outgroup ~ exp_group
                                 + PID_strength + maga + exp_group*PID_strength + exp_group*maga, data = pilot_clean)
summary(model_strength_maga_meta_out)


# ------ META-SPV INGROUP
# linear model with no controls (sensitivity test)
lm_meta_in <- lm_robust(SPV_meta_ingroup ~ exp_group, data = pilot_clean)
summary(lm_meta_in)

# meta-SPV ingroup ~ condition + covariates
lm_meta_in_cov <- lm_robust(SPV_meta_ingroup ~ exp_group
                            + maga + ideology_selfreport + PID_strength
                            + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
summary(lm_meta_in_cov)

# ATE
ate_meta_in <- coef(lm_meta_in)["exp_group"]
print(ate_meta_in)
ate_meta_in_cov <- coef(lm_meta_in_cov)["exp_group"]
print(ate_meta_in_cov)

# covariate heterogeneity
model_strength_meta_in <- lm_robust(SPV_meta_ingroup ~ exp_group
                            + PID_strength + exp_group*PID_strength, data = pilot_clean)
summary(model_strength_meta_in)
model_maga_meta_in <- lm_robust(SPV_meta_ingroup ~ exp_group
                        + maga + exp_group*maga, data = pilot_clean)
summary(model_maga_meta_in)
model_strength_maga_meta_in <- lm_robust(SPV_meta_ingroup ~ exp_group
                                 + PID_strength + maga + exp_group*PID_strength + exp_group*maga, data = pilot_clean)
summary(model_strength_maga_meta_in)

