ft_models <- list(
  "Model 1: Democratic Party" = model_ft_dem,
  "Model 2: Republican Party" = model_ft_rep
)
modelsummary(ft_models,
             title = "Regression Results for Post-Treatment Affective Ratings By Party",
             coef_rename = c("exp_group" = "Treatment",
             "ft_rep_pre_1" = "FT: Republican Party (Pre-Treatment)",
             "ft_dem_pre_1" = "FT: Democratic Party (Pre-Treatment)"),
             stars = TRUE,
             output = "Feeling Thermometers.html")

moral_models <- list(
  "Model 1: No Covariates" = lm_moral,
  "Model 2: Covariates" = lm_moral_cov,
  "Model 3: PID Strength Interaction" = model_strength_md,
  "Model 4: MAGA Interaction" = model_maga_md,
  "Model 5: PID Strength and MAGA Interactions" = model_strength_maga_md
)

modelsummary(moral_models,
             title = "Regression Results for Moral Disengagement",
             coef_rename = c("exp_group" = "Treatment", "maga" = "MAGA Support",
                             "PID_strength" = "Strength of Partisan Identity",
                             "ideology_selfreport" = "Self-Reported Ideology",
                             "ft_rep_pre_1" = "FT: Republican Party (Pre-Treatment)",
                             "ft_dem_pre_1" = "FT: Democratic Party (Pre-Treatment",
                             "trait_agg" = "Trait Aggression",
                             "exp_group x PID_strength" = "Treatment x PID Strength",
                             "exp_group x maga" = "Treatment x MAGA Support"),
             stars = TRUE,
             output = "Moral Disengagement Tables.html")


indiv_models <- list(
  "Model 1: No Covariates" = lm_indiv,
  "Model 2: Covariates" = lm_indiv_cov,
  "Model 3: PID Strength Interaction" = model_strength_indiv,
  "Model 4: MAGA Interaction" = model_maga_indiv,
  "Model 5: PID Strength and MAGA Interactions" = model_strength_maga_indiv
)

modelsummary(indiv_models,
             title = "Regression Results for Individual SPV",,
             coef_rename = c("exp_group" = "Treatment", "maga" = "MAGA Support",
                             "PID_strength" = "Strength of Partisan Identity",
                             "ideology_selfreport" = "Self-Reported Ideology",
                             "ft_rep_pre_1" = "FT: Republican Party (Pre-Treatment)",
                             "ft_dem_pre_1" = "FT: Democratic Party (Pre-Treatment",
                             "trait_agg" = "Trait Aggression",
                             "exp_group x PID_strength" = "Treatment x PID Strength",
                             "exp_group x maga" = "Treatment x MAGA Support"),
             stars = TRUE,
             output = "Individual SPV Tables.html")


meta_out_models <- list(
  "Model 1: No Covariates" = lm_meta_out,
  "Model 2: Covariates" = lm_meta_out_cov,
  "Model 3: PID Strength Interaction" = model_strength_meta_out,
  "Model 4: MAGA Interaction" = model_maga_meta_out,
  "Model 5: PID Strength and MAGA Interactions" = model_strength_maga_meta_out
)

modelsummary(meta_out_models,
             title = "Regression Results for Meta-Perceptions of Outgroup SPV",,
             coef_rename = c("exp_group" = "Treatment", "maga" = "MAGA Support",
                             "PID_strength" = "Strength of Partisan Identity",
                             "ideology_selfreport" = "Self-Reported Ideology",
                             "ft_rep_pre_1" = "FT: Republican Party (Pre-Treatment)",
                             "ft_dem_pre_1" = "FT: Democratic Party (Pre-Treatment",
                             "trait_agg" = "Trait Aggression",
                             "exp_group x PID_strength" = "Treatment x PID Strength",
                             "exp_group x maga" = "Treatment x MAGA Support"),
             stars = TRUE,
             output = "Meta SPV Out Tables.html")



meta_in_models <- list(
  "Model 1: No Covariates" = lm_meta_in,
  "Model 2: Covariates" = lm_meta_in_cov,
  "Model 3: PID Strength Interaction" = model_strength_meta_in,
  "Model 4: MAGA Interaction" = model_maga_meta_in,
  "Model 5: PID Strength and MAGA Interactions" = model_strength_maga_meta_in
)

modelsummary(meta_in_models,
             title = "Regression Results for Meta-Perceptions of Ingroup SPV",,
             coef_rename = c("exp_group" = "Treatment", "maga" = "MAGA Support",
                             "PID_strength" = "Strength of Partisan Identity",
                             "ideology_selfreport" = "Self-Reported Ideology",
                             "ft_rep_pre_1" = "FT: Republican Party (Pre-Treatment)",
                             "ft_dem_pre_1" = "FT: Democratic Party (Pre-Treatment",
                             "trait_agg" = "Trait Aggression",
                             "exp_group x PID_strength" = "Treatment x PID Strength",
                             "exp_group x maga" = "Treatment x MAGA Support"),
             stars = TRUE,
             output = "Meta SPV In Tables.html")