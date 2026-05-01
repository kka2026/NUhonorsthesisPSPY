# ---- MORAL DISENGAGEMENT

lm_moral_1 <- lm(moral_disengagement_outgroup ~ exp_group, data = pilot_clean)
lm_moral_2 <- lm(moral_disengagement_outgroup ~ exp_group
                          + maga + ideology_selfreport + PID_strength
                          + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
lm_moral_3 <- lm(moral_disengagement_outgroup ~ exp_group
                               + PID_strength + exp_group*PID_strength, data = pilot_clean)
lm_moral_4 <- lm(moral_disengagement_outgroup ~ exp_group
                           + maga + exp_group*maga, data = pilot_clean)
lm_moral_5 <- lm(moral_disengagement_outgroup ~ exp_group
                                    + PID_strength + maga
                                    + exp_group*PID_strength + exp_group*maga,
                                    data = pilot_clean)

starprep(lm_moral_1, lm_moral_2, lm_moral_3, lm_moral_4, lm_moral_5,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)

stargazer(lm_moral_1, lm_moral_2, lm_moral_3, lm_moral_4, lm_moral_5,
          type = "html",
          se = starprep(lm_moral_1, lm_moral_2, lm_moral_3, lm_moral_4, lm_moral_5),
          p  = starprep(lm_moral_1, lm_moral_2, lm_moral_3, lm_moral_4, lm_moral_5),
          title = "Table 5: Regression Results for Moral Disengagement",
          dep.var.labels = c("Moral Disengagement"),
          column.labels = c("Model 1: No Covariates", "Model 2: Covariates", "Model 3: PID Strength Interaction",
                            "Model 4: MAGA Interaction", "Model 5: PID Strength and MAGA Interactions"),
          covariate.labels = c("Treatment", "MAGA Support",
                               "Strength of Partisan Identity",
                               "Self-Reported Ideology",
                               "FT: Republican Party (Pre-Treatment)",
                               "FT: Democratic Party (Pre-Treatment",
                               "Trait Aggression",
                               "Treatment x PID Strength",
                               "Treatment x MAGA Support"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "Moral_Disengagement_Regression_Interactions.htm")



# ---- INDIVIDUAL SPV

lm_indiv_1 <- lm(SPV_individual ~ exp_group, data = pilot_clean)
lm_indiv_2 <- lm(SPV_individual ~ exp_group
                 + maga + ideology_selfreport + PID_strength
                 + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
lm_indiv_3 <- lm(SPV_individual ~ exp_group
                 + PID_strength + exp_group*PID_strength, data = pilot_clean)
lm_indiv_4 <- lm(SPV_individual ~ exp_group
                 + maga + exp_group*maga, data = pilot_clean)
lm_indiv_5 <- lm(SPV_individual ~ exp_group
                 + PID_strength + maga
                 + exp_group*PID_strength + exp_group*maga,
                 data = pilot_clean)

starprep(lm_indiv_1, lm_indiv_2, lm_indiv_3, lm_indiv_4, lm_indiv_5,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)

stargazer(lm_indiv_1, lm_indiv_2, lm_indiv_3, lm_indiv_4, lm_indiv_5,
          type = "html",
          se = starprep(lm_indiv_1, lm_indiv_2, lm_indiv_3, lm_indiv_4, lm_indiv_5),
          p  = starprep(lm_indiv_1, lm_indiv_2, lm_indiv_3, lm_indiv_4, lm_indiv_5),
          title = "Table 6: Regression Results for Individual SPV",
          dep.var.labels = c("Individual SPV"),
          column.labels = c("Model 1: No Covariates", "Model 2: Covariates", "Model 3: PID Strength Interaction",
                            "Model 4: MAGA Interaction", "Model 5: PID Strength and MAGA Interactions"),
          covariate.labels = c("Treatment", "MAGA Support",
                               "Strength of Partisan Identity",
                               "Self-Reported Ideology",
                               "FT: Republican Party (Pre-Treatment)",
                               "FT: Democratic Party (Pre-Treatment",
                               "Trait Aggression",
                               "Treatment x PID Strength",
                               "Treatment x MAGA Support"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "SPV_Individual_Regression_Interactions.htm")





# ---- META-PERCEPTIONS OUTGROUP

lm_out_1 <- lm(SPV_meta_outgroup ~ exp_group, data = pilot_clean)
lm_out_2 <- lm(SPV_meta_outgroup ~ exp_group
                 + maga + ideology_selfreport + PID_strength
                 + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
lm_out_3 <- lm(SPV_meta_outgroup ~ exp_group
                 + PID_strength + exp_group*PID_strength, data = pilot_clean)
lm_out_4 <- lm(SPV_meta_outgroup ~ exp_group
                 + maga + exp_group*maga, data = pilot_clean)
lm_out_5 <- lm(SPV_meta_outgroup ~ exp_group
                 + PID_strength + maga
                 + exp_group*PID_strength + exp_group*maga,
                 data = pilot_clean)

starprep(lm_out_1, lm_out_2, lm_out_3, lm_out_4, lm_out_5,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)

stargazer(lm_out_1, lm_out_2, lm_out_3, lm_out_4, lm_out_5,
          type = "html",
          se = starprep(lm_out_1, lm_out_2, lm_out_3, lm_out_4, lm_out_5),
          p  = starprep(lm_out_1, lm_out_2, lm_out_3, lm_out_4, lm_out_5),
          title = "Table 4: Regression Results for Meta-Perceptions of Out-Party SPV",
          dep.var.labels = c("Meta-Perceptions: Outgroup SPV"),
          column.labels = c("Model 1: No Covariates", "Model 2: Covariates", "Model 3: PID Strength Interaction",
                            "Model 4: MAGA Interaction", "Model 5: PID Strength and MAGA Interactions"),
          covariate.labels = c("Treatment", "MAGA Support",
                               "Strength of Partisan Identity",
                               "Self-Reported Ideology",
                               "FT: Republican Party (Pre-Treatment)",
                               "FT: Democratic Party (Pre-Treatment",
                               "Trait Aggression",
                               "Treatment x PID Strength",
                               "Treatment x MAGA Support"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "SPV_MetOut_Regression_Interactions.htm")



# ---- META-PERCEPTIONS INGROUP

lm_in_1 <- lm(SPV_meta_ingroup ~ exp_group, data = pilot_clean)
lm_in_2 <- lm(SPV_meta_ingroup ~ exp_group
               + maga + ideology_selfreport + PID_strength
               + ft_dem_pre_1 + ft_rep_pre_1 + trait_agg, data = pilot_clean)
lm_in_3 <- lm(SPV_meta_ingroup ~ exp_group
               + PID_strength + exp_group*PID_strength, data = pilot_clean)
lm_in_4 <- lm(SPV_meta_ingroup ~ exp_group
               + maga + exp_group*maga, data = pilot_clean)
lm_in_5 <- lm(SPV_meta_ingroup ~ exp_group
               + PID_strength + maga
               + exp_group*PID_strength + exp_group*maga,
               data = pilot_clean)

starprep(lm_in_1, lm_in_2, lm_in_3, lm_in_4, lm_in_5,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)

stargazer(lm_in_1, lm_in_2, lm_in_3, lm_in_4, lm_in_5,
          type = "html",
          se = starprep(lm_in_1, lm_in_2, lm_in_3, lm_in_4, lm_in_5),
          p  = starprep(lm_in_1, lm_in_2, lm_in_3, lm_in_4, lm_in_5),
          title = "Table 3: Regression Results for Meta-Perceptions of In-Party SPV",
          dep.var.labels = c("Meta-Perceptions: Ingroup SPV"),
          column.labels = c("Model 1: No Covariates", "Model 2: Covariates", "Model 3: PID Strength Interaction",
                            "Model 4: MAGA Interaction", "Model 5: PID Strength and MAGA Interactions"),
          covariate.labels = c("Treatment", "MAGA Support",
                               "Strength of Partisan Identity",
                               "Self-Reported Ideology",
                               "FT: Republican Party (Pre-Treatment)",
                               "FT: Democratic Party (Pre-Treatment",
                               "Trait Aggression",
                               "Treatment x PID Strength",
                               "Treatment x MAGA Support"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "SPV_MetaIn_Regression_Interactions.htm")

