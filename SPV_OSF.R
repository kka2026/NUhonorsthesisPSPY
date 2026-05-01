source("CLN_OSF.R")
source("DESC_OSF.R")
# NOTE!!!! MAKE SURE THAT IF YOU ARE TESTING THIS OUT, THAT DATA_GROUP IS IN PLACE OF DATA_FILTER!!!! OTHERWISE IT WILL SAY ERROR!!!
source("AYL_OSF.R")


# |--------------------------------|
#            PROTESTING
# |--------------------------------|
# 1.1: form models for:
# A: Individual SPV (Aggregated)
lm_protest_indiv <- lm_robust(SPV_indiv_protest ~ condition_dummy * PID_dummy * PID_bin 
                              + condition_dummy*social_closeness
                              + ideology_selfreport
                              + maga,
                              data = data_clean, se_type = "HC2")
# B: Meta-Perceptions of Republican SPV 
lm_protest_meta_R <- lm_robust(SPV_meta_protest ~ condition_dummy * PID_dummy * PID_bin 
                               + condition_dummy*social_closeness
                               + ideology_selfreport
                               + maga,
                               data = data_clean, se_type = "HC2")
# C: Individual SPV (Reps only)
lm_R_protest <- lm_robust(spv_Rprotest ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# D: Individual SPV (Dems only)
lm_D_protest <- lm_robust(spv_Dprotest ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# 1.2: summarize models
summary(lm_protest_indiv)
summary(lm_protest_meta_R)
summary(lm_R_protest)
summary(lm_D_protest)

# 1.3: form emmeans and contrast matrices for Individual SPV (Aggregated)
emm_protest_indiv <- emmeans(lm_protest_indiv, ~ condition_dummy * PID_dummy,
                             vcov. = vcov(lm_protest_indiv))
emm_protest_indiv_df <- as.data.frame(confint(emm_protest_indiv)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_protest_indiv <- contrast(emm_protest_indiv,
                                method = list(
                                  "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                  "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                  "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                  "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_protest_indiv_df <- as.data.frame(confint(contr_protest_indiv))
contr_table_protest_indiv <- summary(contr_protest_indiv) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_protest_indiv, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Protesting Without a Permit) by Condition x Party ID",
          digits = 3, out = "protest_indiv_contrasts.htm")

# 1.4: form emmeans and contrast matrices for Meta-Perceptions of Republican SPV
emm_protest_meta_R <- emmeans(lm_protest_meta_R, ~ condition_dummy * PID_dummy,
                              vcov. = vcov(lm_protest_meta_R))
emm_protest_meta_R_df <- as.data.frame(confint(emm_protest_meta_R)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_protest_meta_R <- contrast(emm_protest_meta_R,
                                 method = list(
                                   "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                   "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                   "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                   "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_protest_meta_R_df <- as.data.frame(confint(contr_protest_meta_R))
contr_table_protest_meta_R <- summary(contr_protest_meta_R) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_protest_meta_R, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Protesting Without a Permit) by Condition x Party ID",
          digits = 3, out = "protest_meta_R_contrasts.htm")

# 1.5: same as 1.3 but using party_numeric
protest_indiv_partyaffil <- lm_robust(SPV_indiv_protest ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_protest_indiv_partyaffil<- emmeans(protest_indiv_partyaffil, ~ condition_dummy * party_numeric,
                                       at = list(party_numeric = 1:6),
                                       vcov. = vcov(protest_indiv_partyaffil))
contr_protest_indiv_partyaffil<- contrast(emm_protest_indiv_partyaffil,
                                          method = list("Treatment - Control" = c(-1, 1)),
                                          by = "party_numeric")
contr_protest_indiv_partyaffil_df <- confint(contr_protest_indiv_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Individual Ratings")

# 1.6: same as 1.4 but using party_numeric
protest_meta_R_partyaffil <- lm_robust(SPV_meta_protest ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_protest_meta_R_partyaffil<- emmeans(protest_meta_R_partyaffil, ~ condition_dummy * party_numeric,
                                        at = list(party_numeric = 1:6),
                                        vcov. = vcov(protest_meta_R_partyaffil))
contr_protest_meta_R_partyaffil<- contrast(emm_protest_meta_R_partyaffil,
                                           method = list("Treatment - Control" = c(-1, 1)),
                                           by = "party_numeric")
contr_protest_meta_R_partyaffil_df <- confint(contr_protest_meta_R_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Meta-Perceptions of Republicans' Ratings")

# 1.7: PLANNED CONTRASTS BY 7-PT SCALE
# A: INDIVIDUAL
lm_protest_indiv_R_7 <- lm_robust(SPV_indiv_protest ~ condition_dummy * party_numeric * PID_bin 
                                  + condition_dummy*social_closeness
                                  + ideology_selfreport
                                  + maga,
                                  data = data_clean, se_type = "HC2")
summary(lm_protest_indiv_R_7)
emm_protest_indiv_R_7<- emmeans(lm_protest_indiv_R_7, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(lm_protest_indiv_R_7))
contr_protest_indiv_R_7<- contrast(emm_protest_indiv_R_7,
                                   method = list("Treatment - Control" = c(-1, 1)),
                                   by = "party_numeric")
contr_protest_indiv_R_7_df <- as.data.frame(confint(contr_protest_indiv_R_7))
contr_table <- summary(contr_protest_indiv_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Protesting Without a Permit) by Condition x 7-Point Party ID",
          digits = 3, out = "protest_indiv_R_7_contrasts.htm")

# B: META_PERCEPTIONS
lm_protest_meta_R_7 <- lm_robust(SPV_meta_protest ~ condition_dummy * party_numeric * PID_bin 
                                 + condition_dummy*social_closeness
                                 + ideology_selfreport
                                 + maga,
                                 data = data_clean, se_type = "HC2")
summary(lm_protest_meta_R_7)
emm_protest_meta_R_7<- emmeans(lm_protest_meta_R_7, ~ condition_dummy | party_numeric,
                               at = list(party_numeric = 1:6),
                               vcov. = vcov(lm_protest_meta_R_7))
contr_protest_meta_R_7<- contrast(emm_protest_meta_R_7,
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_protest_meta_R_7_df <- as.data.frame(confint(contr_protest_meta_R_7))
contr_table <- summary(contr_protest_meta_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Protesting Without a Permit) by Condition x 7-Point Party ID",
          digits = 3, out = "protest_meta_R_7_contrasts.htm")

# 1.8: combine matrices and form visual contrasts plot
protest_combined_df <- bind_rows(contr_protest_indiv_partyaffil_df, contr_protest_meta_R_partyaffil_df)

ggplot(protest_combined_df, aes(x = party_numeric, y = estimate,
                                color = model, group = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Individual Ratings" = "salmon",
    "Meta-Perceptions of Republicans' Ratings" = "firebrick")) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Support for Political Violence: Protesting Without a Permit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1.9: export models
indiv_protest_lm <- lm(SPV_indiv_protest ~ condition_dummy * PID_dummy * PID_bin 
                       + condition_dummy*social_closeness
                       + ideology_selfreport
                       + maga,
                       data = data_clean)
R_protest_lm <- lm(spv_Rprotest ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
D_protest_lm <- lm(spv_Dprotest ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
meta_R_protest_lm <- lm(SPV_meta_protest ~ condition_dummy * PID_dummy * PID_bin 
                        + condition_dummy*social_closeness
                        + ideology_selfreport
                        + maga,
                        data = data_clean)
starprep(indiv_protest_lm, R_protest_lm, D_protest_lm, meta_R_protest_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
# A: individual (aggregated) v. meta
stargazer(indiv_protest_lm, meta_R_protest_lm,
          type = "html",
          se = starprep(indiv_protest_lm, meta_R_protest_lm),
          p  = starprep(indiv_protest_lm, meta_R_protest_lm),
          title = "Support for Political Violence: Protesting Without a Permit",
          dep.var.labels  = c("Individual Ratings (Aggregated)", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Protest_Group.htm")
# B: Republicans v. Democrats v. meta
stargazer(R_protest_lm, D_protest_lm, meta_R_protest_lm,
          type = "html",
          se = starprep(R_protest_lm, D_protest_lm, meta_R_protest_lm),
          p  = starprep(R_protest_lm, D_protest_lm, meta_R_protest_lm),
          title = "Support for Political Violence: Protesting Without a Permit",
          dep.var.labels  = c("Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          out = "Protest_Party.htm")
# C: all models   
stargazer(indiv_protest_lm, R_protest_lm, D_protest_lm, meta_R_protest_lm,
          type = "html",
          se = starprep(indiv_protest_lm, R_protest_lm, D_protest_lm, meta_R_protest_lm),
          p  = starprep(indiv_protest_lm, R_protest_lm, D_protest_lm, meta_R_protest_lm),
          title = "Support for Political Violence: Protesting Without a Permit",
          dep.var.labels  = c("Individual Ratings (Aggregated)", "Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          out = "Protest_All.htm")


# |--------------------------------|
#            VANDALISM
# |--------------------------------|
# 2.1: form models for:
# A: Individual SPV (Aggregated)
lm_vandalize_indiv <- lm_robust(SPV_indiv_vandalize ~ condition_dummy * PID_dummy * PID_bin 
                                + condition_dummy*social_closeness
                                + ideology_selfreport
                                + maga,
                                data = data_clean, se_type = "HC2")
# B: Meta-Perceptions of Republican SPV 
lm_vandalize_meta_R <- lm_robust(SPV_meta_vandalize ~ condition_dummy * PID_dummy * PID_bin 
                                 + condition_dummy*social_closeness
                                 + ideology_selfreport
                                 + maga,
                                 data = data_clean, se_type = "HC2")
# C: Individual SPV (Reps only)
lm_R_vandalize <- lm_robust(spv_Rvandalize ~ condition_dummy*PID_bin*social_closeness
                            + ideology_selfreport
                            + maga,
                            data = data_clean, se_type = "HC2")
# D: Individual SPV (Dems only)
lm_D_vandalize <- lm_robust(spv_Dvandalize ~ condition_dummy*PID_bin*social_closeness
                            + ideology_selfreport
                            + maga,
                            data = data_clean, se_type = "HC2")
# 2.2: summarize models
summary(lm_vandalize_indiv)
summary(lm_vandalize_meta_R)
summary(lm_R_vandalize)
summary(lm_D_vandalize)

# 2.3: form emmeans and contrast matrices for Individual SPV (Aggregated)
emm_vandalize_indiv <- emmeans(lm_vandalize_indiv, ~ condition_dummy * PID_dummy,
                               vcov. = vcov(lm_vandalize_indiv))
emm_vandalize_indiv_df <- as.data.frame(confint(emm_vandalize_indiv)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_vandalize_indiv <- contrast(emm_vandalize_indiv,
                                  method = list(
                                    "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                    "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                    "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                    "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_vandalize_indiv_df <- as.data.frame(confint(contr_vandalize_indiv))
contr_table_vandalize_indiv <- summary(contr_vandalize_indiv) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_vandalize_indiv, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Vandalizing Opposing Party's Signs) by Condition x Party ID",
          digits = 3, out = "vandalize_indiv_contrasts.htm")

# 2.4: form emmeans and contrast matrices for Meta-Perceptions of Republican SPV
emm_vandalize_meta_R <- emmeans(lm_vandalize_meta_R, ~ condition_dummy * PID_dummy,
                                vcov. = vcov(lm_vandalize_meta_R))
emm_vandalize_meta_R_df <- as.data.frame(confint(emm_vandalize_meta_R)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_vandalize_meta_R <- contrast(emm_vandalize_meta_R,
                                   method = list(
                                     "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                     "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                     "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                     "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_vandalize_meta_R_df <- as.data.frame(confint(contr_vandalize_meta_R))
contr_table_vandalize_meta_R <- summary(contr_vandalize_meta_R) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_vandalize_meta_R, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Vandalizing Opposing Party's Signs) by Condition x Party ID",
          digits = 3, out = "vandalize_meta_R_contrasts.htm")

# 2.5: same as 2.3 but using party_numeric
vandalize_indiv_partyaffil <- lm_robust(SPV_indiv_vandalize ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_vandalize_indiv_partyaffil<- emmeans(vandalize_indiv_partyaffil, ~ condition_dummy * party_numeric,
                                         at = list(party_numeric = 1:6),
                                         vcov. = vcov(vandalize_indiv_partyaffil))
contr_vandalize_indiv_partyaffil<- contrast(emm_vandalize_indiv_partyaffil,
                                            method = list("Treatment - Control" = c(-1, 1)),
                                            by = "party_numeric")
contr_vandalize_indiv_partyaffil_df <- confint(contr_vandalize_indiv_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Individual Ratings")

# 2.6: same as 2.4 but using party_numeric
vandalize_meta_R_partyaffil <- lm_robust(SPV_meta_vandalize ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_vandalize_meta_R_partyaffil<- emmeans(vandalize_meta_R_partyaffil, ~ condition_dummy * party_numeric,
                                          at = list(party_numeric = 1:6),
                                          vcov. = vcov(vandalize_meta_R_partyaffil))
contr_vandalize_meta_R_partyaffil<- contrast(emm_vandalize_meta_R_partyaffil,
                                             method = list("Treatment - Control" = c(-1, 1)),
                                             by = "party_numeric")
contr_vandalize_meta_R_partyaffil_df <- confint(contr_vandalize_meta_R_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Meta-Perceptions of Republicans' Ratings")

# 2.7: PLANNED CONTRASTS BY 7-PT SCALE
# A: INDIVIDUAL
lm_vandalize_indiv_R_7 <- lm_robust(SPV_indiv_vandalize ~ condition_dummy * party_numeric * PID_bin 
                                    + condition_dummy*social_closeness
                                    + ideology_selfreport
                                    + maga,
                                    data = data_clean, se_type = "HC2")
summary(lm_vandalize_indiv_R_7)
emm_vandalize_indiv_R_7<- emmeans(lm_vandalize_indiv_R_7, ~ condition_dummy | party_numeric,
                                  at = list(party_numeric = 1:6),
                                  vcov. = vcov(lm_vandalize_indiv_R_7))
contr_vandalize_indiv_R_7<- contrast(emm_vandalize_indiv_R_7,
                                     method = list("Treatment - Control" = c(-1, 1)),
                                     by = "party_numeric")
contr_vandalize_indiv_R_7_df <- as.data.frame(confint(contr_vandalize_indiv_R_7))
contr_table <- summary(contr_vandalize_indiv_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Vandalizing Opposing Party's Signs) by Condition x 7-Point Party ID",
          digits = 3, out = "vandalize_indiv_R_7_contrasts.htm")

# B: META_PERCEPTIONS
lm_vandalize_meta_R_7 <- lm_robust(SPV_meta_vandalize ~ condition_dummy * party_numeric * PID_bin 
                                   + condition_dummy*social_closeness
                                   + ideology_selfreport
                                   + maga,
                                   data = data_clean, se_type = "HC2")
summary(lm_vandalize_meta_R_7)
emm_vandalize_meta_R_7<- emmeans(lm_vandalize_meta_R_7, ~ condition_dummy | party_numeric,
                                 at = list(party_numeric = 1:6),
                                 vcov. = vcov(lm_vandalize_meta_R_7))
contr_vandalize_meta_R_7<- contrast(emm_vandalize_meta_R_7,
                                    method = list("Treatment - Control" = c(-1, 1)),
                                    by = "party_numeric")
contr_vandalize_meta_R_7_df <- as.data.frame(confint(contr_vandalize_meta_R_7))
contr_table <- summary(contr_vandalize_meta_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Vandalizing Opposing Party's Signs) by Condition x 7-Point Party ID",
          digits = 3, out = "vandalize_meta_R_7_contrasts.htm")  

# 2.8: combine matrices and form visual contrasts plot
vandalize_combined_df <- bind_rows(contr_vandalize_indiv_partyaffil_df, contr_vandalize_meta_R_partyaffil_df)

ggplot(vandalize_combined_df, aes(x = party_numeric, y = estimate,
                                  color = model, group = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Individual Ratings" = "peachpuff",
    "Meta-Perceptions of Republicans' Ratings" = "coral")) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Support for Political Violence: Vandalizing Opposing Party's Signs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2.9: export models
indiv_vandalize_lm <- lm(SPV_indiv_vandalize ~ condition_dummy * PID_dummy * PID_bin 
                         + condition_dummy*social_closeness
                         + ideology_selfreport
                         + maga,
                         data = data_clean)
R_vandalize_lm <- lm(spv_Rvandalize ~ condition_dummy*PID_bin*social_closeness
                     + ideology_selfreport
                     + maga,
                     data = data_clean)
D_vandalize_lm <- lm(spv_Dvandalize ~ condition_dummy*PID_bin*social_closeness
                     + ideology_selfreport
                     + maga,
                     data = data_clean)
meta_R_vandalize_lm <- lm(SPV_meta_vandalize ~ condition_dummy * PID_dummy * PID_bin 
                          + condition_dummy*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean)
starprep(indiv_vandalize_lm, R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
# A: individual (aggregated) v. meta
stargazer(indiv_vandalize_lm, meta_R_vandalize_lm,
          type = "html",
          se = starprep(indiv_vandalize_lm, meta_R_vandalize_lm),
          p  = starprep(indiv_vandalize_lm, meta_R_vandalize_lm),
          title = "Support for Political Violence: Vandalizing Opposing Party's Signs",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Vandalize_Group.htm")
# B: Republicans v. Democrats v. meta
stargazer(R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm,
          type = "html",
          se = starprep(R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm),
          p  = starprep(R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm),
          title = "Support for Political Violence: Vandalizing Opposing Party's Signs",
          dep.var.labels = c("Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Vandalize_Party.htm")
# C: all models   
stargazer(indiv_vandalize_lm, R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm,
          type = "html",
          se = starprep(indiv_vandalize_lm, R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm),
          p  = starprep(indiv_vandalize_lm, R_vandalize_lm, D_vandalize_lm, meta_R_vandalize_lm),
          title = "Support for Political Violence: Vandalizing Opposing Party's Signs",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Vandalize_All.htm")


# |--------------------------------|
#              MESSAGE
# |--------------------------------|
# 3.1: form models for:
# A: Individual SPV (Aggregated)
lm_message_indiv <- lm_robust(SPV_indiv_message ~ condition_dummy * PID_dummy * PID_bin 
                              + condition_dummy*social_closeness
                              + ideology_selfreport
                              + maga,
                              data = data_clean, se_type = "HC2")
# B: Meta-Perceptions of Republican SPV 
lm_message_meta_R <- lm_robust(SPV_meta_message ~ condition_dummy * PID_dummy * PID_bin 
                               + condition_dummy*social_closeness
                               + ideology_selfreport
                               + maga,
                               data = data_clean, se_type = "HC2")
# C: Individual SPV (Reps only)
lm_R_message <- lm_robust(spv_Rmessage ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# D: Individual SPV (Dems only)
lm_D_message <- lm_robust(spv_Dmessage ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# 3.2: summarize models
summary(lm_message_indiv)
summary(lm_message_meta_R)
summary(lm_R_message)
summary(lm_D_message)

# 3.3: form emmeans and contrast matrices for Individual SPV (Aggregated)
emm_message_indiv <- emmeans(lm_message_indiv, ~ condition_dummy * PID_dummy,
                             vcov. = vcov(lm_message_indiv))
emm_message_indiv_df <- as.data.frame(confint(emm_message_indiv)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_message_indiv <- contrast(emm_message_indiv,
                                method = list(
                                  "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                  "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                  "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                  "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_message_indiv_df <- as.data.frame(confint(contr_message_indiv))
contr_table_message_indiv <- summary(contr_message_indiv) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_message_indiv, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Messaging Threats Online to Out-Party Representative) by Condition x Party ID",
          digits = 3, out = "message_indiv_contrasts.htm")

# 3.4: form emmeans and contrast matrices for Meta-Perceptions of Republican SPV
emm_message_meta_R <- emmeans(lm_message_meta_R, ~ condition_dummy * PID_dummy,
                              vcov. = vcov(lm_message_meta_R))
emm_message_meta_R_df <- as.data.frame(confint(emm_message_meta_R)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_message_meta_R <- contrast(emm_message_meta_R,
                                 method = list(
                                   "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                   "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                   "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                   "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_message_meta_R_df <- as.data.frame(confint(contr_message_meta_R))
contr_table_message_meta_R <- summary(contr_message_meta_R) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_message_meta_R, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Messaging Threats Online to Out-Party Representative) by Condition x Party ID",
          digits = 3, out = "message_meta_R_contrasts.htm")

# 3.5: same as 3.3 but using party_numeric
message_indiv_partyaffil <- lm_robust(SPV_indiv_message ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_message_indiv_partyaffil<- emmeans(message_indiv_partyaffil, ~ condition_dummy * party_numeric,
                                       at = list(party_numeric = 1:6),
                                       vcov. = vcov(message_indiv_partyaffil))
contr_message_indiv_partyaffil<- contrast(emm_message_indiv_partyaffil,
                                          method = list("Treatment - Control" = c(-1, 1)),
                                          by = "party_numeric")
contr_message_indiv_partyaffil_df <- confint(contr_message_indiv_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Individual Ratings")

# 3.6: same as 3.4 but using party_numeric
message_meta_R_partyaffil <- lm_robust(SPV_meta_message ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_message_meta_R_partyaffil<- emmeans(message_meta_R_partyaffil, ~ condition_dummy * party_numeric,
                                        at = list(party_numeric = 1:6),
                                        vcov. = vcov(message_meta_R_partyaffil))
contr_message_meta_R_partyaffil<- contrast(emm_message_meta_R_partyaffil,
                                           method = list("Treatment - Control" = c(-1, 1)),
                                           by = "party_numeric")
contr_message_meta_R_partyaffil_df <- confint(contr_message_meta_R_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Meta-Perceptions of Republicans' Ratings")

# 3.7: PLANNED CONTRASTS BY 7-PT SCALE
# A: INDIVIDUAL
lm_message_indiv_R_7 <- lm_robust(SPV_indiv_message ~ condition_dummy * party_numeric * PID_bin 
                                  + condition_dummy*social_closeness
                                  + ideology_selfreport
                                  + maga,
                                  data = data_clean, se_type = "HC2")
summary(lm_message_indiv_R_7)
emm_message_indiv_R_7<- emmeans(lm_message_indiv_R_7, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(lm_message_indiv_R_7))
contr_message_indiv_R_7<- contrast(emm_message_indiv_R_7,
                                   method = list("Treatment - Control" = c(-1, 1)),
                                   by = "party_numeric")
contr_message_indiv_R_7_df <- as.data.frame(confint(contr_message_indiv_R_7))
contr_table <- summary(contr_message_indiv_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Messaging Threats Online to Out-Party Representative) by Condition x 7-Point Party ID",
          digits = 3, out = "message_indiv_R_7_contrasts.htm")

# B: META_PERCEPTIONS
lm_message_meta_R_7 <- lm_robust(SPV_meta_message ~ condition_dummy * party_numeric * PID_bin 
                                 + condition_dummy*social_closeness
                                 + ideology_selfreport
                                 + maga,
                                 data = data_clean, se_type = "HC2")
summary(lm_message_meta_R_7)
emm_message_meta_R_7<- emmeans(lm_message_meta_R_7, ~ condition_dummy | party_numeric,
                               at = list(party_numeric = 1:6),
                               vcov. = vcov(lm_message_meta_R_7))
contr_message_meta_R_7<- contrast(emm_message_meta_R_7,
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_message_meta_R_7_df <- as.data.frame(confint(contr_message_meta_R_7))
contr_table <- summary(contr_message_meta_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Messaging Threats Online to Out-Party Representative) by Condition x 7-Point Party ID",
          digits = 3, out = "message_meta_R_7_contrasts.htm")  

# 3.8: combine matrices and form visual contrasts plot
message_combined_df <- bind_rows(contr_message_indiv_partyaffil_df, contr_message_meta_R_partyaffil_df)

ggplot(message_combined_df, aes(x = party_numeric, y = estimate,
                                color = model, group = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Individual Ratings" = "gold",
    "Meta-Perceptions of Republicans' Ratings" = "goldenrod")) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Support for Political Violence: Messaging Threats Online to Out-Party Representative") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.9: export models
indiv_message_lm <- lm(SPV_indiv_message ~ condition_dummy * PID_dummy * PID_bin 
                       + condition_dummy*social_closeness
                       + ideology_selfreport
                       + maga,
                       data = data_clean)
R_message_lm <- lm(spv_Rmessage ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
D_message_lm <- lm(spv_Dmessage ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
meta_R_message_lm <- lm(SPV_meta_message ~ condition_dummy * PID_dummy * PID_bin 
                        + condition_dummy*social_closeness
                        + ideology_selfreport
                        + maga,
                        data = data_clean)
starprep(indiv_message_lm, R_message_lm, D_message_lm, meta_R_message_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
# A: individual (aggregated) v. meta
stargazer(indiv_message_lm, meta_R_message_lm,
          type = "html",
          se = starprep(indiv_message_lm, meta_R_message_lm),
          p  = starprep(indiv_message_lm, meta_R_message_lm),
          title = "Support for Political Violence: Messaging Threats Online to Out-Party Representative",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Message_Group.htm")
# B: Republicans v. Democrats v. meta
stargazer(R_message_lm, D_message_lm, meta_R_message_lm,
          type = "html",
          se = starprep(R_message_lm, D_message_lm, meta_R_message_lm),
          p  = starprep(R_message_lm, D_message_lm, meta_R_message_lm),
          title = "Support for Political Violence: Messaging Threats Online to Out-Party Representative",
          dep.var.labels = c("Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Message_Party.htm")
# C: all models   
stargazer(indiv_message_lm, R_message_lm, D_message_lm, meta_R_message_lm,
          type = "html",
          se = starprep(indiv_message_lm, R_message_lm, D_message_lm, meta_R_message_lm),
          p  = starprep(indiv_message_lm, R_message_lm, D_message_lm, meta_R_message_lm),
          title = "Support for Political Violence: Messaging Threats Online to Out-Party Representative",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Message_All.htm")


# |--------------------------------|
#            ASSAULT
# |--------------------------------|
# 4.1: form models for:
# A: Individual SPV (Aggregated)
lm_assault_indiv <- lm_robust(SPV_indiv_assault ~ condition_dummy * PID_dummy * PID_bin 
                              + condition_dummy*social_closeness
                              + ideology_selfreport
                              + maga,
                              data = data_clean, se_type = "HC2")
# B: Meta-Perceptions of Republican SPV 
lm_assault_meta_R <- lm_robust(SPV_meta_assault ~ condition_dummy * PID_dummy * PID_bin 
                               + condition_dummy*social_closeness
                               + ideology_selfreport
                               + maga,
                               data = data_clean, se_type = "HC2")
# C: Individual SPV (Reps only)
lm_R_assault <- lm_robust(spv_Rassault ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# D: Individual SPV (Dems only)
lm_D_assault <- lm_robust(spv_Dassault ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# 4.2: summarize models
summary(lm_assault_indiv)
summary(lm_assault_meta_R)
summary(lm_R_assault)
summary(lm_D_assault)

# 4.3: form emmeans and contrast matrices for Individual SPV (Aggregated)
emm_assault_indiv <- emmeans(lm_assault_indiv, ~ condition_dummy * PID_dummy,
                             vcov. = vcov(lm_assault_indiv))
emm_assault_indiv_df <- as.data.frame(confint(emm_assault_indiv)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_assault_indiv <- contrast(emm_assault_indiv,
                                method = list(
                                  "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                  "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                  "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                  "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_assault_indiv_df <- as.data.frame(confint(contr_assault_indiv))
contr_table_assault_indiv <- summary(contr_assault_indiv) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_assault_indiv, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Physically Assaulting an Out-Partisan) by Condition x Party ID",
          digits = 3, out = "assault_indiv_contrasts.htm")

# 4.4: form emmeans and contrast matrices for Meta-Perceptions of Republican SPV
emm_assault_meta_R <- emmeans(lm_assault_meta_R, ~ condition_dummy * PID_dummy,
                              vcov. = vcov(lm_assault_meta_R))
emm_assault_meta_R_df <- as.data.frame(confint(emm_assault_meta_R)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_assault_meta_R <- contrast(emm_assault_meta_R,
                                 method = list(
                                   "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                   "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                   "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                   "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_assault_meta_R_df <- as.data.frame(confint(contr_assault_meta_R))
contr_table_assault_meta_R <- summary(contr_assault_meta_R) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_assault_meta_R, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Physically Assaulting an Out-Partisan) by Condition x Party ID",
          digits = 3, out = "assault_meta_R_contrasts.htm")

# 4.5: same as 4.3 but using party_numeric
assault_indiv_partyaffil <- lm_robust(SPV_indiv_assault ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_assault_indiv_partyaffil<- emmeans(assault_indiv_partyaffil, ~ condition_dummy * party_numeric,
                                       at = list(party_numeric = 1:6),
                                       vcov. = vcov(assault_indiv_partyaffil))
contr_assault_indiv_partyaffil<- contrast(emm_assault_indiv_partyaffil,
                                          method = list("Treatment - Control" = c(-1, 1)),
                                          by = "party_numeric")
contr_assault_indiv_partyaffil_df <- confint(contr_assault_indiv_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Individual Ratings")

# 4.6: same as 4.4 but using party_numeric
assault_meta_R_partyaffil <- lm_robust(SPV_meta_assault ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_assault_meta_R_partyaffil<- emmeans(assault_meta_R_partyaffil, ~ condition_dummy * party_numeric,
                                        at = list(party_numeric = 1:6),
                                        vcov. = vcov(assault_meta_R_partyaffil))
contr_assault_meta_R_partyaffil<- contrast(emm_assault_meta_R_partyaffil,
                                           method = list("Treatment - Control" = c(-1, 1)),
                                           by = "party_numeric")
contr_assault_meta_R_partyaffil_df <- confint(contr_assault_meta_R_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Meta-Perceptions of Republicans' Ratings")

# 4.7: PLANNED CONTRASTS BY 7-PT SCALE
# A: INDIVIDUAL
lm_assault_indiv_R_7 <- lm_robust(SPV_indiv_assault ~ condition_dummy * party_numeric * PID_bin 
                                  + condition_dummy*social_closeness
                                  + ideology_selfreport
                                  + maga,
                                  data = data_clean, se_type = "HC2")
summary(lm_assault_indiv_R_7)
emm_assault_indiv_R_7<- emmeans(lm_assault_indiv_R_7, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(lm_assault_indiv_R_7))
contr_assault_indiv_R_7<- contrast(emm_assault_indiv_R_7,
                                   method = list("Treatment - Control" = c(-1, 1)),
                                   by = "party_numeric")
contr_assault_indiv_R_7_df <- as.data.frame(confint(contr_assault_indiv_R_7))
contr_table <- summary(contr_assault_indiv_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Physically Assaulting an Out-Partisan) by Condition x 7-Point Party ID",
          digits = 3, out = "assault_indiv_R_7_contrasts.htm")

# B: META_PERCEPTIONS
lm_assault_meta_R_7 <- lm_robust(SPV_meta_assault ~ condition_dummy * party_numeric * PID_bin 
                                 + condition_dummy*social_closeness
                                 + ideology_selfreport
                                 + maga,
                                 data = data_clean, se_type = "HC2")
summary(lm_assault_meta_R_7)
emm_assault_meta_R_7<- emmeans(lm_assault_meta_R_7, ~ condition_dummy | party_numeric,
                               at = list(party_numeric = 1:6),
                               vcov. = vcov(lm_assault_meta_R_7))
contr_assault_meta_R_7<- contrast(emm_assault_meta_R_7,
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_assault_meta_R_7_df <- as.data.frame(confint(contr_assault_meta_R_7))
contr_table <- summary(contr_assault_meta_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Physically Assaulting an Out-Partisan) by Condition x 7-Point Party ID",
          digits = 3, out = "assault_meta_R_7_contrasts.htm")    

# 4.8: combine matrices and form visual contrasts plot
assault_combined_df <- bind_rows(contr_assault_indiv_partyaffil_df, contr_assault_meta_R_partyaffil_df)

ggplot(assault_combined_df, aes(x = party_numeric, y = estimate,
                                color = model, group = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Individual Ratings" = "yellowgreen",
    "Meta-Perceptions of Republicans' Ratings" = "forestgreen")) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Support for Political Violence: Physically Assaulting an Out-Partisan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4.9: export models
indiv_assault_lm <- lm(SPV_indiv_assault ~ condition_dummy * PID_dummy * PID_bin 
                       + condition_dummy*social_closeness
                       + ideology_selfreport
                       + maga,
                       data = data_clean)
R_assault_lm <- lm(spv_Rassault ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
D_assault_lm <- lm(spv_Dassault ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
meta_R_assault_lm <- lm(SPV_meta_assault ~ condition_dummy * PID_dummy * PID_bin 
                        + condition_dummy*social_closeness
                        + ideology_selfreport
                        + maga,
                        data = data_clean)
starprep(indiv_assault_lm, R_assault_lm, D_assault_lm, meta_R_assault_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
# A: individual (aggregated) v. meta
stargazer(indiv_assault_lm, meta_R_assault_lm,
          type = "html",
          se = starprep(indiv_assault_lm, meta_R_assault_lm),
          p  = starprep(indiv_assault_lm, meta_R_assault_lm),
          title = "Support for Political Violence: Physically Assaulting an Out-Partisan",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Assault_Group.htm")
# B: Republicans v. Democrats v. meta
stargazer(R_assault_lm, D_assault_lm, meta_R_assault_lm,
          type = "html",
          se = starprep(R_assault_lm, D_assault_lm, meta_R_assault_lm),
          p  = starprep(R_assault_lm, D_assault_lm, meta_R_assault_lm),
          title = "Support for Political Violence: Physically Assaulting an Out-Partisan",
          dep.var.labels = c("Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Assault_Party.htm")
# C: all models   
stargazer(indiv_assault_lm, R_assault_lm, D_assault_lm, meta_R_assault_lm,
          type = "html",
          se = starprep(indiv_assault_lm, R_assault_lm, D_assault_lm, meta_R_assault_lm),
          p  = starprep(indiv_assault_lm, R_assault_lm, D_assault_lm, meta_R_assault_lm),
          title = "Support for Political Violence: Physically Assaulting an Out-Partisan",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Assault_All.htm")


# |--------------------------------|
#     TO ACHIEVE POLITICAL GOALS
# |--------------------------------|
# 5.1: form models for:
# A: Individual SPV (Aggregated)
lm_justify_indiv <- lm_robust(SPV_indiv_justify ~ condition_dummy * PID_dummy * PID_bin 
                              + condition_dummy*social_closeness
                              + ideology_selfreport
                              + maga,
                              data = data_clean, se_type = "HC2")
# B: Meta-Perceptions of Republican SPV 
lm_justify_meta_R <- lm_robust(SPV_meta_justify ~ condition_dummy * PID_dummy * PID_bin 
                               + condition_dummy*social_closeness
                               + ideology_selfreport
                               + maga,
                               data = data_clean, se_type = "HC2")
# C: Individual SPV (Reps only)
lm_R_justify <- lm_robust(spv_Rjustify ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# D: Individual SPV (Dems only)
lm_D_justify <- lm_robust(spv_Djustify ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# 5.2: summarize models
summary(lm_justify_indiv)
summary(lm_justify_meta_R)
summary(lm_R_justify)
summary(lm_D_justify)

# 5.3: form emmeans and contrast matrices for Individual SPV (Aggregated)
emm_justify_indiv <- emmeans(lm_justify_indiv, ~ condition_dummy * PID_dummy,
                             vcov. = vcov(lm_justify_indiv))
emm_justify_indiv_df <- as.data.frame(confint(emm_justify_indiv)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_justify_indiv <- contrast(emm_justify_indiv,
                                method = list(
                                  "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                  "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                  "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                  "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_justify_indiv_df <- as.data.frame(confint(contr_justify_indiv))
contr_table_justify_indiv <- summary(contr_justify_indiv) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_justify_indiv, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Achieving In-Party Political Goals) by Condition x Party ID",
          digits = 3, out = "justify_indiv_contrasts.htm")

# 5.4: form emmeans and contrast matrices for Meta-Perceptions of Republican SPV
emm_justify_meta_R <- emmeans(lm_justify_meta_R, ~ condition_dummy * PID_dummy,
                              vcov. = vcov(lm_justify_meta_R))
emm_justify_meta_R_df <- as.data.frame(confint(emm_justify_meta_R)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_justify_meta_R <- contrast(emm_justify_meta_R,
                                 method = list(
                                   "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                   "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                   "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                   "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_justify_meta_R_df <- as.data.frame(confint(contr_justify_meta_R))
contr_table_justify_meta_R <- summary(contr_justify_meta_R) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_justify_meta_R, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Achieving In-Party Political Goals) by Condition x Party ID",
          digits = 3, out = "justify_meta_R_contrasts.htm")

# 5.5: same as 5.3 but using party_numeric
justify_indiv_partyaffil <- lm_robust(SPV_indiv_justify ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_justify_indiv_partyaffil<- emmeans(justify_indiv_partyaffil, ~ condition_dummy * party_numeric,
                                       at = list(party_numeric = 1:6),
                                       vcov. = vcov(justify_indiv_partyaffil))
contr_justify_indiv_partyaffil<- contrast(emm_justify_indiv_partyaffil,
                                          method = list("Treatment - Control" = c(-1, 1)),
                                          by = "party_numeric")
contr_justify_indiv_partyaffil_df <- confint(contr_justify_indiv_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Individual Ratings")

# 5.6: same as 5.4 but using party_numeric
justify_meta_R_partyaffil <- lm_robust(SPV_meta_justify ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_justify_meta_R_partyaffil<- emmeans(justify_meta_R_partyaffil, ~ condition_dummy * party_numeric,
                                        at = list(party_numeric = 1:6),
                                        vcov. = vcov(justify_meta_R_partyaffil))
contr_justify_meta_R_partyaffil<- contrast(emm_justify_meta_R_partyaffil,
                                           method = list("Treatment - Control" = c(-1, 1)),
                                           by = "party_numeric")
contr_justify_meta_R_partyaffil_df <- confint(contr_justify_meta_R_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Meta-Perceptions of Republicans' Ratings")

# 5.7: PLANNED CONTRASTS BY 7-PT SCALE
# A: INDIVIDUAL
lm_justify_indiv_R_7 <- lm_robust(SPV_indiv_justify ~ condition_dummy * party_numeric * PID_bin 
                                  + condition_dummy*social_closeness
                                  + ideology_selfreport
                                  + maga,
                                  data = data_clean, se_type = "HC2")
summary(lm_justify_indiv_R_7)
emm_justify_indiv_R_7<- emmeans(lm_justify_indiv_R_7, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(lm_justify_indiv_R_7))
contr_justify_indiv_R_7<- contrast(emm_justify_indiv_R_7,
                                   method = list("Treatment - Control" = c(-1, 1)),
                                   by = "party_numeric")
contr_justify_indiv_R_7_df <- as.data.frame(confint(contr_justify_indiv_R_7))
contr_table <- summary(contr_justify_indiv_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (Achieving In-Party Political Goals) by Condition x 7-Point Party ID",
          digits = 3, out = "justify_indiv_R_7_contrasts.htm")

# B: META_PERCEPTIONS
lm_justify_meta_R_7 <- lm_robust(SPV_meta_justify ~ condition_dummy * party_numeric * PID_bin 
                                 + condition_dummy*social_closeness
                                 + ideology_selfreport
                                 + maga,
                                 data = data_clean, se_type = "HC2")
summary(lm_justify_meta_R_7)
emm_justify_meta_R_7<- emmeans(lm_justify_meta_R_7, ~ condition_dummy | party_numeric,
                               at = list(party_numeric = 1:6),
                               vcov. = vcov(lm_justify_meta_R_7))
contr_justify_meta_R_7<- contrast(emm_justify_meta_R_7,
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_justify_meta_R_7_df <- as.data.frame(confint(contr_justify_meta_R_7))
contr_table <- summary(contr_justify_meta_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (Achieving In-Party Political Goals) by Condition x 7-Point Party ID",
          digits = 3, out = "justify_meta_R_7_contrasts.htm")    

# 5.8: combine matrices and form visual contrasts plot
justify_combined_df <- bind_rows(contr_justify_indiv_partyaffil_df, contr_justify_meta_R_partyaffil_df)

ggplot(justify_combined_df, aes(x = party_numeric, y = estimate,
                                color = model, group = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Individual Ratings" = "lightblue",
    "Meta-Perceptions of Republicans' Ratings" = "dodgerblue")) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Support for Political Violence: Achieving In-Party Political Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.9: export models
indiv_justify_lm <- lm(SPV_indiv_justify ~ condition_dummy * PID_dummy * PID_bin 
                       + condition_dummy*social_closeness
                       + ideology_selfreport
                       + maga,
                       data = data_clean)
R_justify_lm <- lm(spv_Rjustify ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
D_justify_lm <- lm(spv_Djustify ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
meta_R_justify_lm <- lm(SPV_meta_justify ~ condition_dummy * PID_dummy * PID_bin 
                        + condition_dummy*social_closeness
                        + ideology_selfreport
                        + maga,
                        data = data_clean)
starprep(indiv_justify_lm, R_justify_lm, D_justify_lm, meta_R_justify_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
# A: individual (aggregated) v. meta
stargazer(indiv_justify_lm, meta_R_justify_lm,
          type = "html",
          se = starprep(indiv_justify_lm, meta_R_justify_lm),
          p  = starprep(indiv_justify_lm, meta_R_justify_lm),
          title = "Support for Political Violence to Achieve In-Party Political Goals",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Justify_Group.htm")
# B: Republicans v. Democrats v. meta
stargazer(R_justify_lm, D_justify_lm, meta_R_justify_lm,
          type = "html",
          se = starprep(R_justify_lm, D_justify_lm, meta_R_justify_lm),
          p  = starprep(R_justify_lm, D_justify_lm, meta_R_justify_lm),
          title = "Support for Political Violence to Achieve In-Party Political Goals",
          dep.var.labels = c("Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Justify_Party.htm")
# C: all models   
stargazer(indiv_justify_lm, R_justify_lm, D_justify_lm, meta_R_justify_lm,
          type = "html",
          se = starprep(indiv_justify_lm, R_justify_lm, D_justify_lm, meta_R_justify_lm),
          p  = starprep(indiv_justify_lm, R_justify_lm, D_justify_lm, meta_R_justify_lm),
          title = "Support for Political Violence to Achieve In-Party Political Goals",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Justify_All.htm")


# |----------------------------------|
#  IF OTHER PARTY WINS NEXT ELECTION
# |----------------------------------|
# 6.1: form models for:
# A: Individual SPV (Aggregated)
lm_predict_indiv <- lm_robust(SPV_indiv_predict ~ condition_dummy * PID_dummy * PID_bin 
                              + condition_dummy*social_closeness
                              + ideology_selfreport
                              + maga,
                              data = data_clean, se_type = "HC2")
# B: Meta-Perceptions of Republican SPV 
lm_predict_meta_R <- lm_robust(SPV_meta_predict ~ condition_dummy * PID_dummy * PID_bin 
                               + condition_dummy*social_closeness
                               + ideology_selfreport
                               + maga,
                               data = data_clean, se_type = "HC2")
# C: Individual SPV (Reps only)
lm_R_predict <- lm_robust(spv_Rpredict ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# D: Individual SPV (Dems only)
lm_D_predict <- lm_robust(spv_Dpredict ~ condition_dummy*PID_bin*social_closeness
                          + ideology_selfreport
                          + maga,
                          data = data_clean, se_type = "HC2")
# 6.2: summarize models
summary(lm_predict_indiv)
summary(lm_predict_meta_R)
summary(lm_R_predict)
summary(lm_D_predict)

# 6.3: form emmeans and contrast matrices for Individual SPV (Aggregated)
emm_predict_indiv <- emmeans(lm_predict_indiv, ~ condition_dummy * PID_dummy,
                             vcov. = vcov(lm_predict_indiv))
emm_predict_indiv_df <- as.data.frame(confint(emm_predict_indiv)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_predict_indiv <- contrast(emm_predict_indiv,
                                method = list(
                                  "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                  "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                  "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                  "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_predict_indiv_df <- as.data.frame(confint(contr_predict_indiv))
contr_table_predict_indiv <- summary(contr_predict_indiv) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_predict_indiv, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (If Opposing Party Wins Next Election) by Condition x Party ID",
          digits = 3, out = "predict_indiv_contrasts.htm")

# 6.4: form emmeans and contrast matrices for Meta-Perceptions of Republican SPV
emm_predict_meta_R <- emmeans(lm_predict_meta_R, ~ condition_dummy * PID_dummy,
                              vcov. = vcov(lm_predict_meta_R))
emm_predict_meta_R_df <- as.data.frame(confint(emm_predict_meta_R)) %>%
  mutate(
    condition_dummy = factor(condition_dummy, levels = c(0, 1),
                             labels = c("Control", "Treatment")),
    PID_dummy = factor(PID_dummy, levels = c(0, 1),
                       labels = c("Democrat", "Republican")),
    group = interaction(condition_dummy, PID_dummy))
contr_predict_meta_R <- contrast(emm_predict_meta_R,
                                 method = list(
                                   "Control D vs. Control R"     = c(-1,  0,  1,  0),
                                   "Control D vs. Treatment D"   = c(-1,  1,  0,  0),
                                   "Control R vs. Treatment R"   = c( 0,  0, -1,  1),
                                   "Treatment D vs. Treatment R" = c( 0, -1,  0,  1)))
contr_predict_meta_R_df <- as.data.frame(confint(contr_predict_meta_R))
contr_table_predict_meta_R <- summary(contr_predict_meta_R) |>
  as.data.frame() |>
  dplyr::select(contrast, estimate, SE, df, t.ratio, p.value)
stargazer(contr_table_predict_meta_R, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (If Opposing Party Wins Next Election) by Condition x Party ID",
          digits = 3, out = "predict_meta_R_contrasts.htm")

# 6.5: same as 6.3 but using party_numeric
predict_indiv_partyaffil <- lm_robust(SPV_indiv_predict ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_predict_indiv_partyaffil<- emmeans(predict_indiv_partyaffil, ~ condition_dummy * party_numeric,
                                       at = list(party_numeric = 1:6),
                                       vcov. = vcov(predict_indiv_partyaffil))
contr_predict_indiv_partyaffil<- contrast(emm_predict_indiv_partyaffil,
                                          method = list("Treatment - Control" = c(-1, 1)),
                                          by = "party_numeric")
contr_predict_indiv_partyaffil_df <- confint(contr_predict_indiv_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Individual Ratings")

# 6.6: same as 6.4 but using party_numeric
predict_meta_R_partyaffil <- lm_robust(SPV_meta_predict ~ condition_dummy * party_numeric, data = data_clean, se_type = "HC2")
emm_predict_meta_R_partyaffil<- emmeans(predict_meta_R_partyaffil, ~ condition_dummy * party_numeric,
                                        at = list(party_numeric = 1:6),
                                        vcov. = vcov(predict_meta_R_partyaffil))
contr_predict_meta_R_partyaffil<- contrast(emm_predict_meta_R_partyaffil,
                                           method = list("Treatment - Control" = c(-1, 1)),
                                           by = "party_numeric")
contr_predict_meta_R_partyaffil_df <- confint(contr_predict_meta_R_partyaffil) |>
  as.data.frame() |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")),
    model = "Meta-Perceptions of Republicans' Ratings")

# 6.7: PLANNED CONTRASTS BY 7-PT SCALE
# A: INDIVIDUAL
lm_predict_indiv_R_7 <- lm_robust(SPV_indiv_predict ~ condition_dummy * party_numeric * PID_bin 
                                  + condition_dummy*social_closeness
                                  + ideology_selfreport
                                  + maga,
                                  data = data_clean, se_type = "HC2")
summary(lm_predict_indiv_R_7)
emm_predict_indiv_R_7<- emmeans(lm_predict_indiv_R_7, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(lm_predict_indiv_R_7))
contr_predict_indiv_R_7<- contrast(emm_predict_indiv_R_7,
                                   method = list("Treatment - Control" = c(-1, 1)),
                                   by = "party_numeric")
contr_predict_indiv_R_7_df <- as.data.frame(confint(contr_predict_indiv_R_7))
contr_table <- summary(contr_predict_indiv_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Individual SPV (If Opposing Party Wins Next Election) by Condition x 7-Point Party ID",
          digits = 3, out = "predict_indiv_R_7_contrasts.htm")

# B: META_PERCEPTIONS
lm_predict_meta_R_7 <- lm_robust(SPV_meta_predict ~ condition_dummy * party_numeric * PID_bin 
                                 + condition_dummy*social_closeness
                                 + ideology_selfreport
                                 + maga,
                                 data = data_clean, se_type = "HC2")
summary(lm_predict_meta_R_7)
emm_predict_meta_R_7<- emmeans(lm_predict_meta_R_7, ~ condition_dummy | party_numeric,
                               at = list(party_numeric = 1:6),
                               vcov. = vcov(lm_predict_meta_R_7))
contr_predict_meta_R_7<- contrast(emm_predict_meta_R_7,
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_predict_meta_R_7_df <- as.data.frame(confint(contr_predict_meta_R_7))
contr_table <- summary(contr_predict_meta_R_7) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table, summary = FALSE, type = "html",
          title = "Planned Contrasts for Meta-Perceptions of Republican SPV (If Opposing Party Wins Next Election) by Condition x 7-Point Party ID",
          digits = 3, out = "predict_meta_R_7_contrasts.htm")    

# 6.8: combine matrices and form visual contrasts plot
predict_combined_df <- bind_rows(contr_predict_indiv_partyaffil_df, contr_predict_meta_R_partyaffil_df)

ggplot(predict_combined_df, aes(x = party_numeric, y = estimate,
                                color = model, group = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Individual Ratings" = "thistle",
    "Meta-Perceptions of Republicans' Ratings" = "orchid")) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Support for Political Violence: If Opposing Party Wins Next Election") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6.9: export models
indiv_predict_lm <- lm(SPV_indiv_predict ~ condition_dummy * PID_dummy * PID_bin 
                       + condition_dummy*social_closeness
                       + ideology_selfreport
                       + maga,
                       data = data_clean)
R_predict_lm <- lm(spv_Rpredict ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
D_predict_lm <- lm(spv_Dpredict ~ condition_dummy*PID_bin*social_closeness
                   + ideology_selfreport
                   + maga,
                   data = data_clean)
meta_R_predict_lm <- lm(SPV_meta_predict ~ condition_dummy * PID_dummy * PID_bin 
                        + condition_dummy*social_closeness
                        + ideology_selfreport
                        + maga,
                        data = data_clean)
starprep(indiv_predict_lm, R_predict_lm, D_predict_lm, meta_R_predict_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
# A: individual (aggregated) v. meta
stargazer(indiv_predict_lm, meta_R_predict_lm,
          type = "html",
          se = starprep(indiv_predict_lm, meta_R_predict_lm),
          p  = starprep(indiv_predict_lm, meta_R_predict_lm),
          title = "Support for Political Violence if the Opposing Party Wins the Next Election",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Predict_Group.htm")
# B: Republicans v. Democrats v. meta
stargazer(R_predict_lm, D_predict_lm, meta_R_predict_lm,
          type = "html",
          se = starprep(R_predict_lm, D_predict_lm, meta_R_predict_lm),
          p  = starprep(R_predict_lm, D_predict_lm, meta_R_predict_lm),
          title = "Support for Political Violence if the Opposing Party Wins the Next Election",
          dep.var.labels = c("Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Predict_Party.htm")
# C: all models   
stargazer(indiv_predict_lm, R_predict_lm, D_predict_lm, meta_R_predict_lm,
          type = "html",
          se = starprep(indiv_predict_lm, R_predict_lm, D_predict_lm, meta_R_predict_lm),
          p  = starprep(indiv_predict_lm, R_predict_lm, D_predict_lm, meta_R_predict_lm),
          title = "Support for Political Violence if the Opposing Party Wins the Next Election",
          dep.var.labels = c("Individual Ratings (Aggregated)", "Republicans' Ratings", "Democrats' Ratings", "Meta-Perceptions of Republicans' Ratings"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)",
                               "Social Closeness x PID Strength (Grouped)",
                               "Treatment x Social Closeness x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          out = "Predict_All.htm")


# |----------------------------------|
#          BINNED SCENARIOS
# |----------------------------------|
# 7. create four baseline models
lm_indiv_scenario <- lm_robust(SPV_indiv_scenario ~ condition_dummy, data = data_clean)
summary(lm_indiv_scenario)
lm_indiv_broad <- lm_robust(SPV_indiv_broad ~ condition_dummy, data = data_clean)
summary(lm_indiv_broad)
lm_meta_R_scenario <- lm_robust(SPV_meta_scenario ~ condition_dummy, data = data_clean)
summary(lm_meta_R_scenario)
lm_meta_R_broad <- lm_robust(SPV_meta_broad ~ condition_dummy, data = data_clean)
summary(lm_meta_R_broad)

# 7.1: INDIV_SCENARIO
# A: bivariate
lm_indiv_1 <- lm(SPV_indiv_scenario ~ condition_dummy, data = data_clean)
# B: multivariate
lm_indiv_1_cov <- lm(SPV_indiv_scenario  ~ condition_dummy * PID_dummy * PID_bin 
                     + condition_dummy*social_closeness
                     + ideology_selfreport
                     + maga,
                     data = data_clean)
# C: export
starprep(lm_indiv_1, lm_indiv_1_cov,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_indiv_1, lm_indiv_1_cov,
          type = "html",
          se = starprep(lm_indiv_1, lm_indiv_1_cov),
          p  = starprep(lm_indiv_1, lm_indiv_1_cov),
          title = "Table 1: Regression Results for Individual Attitudes towards Scenario-Based SPV",
          dep.var.labels = c("Scenario-Based SPV"),
          column.labels = c("No Covariates", "Covariates"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE,
          out = "indiv1.htm")

# 7.2: INDIV_BROAD
# A: bivariate
lm_indiv_2 <- lm(SPV_indiv_broad ~ condition_dummy, data = data_clean)
# B: multivariate
lm_indiv_2_cov <- lm(SPV_indiv_broad  ~ condition_dummy * PID_dummy * PID_bin 
                     + condition_dummy*social_closeness
                     + ideology_selfreport
                     + maga,
                     data = data_clean)
# C: export
starprep(lm_indiv_2, lm_indiv_2_cov,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_indiv_2, lm_indiv_2_cov,
          type = "html",
          se = starprep(lm_indiv_2, lm_indiv_2_cov),
          p  = starprep(lm_indiv_2, lm_indiv_2_cov),
          title = "Table 2: Regression Results for Individual Attitudes towards Broad-Based SPV",
          dep.var.labels = c("Broad-Based SPV"),
          column.labels = c("No Covariates", "Covariates"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE,
          out = "indiv2.htm")

# 7.3: META_R_SCENARIO
# A: bivariate
lm_meta_R_1 <- lm(SPV_meta_scenario ~ condition_dummy, data = data_clean)
# B: multivariate
lm_meta_R_1_cov <- lm(SPV_meta_scenario  ~ condition_dummy * PID_dummy * PID_bin 
                      + condition_dummy*social_closeness
                      + ideology_selfreport
                      + maga,
                      data = data_clean)
# C: export
starprep(lm_meta_R_1, lm_meta_R_1_cov,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_meta_R_1, lm_meta_R_1_cov,
          type = "html",
          se = starprep(lm_meta_R_1, lm_meta_R_1_cov),
          p  = starprep(lm_meta_R_1, lm_meta_R_1_cov),
          title = "Table 3: Regression Results for Meta-Perceptions of Republicans' Attitudes towards Scenario-Based SPV",
          dep.var.labels = c("Scenario-Based SPV"),
          column.labels = c("No Covariates", "Covariates"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE,
          out = "meta_R1.htm")


# 7.4: META_R_BROAD
# A: bivariate
lm_meta_R_2 <- lm(SPV_meta_broad ~ condition_dummy, data = data_clean)
# B: multivariate
lm_meta_R_2_cov <- lm(SPV_meta_broad  ~ condition_dummy * PID_dummy * PID_bin 
                      + condition_dummy*social_closeness
                      + ideology_selfreport
                      + maga,
                      data = data_clean)
# C: export
starprep(lm_meta_R_2, lm_meta_R_2_cov,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_meta_R_2, lm_meta_R_2_cov,
          type = "html",
          se = starprep(lm_meta_R_2, lm_meta_R_2_cov),
          p  = starprep(lm_meta_R_2, lm_meta_R_2_cov),
          title = "Table 4: Regression Results for Meta-Perceptions of Republicans' Attitudes towards Broad-Based SPV",
          dep.var.labels = c("Broad-Based SPV"),
          column.labels = c("No Covariates", "Covariates"),
          covariate.labels = c("Treatment",
                               "Party ID",
                               "PID Strength (Grouped)",
                               "Social Closeness",
                               "Ideology",
                               "MAGA Support",
                               "Treatment x Party ID",
                               "Treatment x PID Strength (Grouped)",
                               "Party ID x PID Strength (Grouped)",
                               "Treatment x Social Closeness",
                               "Treatment x Party ID x PID Strength (Grouped)"),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE,
          out = "meta_R2.htm")


# |----------------------------------|
#       META-PERCEPTION T-TESTS
# |----------------------------------|
# 8. outparty metaperceptions
# 8.1: actual views of R vs. how D think R will respond
t.test((data_clean %>% filter(party_numeric >= 5))$SPV_individual, (data_clean %>% filter(party_numeric <= 3))$SPV_meta_outgroup)
# 8.2: effect size
cohen.d((data_clean %>% filter(party_numeric >= 5))$SPV_individual, (data_clean %>% filter(party_numeric <= 3))$SPV_meta_outgroup)

# 9. inparty metaperceptions
# 9.1: actual views of R vs. how R think R will respond
t.test((data_clean %>% filter(party_numeric >= 5))$SPV_individual, (data_clean %>% filter(party_numeric >= 5))$SPV_meta_ingroup)
# 9.2: effect size
cohen.d((data_clean %>% filter(party_numeric >= 5))$SPV_individual, (data_clean %>% filter(party_numeric >= 5))$SPV_meta_ingroup)

# 10. are democrats' inaccuracies different from republicans' inaccuracies?
# 10.1: how D think R will respond vs. how R think R will respond
t.test((data_clean %>% filter(party_numeric <= 3))$SPV_meta_outgroup, (data_clean %>% filter(party_numeric >= 5))$SPV_meta_ingroup)
# 10.2: effect size
cohen.d(na.omit((data_clean %>% filter(party_numeric <= 3))$SPV_meta_outgroup),
        na.omit((data_clean %>% filter(party_numeric >= 5))$SPV_meta_ingroup))

