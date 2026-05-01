source("CLN_OSF.R")
source("DESC_OSF.R")
# NOTE!!!! MAKE SURE THAT IF YOU ARE TESTING THIS OUT, THAT DATA_GROUP IS IN PLACE OF DATA_FILTER!!!! OTHERWISE IT WILL SAY ERROR!!!
source("AYL_OSF.R")

# 0. load library
library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(stargazer)
library(effsize)
library(modelsummary)
library(lsr)
library(rempsyc)
library(estimatr)
library(ggsignif)             


# ==================================================================================================================================
#                                        H1: TREATMENT x PID -> META-PERCEPTIONS OF REPUBLICAN SPV
# ==================================================================================================================================

# 1.4: MODEL
H1_cov <- lm_robust(SPV_meta_R ~ condition_dummy*party_numeric
                    + condition_dummy * PID_strength
                    + condition_dummy * social_closeness
                    + ideology_selfreport
                    + maga, data = data_clean, se_type = "HC2")
summary(H1_cov)

# 1.5: PLANNED CONTRASTS
emm_H1_party_numeric <- emmeans(H1_cov, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(H1_cov))
contr_H1_party_numeric<- contrast(emm_H1_party_numeric, 
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_H1_party_numeric_df <- as.data.frame(confint(contr_H1_party_numeric))
contr_table_H1_party_numeric <- summary(contr_H1_party_numeric) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table_H1_party_numeric, summary = FALSE, type = "html",
          title = "Average Treatment Effect on Meta-Perceptions of Republican SPV by Party Affiliation Strength",
          digits = 3, out = "H1_party_numeric_contrasts.htm")

# ==================================================================================================================================
#                                            H2: TREATMENT x PID -> MORAL DISENGAGEMENT
# ==================================================================================================================================

# 2.4: MODEL
H2_cov <- lm_robust(moral_disengagement_outgroup ~ condition_dummy*party_numeric
                    + condition_dummy * PID_strength
                    + condition_dummy * social_closeness
                    + ideology_selfreport
                    + maga, data = data_clean, se_type = "HC2")
summary(H2_cov)

# 2.5: PLANNED CONTRASTS
emm_H2_party_numeric <- emmeans(H2_cov, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(H2_cov))
contr_H2_party_numeric<- contrast(emm_H2_party_numeric, 
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_H2_party_numeric_df <- as.data.frame(confint(contr_H2_party_numeric))
contr_table_H2_party_numeric <- summary(contr_H2_party_numeric) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table_H2_party_numeric, summary = FALSE, type = "html",
          title = "Average Treatment Effect on Moral Disengagement Toward Other Party by Party Affiliation Strength",
          digits = 3, out = "H2_party_numeric_contrasts.htm")

# ==================================================================================================================================
#                                                 H3: TREATMENT x PID ->  INDIVIDUAL SPV
# ==================================================================================================================================

# 3.4: MODEL
H3_cov <- lm_robust(SPV_individual ~ condition_dummy*party_numeric
                    + condition_dummy * PID_strength
                    + condition_dummy * social_closeness
                    + ideology_selfreport
                    + maga, data = data_clean, se_type = "HC2")
summary(H3_cov)

# 3.5: PLANNED CONTRASTS
emm_H3_party_numeric <- emmeans(H3_cov, ~ condition_dummy | party_numeric,
                                at = list(party_numeric = 1:6),
                                vcov. = vcov(H3_cov))
contr_H3_party_numeric<- contrast(emm_H3_party_numeric, 
                                  method = list("Treatment - Control" = c(-1, 1)),
                                  by = "party_numeric")
contr_H3_party_numeric_df <- as.data.frame(confint(contr_H3_party_numeric))
contr_table_H3_party_numeric <- summary(contr_H3_party_numeric) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table_H3_party_numeric, summary = FALSE, type = "html",
          title = "Average Treatment Effect on Individual SPV by Party Affiliation Strength",
          digits = 3, out = "H3_party_numeric_contrasts.htm")

# ==================================================================================================================================
#                                                    EXPORTING PRIMARY HYPOTHESES
# ==================================================================================================================================

H1_cov_lm <- lm(SPV_meta_R ~ condition_dummy*party_numeric
                + condition_dummy * PID_strength
                + condition_dummy * social_closeness
                + ideology_selfreport
                + maga, data = data_clean)
H2_cov_lm <- lm(moral_disengagement_outgroup ~ condition_dummy*party_numeric
                + condition_dummy * PID_strength
                + condition_dummy * social_closeness
                + ideology_selfreport
                + maga, data = data_clean)
H3_cov_lm <- lm(SPV_individual ~ condition_dummy*party_numeric
                + condition_dummy * PID_strength
                + condition_dummy * social_closeness
                + ideology_selfreport
                + maga, data = data_clean)
stargazer(H1_cov_lm, H2_cov_lm, H3_cov_lm,
          type = "html",
          se = starprep(H1_cov_lm, H2_cov_lm, H3_cov_lm),
          p  = starprep(H1_cov_lm, H2_cov_lm, H3_cov_lm, stat = "p.value"),
          dep.var.labels = c("H1: Meta-Perceptions of Republican SPV", "H2: Moral Disengagement Toward Other Party", "H3: Individual SPV"),
          covariate.labels = c(
            "Treatment",                                   # condition_dummy
            "Party Affiliation Strength",                  # party_numeric
            "PID Strength",                                # PID_strength
            "Social Closeness",                            # social_closeness
            "Ideology",                                    # ideology_selfreport
            "MAGA Support",                                # maga
            "Treatment × Party Affiliation Strength",      # condition_dummy:party_numeric
            "Treatment × PID Strength",                    # condition_dummy:PID_strength
            "Treatment × Social Closeness"),               # condition_dummy:social_closeness
          title  = "Robust OLS Results for Hypotheses 1-3",
          digits = 3,
          omit.stat = c("f", "ser"),
          no.space  = TRUE,
          notes     = "HC2 robust standard errors.",
          out    = "Primary_cov_OLS.html")

# ==================================================================================================================================
#                                                        H4: FT_REPUBLICAN
# ==================================================================================================================================
# 4.8: [H4a] TREATMENT -> FT_REP
H4a_cov <- lm_robust(ft_rep_post ~ condition_dummy*party_numeric
                     + condition_dummy * PID_strength
                     + condition_dummy * social_closeness
                     + ideology_selfreport
                     + maga
                     + ft_rep_pre
                     + ft_dem_pre, data = data_clean, se_type = "HC2")
summary(H4a_cov)

# 4.9: PLANNED CONTRASTS
emm_H4a_cov <- emmeans(H4a_cov, ~ condition_dummy | party_numeric,
                       at = list(party_numeric = 1:6),
                       vcov. = vcov(H4a_cov))
contr_H4a_cov <- contrast(emm_H4a_cov,
                          method = list("Treatment - Control" = c(-1, 1)),
                          by = "party_numeric")
contr_H4a_cov_df <- as.data.frame(confint(contr_H4a_cov))
contr_table_H4a_cov <- summary(contr_H4a_cov) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_table_H4a_cov, summary = FALSE, type = "html",
          title = "Table 4.4: Planned Contrasts for Post-Treatment Affective Ratings toward Republican Party by Condition x Party ID",
          digits = 3, out = "H4a_cov_contrasts.htm")

# 4.10: [H4b] + PRE_TREAT R -> FT_REP
H4b_cov <- lm_robust(ft_rep_post ~ condition_dummy*party_numeric
                     + condition_dummy * PID_strength
                     + condition_dummy * social_closeness
                     + condition_dummy * ft_rep_pre
                     + ideology_selfreport
                     + maga
                     + ft_dem_pre, data = data_clean, se_type = "HC2")
summary(H4b_cov)

# 4.11: EXPORTING
H4a_cov_lm <- lm(ft_rep_post ~ condition_dummy*party_numeric
                 + condition_dummy * PID_strength
                 + condition_dummy * social_closeness
                 + ideology_selfreport
                 + maga
                 + ft_rep_pre
                 + ft_dem_pre, data = data_clean)
H4b_cov_lm <- lm(ft_rep_post ~ condition_dummy*party_numeric
                 + condition_dummy * PID_strength
                 + condition_dummy * social_closeness
                 + condition_dummy * ft_rep_pre
                 + ideology_selfreport
                 + maga
                 + ft_dem_pre,
                 data = data_clean)
stargazer(H4a_cov_lm, H4b_cov_lm,
          type = "html",
          se = starprep(H4a_cov, H4b_cov),
          p  = starprep(H4a_cov, H4b_cov, stat = "p.value"),
          column.labels  = c("H4a: Party Affiliation Strength Moderation", "H4b: Pre-Treatment Moderation"),
          dep.var.labels = "Change in Affective Ratings (Republican Party)",
          covariate.labels = c(
            "Treatment",                                        # condition_dummy
            "Party Affiliation Strength",                       # party_numeric
            "PID Strength",                                     # PID_strength
            "Social Closeness",                                 # social_closeness
            "Ideology",                                         # ideology_selfreport
            "MAGA Support",                                     # maga
            "FT Republican",                                    # ft_rep_pre
            "FT Democratic",                                    # ft_dem_pre
            "Treatment × Party Affiliation Strength",           # condition_dummy:party_numeric
            "Treatment × PID Strength",                         # condition_dummy:PID_strength
            "Treatment × Social Closeness",                     # condition_dummy:social_closeness
            "Treatment x FT Republican"),                       # condition_dummy:ft_rep_pre
          title  = "Table 4.4: Robust OLS Results for Change in Affective Ratings toward Republican Party",
          digits = 3,
          omit.stat = c("f", "ser"),
          no.space  = TRUE,
          notes     = "HC2 robust standard errors.",
          out = "H4_cov_OLS.html")

# ==================================================================================================================================
#                                                        H5: PID STRENGTH -> FT_REP
# ==================================================================================================================================

# 5.4: MODEL
H5_cov <- lm_robust(ft_rep_post ~ condition_dummy * party_numeric * PID_bin
                    + condition_dummy * social_closeness
                    + condition_dummy * ft_rep_pre
                    + condition_dummy * ft_dem_pre
                    + ideology_selfreport
                    + maga,
                    data = data_clean, se_type = "HC2")
summary(H5_cov)

# 5.5: PLANNED CONTRASTS
emm_H5_cov <- emmeans(H5_cov, ~ condition_dummy | party_numeric + PID_bin,
                      at = list(party_numeric = 1:6, PID_bin = c(0, 1, 2)),
                      vcov. = vcov(H5_cov))
contr_H5_cov <- contrast(emm_H5_cov,
                         method = list("Treatment - Control" = c(-1, 1)),
                         by = c("party_numeric", "PID_bin"))
contr_H5_cov_df <- as.data.frame(confint(contr_H5_cov)) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
contr_table_H5_cov <- summary(contr_H5_cov) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, PID_bin, estimate, SE, df, t.ratio, p.value) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
stargazer(contr_table_H5_cov, summary = FALSE, type = "html",
          title = "Table 4.6: Average Treatment Effect of Affective Ratings for Republican Party by Party ID and PID Strength (With Covariates)",
          digits = 3, out = "H5_cov_contrasts.htm")  


# ==================================================================================================================================
#                                                     H6: PID_STRENGTH ---> FT_DEM
# ==================================================================================================================================

# 6.4: MODEL
H6_cov <- lm_robust(ft_dem_post ~ condition_dummy * party_numeric * PID_bin
                    + condition_dummy * social_closeness
                    + condition_dummy * ft_rep_pre
                    + condition_dummy * ft_dem_pre
                    + ideology_selfreport
                    + maga,
                    data = data_clean, se_type = "HC2")
summary(H6_cov)

# 6.5: PLANNED CONTRASTS
emm_H6_cov <- emmeans(H6_cov, ~ condition_dummy | party_numeric + PID_bin,
                      at = list(party_numeric = 1:6, PID_bin = c(0, 1, 2)),
                      vcov. = vcov(H6_cov))
contr_H6_cov <- contrast(emm_H6_cov,
                         method = list("Treatment - Control" = c(-1, 1)),
                         by = c("party_numeric", "PID_bin"))
contr_H6_cov_df <- as.data.frame(confint(contr_H6_cov)) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
contr_table_H6_cov <- summary(contr_H6_cov) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, PID_bin, estimate, SE, df, t.ratio, p.value) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
stargazer(contr_table_H6_cov, summary = FALSE, type = "html",
          title = "Table 4.7: Average Treatment Effect of Affective Ratings for Democratic Party by Party ID and PID Strength (With Covariates)",
          digits = 3, out = "H6_cov_contrasts.htm")

# ==================================================================================================================================
#                                                     EXPORTING SECONDARY HYPOTHESES
# ==================================================================================================================================

H5_cov_lm <- lm(ft_rep_post ~ condition_dummy * party_numeric * PID_bin
                + condition_dummy * social_closeness
                + condition_dummy * ft_rep_pre
                + condition_dummy * ft_dem_pre
                + ideology_selfreport
                + maga,
                data = data_clean)
H6_cov_lm <- lm(ft_dem_post ~ condition_dummy * party_numeric * PID_bin
                + condition_dummy * social_closeness
                + condition_dummy * ft_rep_pre
                + condition_dummy * ft_dem_pre
                + ideology_selfreport
                + maga,
                data = data_clean)
stargazer(H5_cov_lm, H6_cov_lm,
          type = "html",
          se = starprep(H5_cov, H6_cov),
          p  = starprep(H5_cov, H6_cov, stat = "p.value"),
          dep.var.labels = c("H5: Republican Party", "H6: Democratic Party"),
          covariate.labels = c(
            "Treatment",                                                         # condition_dummy
            "Party Affiliation Strength",                                        # party_numeric
            "PID Strength (Grouped)",                                            # PID_bin
            "Social Closeness",                                                  # social_closeness
            "FT Republican (Pre)",                                               # ft_rep_pre
            "FT Democrat (Pre)",                                                 # ft_dem_pre
            "Ideology",                                                          # ideology_selfreport
            "MAGA Support",                                                      # maga
            "Treatment × Party Affiliation Strength",                            # condition_dummy:party_numeric
            "Treatment × PID Strength (Grouped)",                                # condition_dummy:PID_bin
            "Party Affiliation Strength × PID Strength (Grouped)",               # party_numeric:PID_bin
            "Treatment × Social Closeness",                                      # condition_dummy:social_closeness
            "Treatment x FT Republican",                                         # condition_dummy:ft_rep_pre
            "Treatment x FT Democrat",                                           # condition_dummy:ft_dem_pre
            "Treatment × Party Affiliation Strength × PID Strength (Grouped)"),  # condition_dummy:party_numeric:PID_bin
          title  = "Table 4.8: Robust OLS Results for Change in Affective Ratings toward Both Parties by Party Affiliation Strength and PID Strength (Grouped)",
          digits = 3,
          omit.stat = c("f", "ser"),
          no.space  = TRUE,
          notes     = "HC2 robust standard errors.",
          out = "H5_H6_cov_OLS.html") 