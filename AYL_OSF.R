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

# 1.1: MODEL
H1<- lm_robust(SPV_meta_R ~ condition_dummy * party_numeric,
               data = data_clean, se_type = "HC2")
summary(H1)

# 1.2: PLANNED CONTRASTS
emm_H1<- emmeans(H1, ~ condition_dummy | party_numeric,
                 at = list(party_numeric = 1:6),
                 vcov. = vcov(H1))
contr_H1<- contrast(emm_H1,
                    method = list("Treatment - Control" = c(-1, 1)),
                    by = "party_numeric")
contr_H1_df <- as.data.frame(confint(contr_H1))
contr_H1_table <- summary(contr_H1) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_H1_table, summary = FALSE, type = "html",
          title = "Table 4.1: Average Treatment Effect on Meta-Perceptions of Republican SPV by Party ID",
          digits = 3, out = "H1_contrasts.htm")

# 1.3: PLOT
ggplot(contr_H1_df, aes(x = party_numeric, y = estimate,
                        color = factor(party_numeric))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:6,
                     labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")) +
  scale_color_manual(values = c(
    "1" = "midnightblue",
    "2" = "dodgerblue",
    "3" = "steelblue",
    "4" = "salmon",
    "5" = "firebrick",
    "6" = "darkred"
  ), guide = "none") +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.1: Average Treatment Effect for Meta-Perceptions of Republican SPV by Party ID") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==================================================================================================================================
#                                            H2: TREATMENT x PID -> MORAL DISENGAGEMENT
# ==================================================================================================================================

# 2.1: MODEL
H2<- lm_robust(moral_disengagement_outgroup ~ condition_dummy * party_numeric,
               data = data_clean, se_type = "HC2")
summary(H2)

# 2.2: PLANNED CONTRASTS
emm_H2<- emmeans(H2, ~ condition_dummy | party_numeric,
                 at = list(party_numeric = 1:6),
                 vcov. = vcov(H2))
contr_H2<- contrast(emm_H2,
                    method = list("Treatment - Control" = c(-1, 1)),
                    by = "party_numeric")
contr_H2_df <- as.data.frame(confint(contr_H2))
contr_H2_table <- summary(contr_H2) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_H2_table, summary = FALSE, type = "html",
          title = "Table 4.2: Average Treatment Effect on Moral Disengagement toward Other Party by Party ID",
          digits = 3, out = "H2_contrasts.htm")

# 2.3: PLOT
ggplot(contr_H2_df, aes(x = party_numeric, y = estimate,
                        color = factor(party_numeric))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:6,
                     labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")) +
  scale_color_manual(values = c(
    "1" = "midnightblue",
    "2" = "dodgerblue",
    "3" = "steelblue",
    "4" = "salmon",
    "5" = "firebrick",
    "6" = "darkred"
  ), guide = "none") +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.2: Average Treatment Effect for Moral Disengagement toward Other Party by Party ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==================================================================================================================================
#                                                 H3: TREATMENT x PID ->  INDIVIDUAL SPV
# ==================================================================================================================================

# 3.1: MODEL
H3<- lm_robust(SPV_individual ~ condition_dummy * party_numeric,
               data = data_clean, se_type = "HC2")
summary(H3)

# 3.2: PLANNED CONTRASTS
emm_H3<- emmeans(H3, ~ condition_dummy | party_numeric,
                 at = list(party_numeric = 1:6),
                 vcov. = vcov(H3))
contr_H3<- contrast(emm_H3,
                    method = list("Treatment - Control" = c(-1, 1)),
                    by = "party_numeric")
contr_H3_df <- as.data.frame(confint(contr_H3))
contr_H3_table <- summary(contr_H3) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_H3_table, summary = FALSE, type = "html",
          title = "Table 4.3: Average Treatment Effect on Individual SPV by Party ID",
          digits = 3, out = "H3_contrasts.htm")

# 3.3: PLOT
ggplot(contr_H3_df, aes(x = party_numeric, y = estimate,
                        color = factor(party_numeric))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:6,
                     labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")) +
  scale_color_manual(values = c(
    "1" = "midnightblue",
    "2" = "dodgerblue",
    "3" = "steelblue",
    "4" = "salmon",
    "5" = "firebrick",
    "6" = "darkred"
  ), guide = "none") +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.3: Average Treatment Effect for Meta-Perceptions of Individual SPV by Party ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==================================================================================================================================
#                                                        H4: FT_REPUBLICAN
# ==================================================================================================================================

# |--------------------------------|
#    H4a: TREATMENT -> FT_REP
# |--------------------------------|
# 4.1: MODEL
H4a<- lm_robust(ft_rep_post ~ condition_dummy * party_numeric,
                data = data_clean, se_type = "HC2")
summary(H4a)

# 4.2: PLANNED CONTRASTS
emm_H4a<- emmeans(H4a, ~ condition_dummy | party_numeric,
                  at = list(party_numeric = 1:6),
                  vcov. = vcov(H4a))
contr_H4a<- contrast(emm_H4a,
                     method = list("Treatment - Control" = c(-1, 1)),
                     by = "party_numeric")
contr_H4a_table <- summary(contr_H4a) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_H4a_table, summary = FALSE, type = "html",
          title = "Table 4.4: Average Treatment Effect on Affective Ratings toward Republican Party by Party ID",
          digits = 3, out = "H4a_contrasts.htm")

# |--------------------------------|
#    H4b: PRE_TREAT R -> FT_REP
# |--------------------------------|
# 4.3: MODEL
H4b <- lm_robust(ft_rep_post ~ condition_dummy * party_numeric
                 + condition_dummy * ft_rep_pre,
                 data = data_clean, se_type = "HC2")

# 4.4: PLANNED CONTRASTS
emm_H4b<- emmeans(H4b, ~ condition_dummy | party_numeric,
                  at = list(party_numeric = 1:6),
                  vcov. = vcov(H4b))
contr_H4b<- contrast(emm_H4b,
                     method = list("Treatment - Control" = c(-1, 1)),
                     by = "party_numeric")
contr_H4b_table <- summary(contr_H4b) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, estimate, SE, df, t.ratio, p.value) |>
  mutate(party_numeric = factor(party_numeric, levels = 1:6,
                                labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")))
stargazer(contr_H4b_table, summary = FALSE, type = "html",
          title = "Table 4.4: Average Treatment Effect on Affective Ratings toward Republican Party by Party ID by Pre-Treatment (Baseline-Adjusted)",
          digits = 3, out = "H4b_contrasts.htm")

# 4.5: ROBUST OLS
H4b_lm <- lm(ft_rep_post ~ condition_dummy * party_numeric
             + condition_dummy * ft_rep_pre,
             data = data_clean)
starprep(H4b_lm,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(H4b_lm,
          type = "html",
          se = starprep(H4b),
          p  = starprep(H4b),
          title = "Table 4.5: Robust OLS Linear Regression Results for Post-Treatment Affective Ratings toward Republican Party by Pre-Treatment Affective Ratings",
          dep.var.labels = "FT Republican",
          covariate.labels = c("Treatment",                                # condition_dummy
                               "Party Affiliation Strength",               # party_numeric
                               "FT Republican",                            # ft_rep_pre
                               "Treatment x Party Affiliation Strength",   # condition_dummy:party_numeric
                               "Treatment x FT Republican"),               # condition_dummy:ft_rep_pre
          digits = 3,
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "H4_OLS.htm")

# |--------------------------------|
#         COMBINED MODEL
# |--------------------------------|
# 4.6: form data frames for plotting
contr_H4a_df <- as.data.frame(confint(contr_H4a)) %>%
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D",
                                      "Lean R","Weak R","Strong R")),
    model = "Unadjusted (H4a)")
contr_H4b_df <- as.data.frame(confint(contr_H4b)) %>%
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D","Weak D","Lean D",
                                      "Lean R","Weak R","Strong R")),
    model = "Baseline-Adjusted (H4b)")
combined_df <- bind_rows(contr_H4a_df, contr_H4b_df)

# 4.7: PLOT
ggplot(combined_df, aes(x = party_numeric, y = estimate,
                        color = interaction(party_numeric, model),
                        group = interaction(party_numeric, model),
                        shape = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Strong D.Unadjusted (H4a)"        = "lightsteelblue",
    "Strong D.Baseline-Adjusted (H4b)" = "midnightblue",
    "Weak D.Unadjusted (H4a)"          = "lightskyblue",
    "Weak D.Baseline-Adjusted (H4b)"   = "dodgerblue",
    "Lean D.Unadjusted (H4a)"          = "lightblue",
    "Lean D.Baseline-Adjusted (H4b)"   = "steelblue",
    "Lean R.Unadjusted (H4a)"          = "mistyrose",
    "Lean R.Baseline-Adjusted (H4b)"   = "salmon",
    "Weak R.Unadjusted (H4a)"          = "lightpink",
    "Weak R.Baseline-Adjusted (H4b)"   = "firebrick",
    "Strong R.Unadjusted (H4a)"        = "lightcoral",
    "Strong R.Baseline-Adjusted (H4b)" = "darkred"
  ), guide = "none") +
  scale_shape_manual(name = "Model",
                     values = c("Unadjusted (H4a)"        = 16,
                                "Baseline-Adjusted (H4b)" = 17)) +
  labs(x = "Party ID",
       y = "Difference in Means (Treatment — Control)",
       title = "Figure 4.4: Average Treatment Effect on Republican Ratings by Party ID — Unadjusted vs. Baseline-Adjusted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==================================================================================================================================
#                                                        H5: PID STRENGTH -> FT_REP
# ==================================================================================================================================

# 5.1: MODEL
H5 <- lm_robust(ft_rep_post ~ condition_dummy * party_numeric * PID_bin,
                data = data_clean, se_type = "HC2")
summary(H5)

# 5.2: PLANNED CONTRASTS
emm_H5 <- emmeans(H5, ~ condition_dummy | party_numeric + PID_bin,
                  at = list(party_numeric = 1:6, PID_bin = c(0, 1, 2)),
                  vcov. = vcov(H5))
contr_H5 <- contrast(emm_H5,
                     method = list("Treatment - Control" = c(-1, 1)),
                     by = c("party_numeric", "PID_bin"))
contr_table_H5 <- summary(contr_H5) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, PID_bin, estimate, SE, df, t.ratio, p.value) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
contr_H5_df <- as.data.frame(confint(contr_H5)) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
stargazer(contr_table_H5, summary = FALSE, type = "html",
          title = "Table 4.6: Average Treatment Effect of Affective Ratings for Republican Party by Party ID and PID Strength",
          digits = 3, out = "H5.htm")

# 5.3: PLOT
ggplot(contr_H5_df,
       aes(x = party_numeric, y = estimate, color = PID_bin)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.25, size = 0.7,
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(
    values = c("Weak" = "lightpink", "Medium" = "violetred", "Strong" = "firebrick"),
    name = "PID Strength"
  ) +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.5: Average Treatment Effect of Affective Ratings for Republican Party by Party ID and PID Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==================================================================================================================================
#                                                     H6: PID_STRENGTH ---> FT_DEM
# ==================================================================================================================================

# 6.1: MODEL
H6 <- lm_robust(ft_dem_post ~ condition_dummy * party_numeric * PID_bin,
                data = data_clean, se_type = "HC2")
summary(H6)

# 6.2: PLANNED CONTRASTS
emm_H6 <- emmeans(H6, ~ condition_dummy | party_numeric + PID_bin,
                  at = list(party_numeric = 1:6, PID_bin = c(0, 1, 2)),
                  vcov. = vcov(H6))
contr_H6 <- contrast(emm_H6,
                     method = list("Treatment - Control" = c(-1, 1)),
                     by = c("party_numeric", "PID_bin"))
contr_table_H6 <- summary(contr_H6) |>
  as.data.frame() |>
  dplyr::select(contrast, party_numeric, PID_bin, estimate, SE, df, t.ratio, p.value) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
contr_H6_df <- as.data.frame(confint(contr_H6)) |>
  mutate(
    party_numeric = factor(party_numeric, levels = 1:6,
                           labels = c("Strong D", "Weak D", "Lean D",
                                      "Lean R", "Weak R", "Strong R")),
    PID_bin = factor(PID_bin, levels = 0:2,
                     labels = c("Weak", "Medium", "Strong")))
stargazer(contr_table_H6, summary = FALSE, type = "html",
          title = "Table 4.7: Average Treatment Effect of Affective Ratings for Democratic Party by Party ID and PID Strength",
          digits = 3, out = "H6.htm")

# 6.3: PLOT
ggplot(contr_H6_df,
       aes(x = party_numeric, y = estimate, color = PID_bin)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.25, size = 0.7,
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(
    values = c("Weak" = "lightblue", "Medium" = "dodgerblue", "Strong" = "midnightblue"),
    name = "PID Strength"
  ) +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.6: Average Treatment Effect of Affective Ratings for Democratic Party by Party ID and PID Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   