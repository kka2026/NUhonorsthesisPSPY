source("CLN_OSF.R")
source("DESC_OSF.R")
# NOTE!!!! MAKE SURE THAT IF YOU ARE TESTING THIS OUT, THAT DATA_GROUP IS IN PLACE OF DATA_FILTER!!!! OTHERWISE IT WILL SAY ERROR!!!
source("AYL_OSF.R")

# ========================================================================================================================================
#                                                                PRIMARY HYPOTHESES
# ========================================================================================================================================
# ------ H1
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
       y = "Difference in Means (Treatment - Control)",
       title = "Figure 4.1: Average Treatment Effect for Meta-Perceptions of Republican SPV by Party ID") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------ H2
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
       y = "Difference in Means (Treatment - Control)",
       title = "Figure 4.2: Average Treatment Effect for Moral Disengagement toward Other Party by Party ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------ H3
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
       y = "Difference in Means (Treatment - Control)",
       title = "Figure 4.3: Average Treatment Effect for Meta-Perceptions of Individual SPV by Party ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ========================================================================================================================================
#                                                           SECONDARY HYPOTHESES
# ========================================================================================================================================
# ------ H4
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
       y = "Difference in Means (Treatment - Control)",
       title = "Figure 4.4: Average Treatment Effect on Republican Ratings by Party ID — Unadjusted vs. Baseline-Adjusted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------ H5
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
  labs(x = "Party ID", y = "Difference in Means (Treatment - Control)",
       title = "Figure 4.5: Average Treatment Effect of Affective Ratings for Republican Party by Party ID and PID Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------- H6
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
  labs(x = "Party ID", y = "Difference in Means (Treatment - Control)",
       title = "Figure 4.6: Average Treatment Effect of Affective Ratings for Democratic Party by Party ID and PID Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ========================================================================================================================================
#                                                          FEELING THERMOMETERS
# ========================================================================================================================================

# ----- by exp_group_category
ggplot(data_clean, aes(x = ft_dem_pre, y = ft_dem_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() + 
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Pre-Treatment Ratings",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Democratic Party By Pre-Treatment Affective Ratings") +
  theme_minimal()
ggplot(data_clean, aes(x = ft_rep_pre, y = ft_rep_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Pre-Treatment Ratings",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Republican Party By Pre-Treatment Affective Ratings") +
  theme_minimal()

# --- by party bins
ggplot(data_clean, aes(x = ft_dem_pre, y = ft_dem_post,
                       color = party_category, fill = party_category, group = party_category)) +
  geom_point() + 
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Party ID", values = c("orchid", "dodgerblue", "firebrick"),
                     labels = c("Independent", "Democrat", "Republican")) +
  scale_fill_manual(name = "Party ID", values = c("thistle", "lightblue", "lightpink"),
                    labels = c("Independent", "Democrat", "Republican")) +
  labs(x = "Pre-Treatment Ratings",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Democratic Party By Party ID") +
  theme_minimal()
ggplot(data_clean, aes(x = ft_rep_pre, y = ft_rep_post,
                       color = party_category, fill = party_category, group = party_category)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Party ID", values = c("orchid", "dodgerblue", "firebrick"),
                     labels = c("Independent", "Democrat", "Republican")) +
  scale_fill_manual(name = "Party ID", values = c("thistle", "lightblue", "lightpink"),
                    labels = c("Independent", "Democrat", "Republican")) +
  labs(x = "Pre-Treatment Ratings",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Republican Party By Party ID") +
  theme_minimal()

# --- by PID Strength
ggplot(data_clean, aes(x = PID_strength, y = ft_dem_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() + 
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Partisan Identity Strength",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Democratic Party By Strength of Partisan Identity") +
  theme_minimal()
ggplot(data_clean, aes(x = PID_strength, y = ft_rep_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Partisan Identity Strength",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Republican Party By Strength of Partisan Identity") +
  theme_minimal()

# --- by MAGA
ggplot(data_clean, aes(x = ft_rep_pre, y = ft_rep_post,
                       color = maga_category, fill = maga_category, group = maga_category)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "MAGA Support", values = c("hotpink", "firebrick"),
                     labels = c("No", "Yes")) +
  scale_fill_manual(name = "MAGA Support", values = c("mistyrose", "salmon"),
                    labels = c("No", "Yes")) +
  labs(
    title = "Post-Treatment Affective Ratings for Republican Party By MAGA Support",
    x = "Pre-Treatment",
    y = "Post-Treatment") +
  theme_minimal()
ggplot(data_clean, aes(x = ft_dem_pre, y = ft_dem_post,
                       color = maga_category, fill = maga_category, group = maga_category)) + 
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "MAGA Support", values = c("dodgerblue", "midnightblue"),
                     labels = c("No", "Yes")) +
  scale_fill_manual(name = "MAGA Support", values = c("lightblue", "steelblue"),
                    labels = c("No", "Yes")) +
  labs(
    title = "Post-Treatment Affective Ratings for Democratic Party By MAGA Support",
    x = "Pre-Treatment",
    y = "Post-Treatment") +
  theme_minimal()

# --- by ideology
ggplot(data_clean, aes(x = ideology_selfreport, y = ft_dem_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() + 
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Self-Reported Ideology",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Democratic Party By Self-Reported Ideology") +
  theme_minimal()
ggplot(data_clean, aes(x = ideology_selfreport, y = ft_rep_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Self-Reported Ideology",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Republican Party By Self-Reported Ideology") +
  theme_minimal()

# --- by expanded party scale
ggplot(data_clean, aes(x = party_numeric, y = ft_dem_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() + 
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Party Affiliation Strength",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Democratic Party By Party Affiliation Strength") +
  theme_minimal()
ggplot(data_clean, aes(x = party_numeric, y = ft_rep_post,
                       color = exp_group_category, fill = exp_group_category, group = exp_group_category)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Group", values = c("firebrick", "midnightblue","violetred", "dodgerblue"),
                     labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  scale_fill_manual(name = "Group", values = c("lightsalmon", "lightsteelblue","lightpink", "lightblue"),
                    labels = c("Control (R)", "Control (D)", "Treatment (R)", "Treatment (D)")) +
  labs(x = "Party Affiliation Strength",
       y = "Post-Treatment Ratings",
       title = "Post-Treatment Affective Ratings for Republican Party By Party Affiliation Strength") +
  theme_minimal()




# ========================================================================================================================================
#                                                                     SPV
# ========================================================================================================================================
# |--------------------------------|
#             PROTEST
# |--------------------------------|
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
       y = "Difference in Means (Treatment - Control)",
       title = "Support for Political Violence: Protesting Without a Permit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#             VANDALIZE
# |--------------------------------|
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
       y = "Difference in Means (Treatment - Control)",
       title = "Support for Political Violence: Vandalizing Opposing Party's Signs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#              MESSAGE
# |--------------------------------|
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
       y = "Difference in Means (Treatment - Control)",
       title = "Support for Political Violence: Messaging Threats Online to Out-Party Representative") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#             ASSAULT
# |--------------------------------|
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
       y = "Difference in Means (Treatment - Control)",
       title = "Support for Political Violence: Physically Assaulting an Out-Partisan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#              JUSTIFY
# |--------------------------------|
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
       y = "Difference in Means (Treatment - Control)",
       title = "Support for Political Violence: Achieving In-Party Political Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#              PREDICT
# |--------------------------------|
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
       y = "Difference in Means (Treatment - Control)",
       title = "Support for Political Violence: If Opposing Party Wins Next Election") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ========================================================================================================================================
#                                                                 EXPORT
# ========================================================================================================================================
library(ggplot2)

# ----------------------------------------------------------------
#                  ASSIGN ALL PLOTS TO OBJECTS
# ----------------------------------------------------------------

# |--------------------------------|
#            PRIMARY
# |--------------------------------|
p_H1 <- ggplot(contr_H1_df, aes(x = party_numeric, y = estimate, color = factor(party_numeric))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:6, labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")) +
  scale_color_manual(values = c("1"="midnightblue","2"="dodgerblue","3"="steelblue","4"="salmon","5"="firebrick","6"="darkred"), guide = "none") +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.1: Average Treatment Effect for Meta-Perceptions of Republican SPV by Party ID") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_H2 <- ggplot(contr_H2_df, aes(x = party_numeric, y = estimate, color = factor(party_numeric))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:6, labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")) +
  scale_color_manual(values = c("1"="midnightblue","2"="dodgerblue","3"="steelblue","4"="salmon","5"="firebrick","6"="darkred"), guide = "none") +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.2: Average Treatment Effect for Moral Disengagement toward Other Party by Party ID") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_H3 <- ggplot(contr_H3_df, aes(x = party_numeric, y = estimate, color = factor(party_numeric))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 1:6, labels = c("Strong D","Weak D","Lean D","Lean R","Weak R","Strong R")) +
  scale_color_manual(values = c("1"="midnightblue","2"="dodgerblue","3"="steelblue","4"="salmon","5"="firebrick","6"="darkred"), guide = "none") +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.3: Average Treatment Effect for Individual SPV by Party ID") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#             SECONDARY
# |--------------------------------|
p_H4 <- ggplot(combined_df, aes(x = party_numeric, y = estimate,
                                color = interaction(party_numeric, model),
                                group = interaction(party_numeric, model), shape = model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Strong D.Unadjusted (H4a)"="lightsteelblue","Strong D.Baseline-Adjusted (H4b)"="midnightblue",
    "Weak D.Unadjusted (H4a)"="lightskyblue",   "Weak D.Baseline-Adjusted (H4b)"="dodgerblue",
    "Lean D.Unadjusted (H4a)"="lightblue",       "Lean D.Baseline-Adjusted (H4b)"="steelblue",
    "Lean R.Unadjusted (H4a)"="mistyrose",        "Lean R.Baseline-Adjusted (H4b)"="salmon",
    "Weak R.Unadjusted (H4a)"="lightpink",        "Weak R.Baseline-Adjusted (H4b)"="firebrick",
    "Strong R.Unadjusted (H4a)"="lightcoral",     "Strong R.Baseline-Adjusted (H4b)"="darkred"), guide = "none") +
  scale_shape_manual(name = "Model", values = c("Unadjusted (H4a)"=16,"Baseline-Adjusted (H4b)"=17)) +
  labs(x = "Party ID", y = "Difference in Means (Treatment — Control)",
       title = "Figure 4.4: Average Treatment Effect on Republican Ratings by Party ID") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_H5 <- ggplot(contr_H5_df, aes(x = party_numeric, y = estimate, color = PID_bin)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.25, size = 0.7,
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("Weak"="lightpink","Medium"="violetred","Strong"="firebrick"), name = "PID Strength") +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.5: Average Treatment Effect on Republican Ratings by Party ID and PID Strength") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_H6 <- ggplot(contr_H6_df, aes(x = party_numeric, y = estimate, color = PID_bin)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.25, size = 0.7,
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("Weak"="lightblue","Medium"="dodgerblue","Strong"="midnightblue"), name = "PID Strength") +
  labs(x = "Party ID", y = "Difference in Means (Treatment − Control)",
       title = "Figure 4.6: Average Treatment Effect on Democratic Ratings by Party ID and PID Strength") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# |--------------------------------|
#             SPV ITEMS
# |--------------------------------|
p_protest  <- ggplot(protest_combined_df, aes(x=party_numeric,y=estimate,color=model,group=model)) +
              geom_point(size=3,position=position_dodge(0.5)) + 
              geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) +
              geom_hline(yintercept=0,linetype="dashed",color="grey50") +
              scale_color_manual(values=c("Individual Ratings"="salmon","Meta-Perceptions of Republicans' Ratings"="firebrick")) +
              labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Protesting Without a Permit") +
              theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1))
p_vandalize <- ggplot(vandalize_combined_df,aes(x=party_numeric,y=estimate,color=model,group=model)) +
              geom_point(size=3,position=position_dodge(0.5)) + 
              geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) +
              geom_hline(yintercept=0,linetype="dashed",color="grey50") +
              scale_color_manual(values=c("Individual Ratings"="peachpuff","Meta-Perceptions of Republicans' Ratings"="coral")) +
              labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Vandalizing Opposing Party's Signs") +
              theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1))
p_message <- ggplot(message_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) +
              geom_point(size=3,position=position_dodge(0.5)) + 
              geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) +
              geom_hline(yintercept=0,linetype="dashed",color="grey50") +
              scale_color_manual(values=c("Individual Ratings"="gold","Meta-Perceptions of Republicans' Ratings"="goldenrod")) +
              labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Messaging Threats Online") +
              theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1))
p_assault <- ggplot(assault_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) + 
              geom_point(size=3,position=position_dodge(0.5)) + 
              geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) +
              geom_hline(yintercept=0,linetype="dashed",color="grey50") +
              scale_color_manual(values=c("Individual Ratings"="yellowgreen","Meta-Perceptions of Republicans' Ratings"="forestgreen")) +
              labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Physically Assaulting an Out-Partisan") +
              theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1))
p_justify <- ggplot(justify_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) +
              geom_point(size=3,position=position_dodge(0.5)) + 
              geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) +
              geom_hline(yintercept=0,linetype="dashed",color="grey50") +
              scale_color_manual(values=c("Individual Ratings"="lightblue","Meta-Perceptions of Republicans' Ratings"="dodgerblue")) +
              labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Achieving In-Party Political Goals") +
              theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1))
p_predict <- ggplot(predict_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) +
              geom_point(size=3,position=position_dodge(0.5)) + 
              geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) +
              geom_hline(yintercept=0,linetype="dashed",color="grey50") +
              scale_color_manual(values=c("Individual Ratings"="thistle","Meta-Perceptions of Republicans' Ratings"="orchid")) +
              labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: If Opposing Party Wins Next Election") +
              theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1))

# ----------------------------------------------------------------
#         EXPORT ALL TO ONE PDF (one plot per page)
# ----------------------------------------------------------------
all_plots <- list(
  p_H1, p_H2, p_H3,
  p_H4, p_H5, p_H6,
  p_protest, p_vandalize, p_message,
  p_assault, p_justify, p_predict
)

pdf("all_figures.pdf", width = 10, height = 6)
for (p in all_plots) print(p)
dev.off()