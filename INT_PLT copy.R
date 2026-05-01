library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(stargazer)
library(effsize)

source("CLN_PLT.R")
source("DESC_PLT.R")
source("EXPL_PLT.R")

# view cleaned data
View(pilot_clean)

# ---- FT INTERACTION

# compare FT pre and post
model_ft_dem <- lm_robust(ft_dem_post_1 ~ exp_group + ft_dem_pre_1 +exp_group*ft_dem_pre_1, data = pilot_clean)
model_ft_rep <- lm_robust(ft_rep_post_1 ~ exp_group + ft_rep_pre_1 +exp_group*ft_rep_pre_1, data = pilot_clean)

# plot feeling thermometer changes by treatment condition
ggplot(pilot_clean, aes(x = ft_dem_pre_1, y = ft_dem_post_1,
                        color = condition, fill = condition, group = condition)) +
  geom_point() + 
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("dodgerblue", "midnightblue"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lightblue", "cadetblue"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Pre-Treatment Ratings",
       y = "Post-Treatment Ratings") +
  theme_minimal()

ggplot(pilot_clean, aes(x = ft_rep_pre_1, y = ft_rep_post_1,
                        color = condition, fill = condition, group = condition)) +
  geom_point() +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("firebrick", "hotpink"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("mistyrose", "pink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Pre-Treatment Ratings",
       y = "Post-Treatment Ratings") +
  theme_minimal()

# models & export
model_ft_D <- lm(ft_dem_post_1 ~ condition * ft_dem_pre_1, data = pilot_clean)
model_ft_R <- lm(ft_rep_post_1 ~ condition * ft_rep_pre_1, data = pilot_clean)

starprep(model_ft_D, model_ft_R,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05
)
stargazer(model_ft_D, model_ft_R,
          type = "html",
          se = starprep(model_ft_D, model_ft_R),
          p  = starprep(model_ft_D, model_ft_D),
          title = "Table 7: Regression Results for Post-Treatment Affective Ratings By Party",
          dep.var.labels = c("Democratic Party", "Republican Party"),
          covariate.labels = c("Treatment", "FT Democrat (Pre-Treatment)",
                               "Treatment x FT Democrat (Pre-Treatment)", "FT Republican (Pre-Treatment)",
                               "Treatment x FT Republican (Pre-Treatment)"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "FeelingTherm.htm")

# backup!!!
ft_models <- list(
  "Model 1: Democratic Party" = model_ft_dem,
  "Model 2: Republican Party" = model_ft_rep
)
modelsummary::modelsummary(ft_models,
                           output = "Feeling_Thermometer_Regression_Interactions.docx",
                           title = "Regression Results for Post-Treatment Affective Ratings By Party",
                           coef_rename = c("exp_group" = "Treatment",
                                           "ft_rep_pre_1" = "FT: Republican Party (Pre-Treatment)",
                                           "ft_dem_pre_1" = "FT: Democratic Party (Pre-Treatment)"),
                           stars = TRUE)


# ----- INTERACTION OF EXP_GROUP AND PID_STRENGTH

# moral disengagement
ggplot(pilot_clean, aes(x = PID_strength, y = moral_disengagement_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("orchid", "darkmagenta"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavender", "thistle"),
                    labels = c("Control", "Treatment")) +
  labs(x = "PID Strength",
       y = "Moral Disengagement",
       title = "Predicted Moral Disengagement by Condition and Strength of Partisan Identity") +
  theme_minimal()

# individual SPV
ggplot(pilot_clean, aes(x = PID_strength, y = SPV_individual,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("orchid", "darkmagenta"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavender", "thistle"),
                    labels = c("Control", "Treatment")) +
  labs(x = "PID Strength",
       y = "Individual SPV",
       title = "Predicted Individual SPV by Condition and Strength of Partisan Identity") +
  theme_minimal()

# meta-SPV outgroup
ggplot(pilot_clean, aes(x = PID_strength, y = SPV_meta_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("orchid", "darkmagenta"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavender", "thistle"),
                    labels = c("Control", "Treatment")) +
  labs(x = "PID Strength",
       y = "SPV Meta Outgroup",
       title = "Predicted SPV (Meta Outgroup) by Condition and PID Strength") +
  theme_minimal()

# meta-SPV ingroup
ggplot(pilot_clean, aes(x = PID_strength, y = SPV_meta_ingroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("orchid", "darkmagenta"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavender", "thistle"),
                    labels = c("Control", "Treatment")) +
  labs(x = "PID Strength",
       y = "SPV Meta Ingroup",
       title = "Predicted SPV (Meta Ingroup) by Condition and PID Strength") +
  theme_minimal()




# ----- INTERACTION OF EXP_GROUP AND MAGA


# moral disengagement
ggplot(pilot_clean, aes(x = maga, y = moral_disengagement_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("coral", "orange"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("peachpuff", "cornsilk"),
                    labels = c("Control", "Treatment")) +
  labs(x = "MAGA Support",
       y = "Moral Disengagement",
       title = "Predicted Moral Disengagement by Condition and MAGA Support") +
  theme_minimal()

# individual SPV
ggplot(pilot_clean, aes(x = maga, y = SPV_individual,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("coral", "orange"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("peachpuff", "cornsilk"),
                    labels = c("Control", "Treatment")) +
  labs(x = "MAGA Support",
       y = "Individual SPV",
       title = "Predicted Individual SPV by Condition and MAGA Support") +
  theme_minimal()

# meta-SPV outgroup
ggplot(pilot_clean, aes(x = maga, y = SPV_meta_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("coral", "orange"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("peachpuff", "cornsilk"),
                    labels = c("Control", "Treatment")) +
  labs(x = "MAGA Support",
       y = "SPV Meta Outgroup",
       title = "Predicted SPV (Meta Outgroup) by Condition and MAGA Support") +
  theme_minimal()

# meta-SPV ingroup
ggplot(pilot_clean, aes(x = maga, y = SPV_meta_ingroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("coral", "orange"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("peachpuff", "cornsilk"),
                    labels = c("Control", "Treatment")) +
  labs(x = "MAGA Support",
       y = "SPV Meta Ingroup",
       title = "Predicted SPV (Meta Ingroup) by Condition and MAGA Support") +
  theme_minimal()





# ----- INTERACTION OF EXP_GROUP AND REPUBLICAN FT


# moral disengagement
ggplot(pilot_clean, aes(x = ft_rep_pre_1, y = moral_disengagement_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("firebrick", "hotpink"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("mistyrose", "pink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Republican Party: Pre-Treatment",
       y = "Moral Disengagement",
       title = "Predicted Moral Disengagement by Condition and Republican Party Affective Ratings") +
  theme_minimal()

# individual SPV
ggplot(pilot_clean, aes(x = ft_rep_pre_1, y = SPV_individual,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("firebrick", "hotpink"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("mistyrose", "pink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Republican Party: Pre-Treatment",
       y = "Individual SPV",
       title = "Predicted Individual SPV by Condition and Republican Party Affective Ratings") +
  theme_minimal()

# meta-SPV outgroup
ggplot(pilot_clean, aes(x = ft_rep_pre_1, y = SPV_meta_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("firebrick", "hotpink"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("mistyrose", "pink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Republican Party: Pre-Treatment",
       y = "SPV Meta Outgroup",
       title = "Predicted SPV (Meta Outgroup) by Condition and Republican Party Affective Ratings") +
  theme_minimal()

# meta-SPV ingroup
ggplot(pilot_clean, aes(x = ft_rep_pre_1, y = SPV_meta_ingroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("firebrick", "hotpink"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("mistyrose", "pink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Republican Party: Pre-Treatment",
       y = "SPV Meta Ingroup",
       title = "Predicted SPV (Meta Ingroup) by Condition and Republican Party Affective Ratings") +
  theme_minimal()



# ----- INTERACTION OF EXP_GROUP AND DEMOCRATIC FT


# moral disengagement
ggplot(pilot_clean, aes(x = ft_dem_pre_1, y = moral_disengagement_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("dodgerblue", "midnightblue"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lightblue", "cadetblue"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Democratic Party: Pre-Treatment",
       y = "Moral Disengagement",
       title = "Predicted Moral Disengagement by Condition and Democratic Party Affective Ratings") +
  theme_minimal()

# individual SPV
ggplot(pilot_clean, aes(x = ft_dem_pre_1, y = SPV_individual,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("dodgerblue", "midnightblue"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lightblue", "cadetblue"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Democratic Party: Pre-Treatment",
       y = "Individual SPV",
       title = "Predicted Individual SPV by Condition and Democratic Party Affective Ratings") +
  theme_minimal()

# meta-SPV outgroup
ggplot(pilot_clean, aes(x = ft_dem_pre_1, y = SPV_meta_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("dodgerblue", "midnightblue"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lightblue", "cadetblue"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Democratic Party: Pre-Treatment",
       y = "SPV Meta Outgroup",
       title = "Predicted SPV (Meta Outgroup) by Condition and Democratic Party Affective Ratings") +
  theme_minimal()

# meta-SPV ingroup
ggplot(pilot_clean, aes(x = ft_dem_pre_1, y = SPV_meta_ingroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("dodgerblue", "midnightblue"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lightblue", "cadetblue"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Affective Ratings for Democratic Party: Pre-Treatment",
       y = "SPV Meta Ingroup",
       title = "Predicted SPV (Meta Ingroup) by Condition and Democratic Party Affective Ratings") +
  theme_minimal()





# ----- INTERACTION OF EXP_GROUP AND TRAIT AGGRESSION


# moral disengagement
ggplot(pilot_clean, aes(x = trait_agg, y = moral_disengagement_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("palevioletred", "violetred"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavenderblush", "lightpink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Trait Aggression",
       y = "Moral Disengagement",
       title = "Predicted Moral Disengagement by Condition and Trait Aggression") +
  theme_minimal()

# individual SPV
ggplot(pilot_clean, aes(x = trait_agg, y = SPV_individual,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("palevioletred", "violetred"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavenderblush", "lightpink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Trait Aggression",
       y = "Individual SPV",
       title = "Predicted Individual SPV by Condition and Trait Aggression") +
  theme_minimal()

# meta-SPV outgroup
ggplot(pilot_clean, aes(x = trait_agg, y = SPV_meta_outgroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("palevioletred", "violetred"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavenderblush", "lightpink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Trait Aggression",
       y = "SPV Meta Outgroup",
       title = "Predicted SPV (Meta Outgroup) by Condition and Trait Aggression") +
  theme_minimal()

# meta-SPV ingroup
ggplot(pilot_clean, aes(x = trait_agg, y = SPV_meta_ingroup,
                        color = condition, fill = condition, group = condition)) +
  geom_smooth(method = "lm_robust", se = TRUE) +
  scale_color_manual(name = "Condition", values = c("palevioletred", "violetred"),
                     labels = c("Control", "Treatment")) +
  scale_fill_manual(name = "Condition", values = c("lavenderblush", "lightpink"),
                    labels = c("Control", "Treatment")) +
  labs(x = "Trait Aggression",
       y = "SPV Meta Ingroup",
       title = "Predicted SPV (Meta Ingroup) by Condition and Trait Aggression") +
  theme_minimal()

