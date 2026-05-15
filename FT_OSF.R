source("CLN_OSF.R")
source("DESC_OSF.R")
# NOTE!!!! MAKE SURE THAT IF YOU ARE TESTING THIS OUT, THAT DATA_GROUP IS IN PLACE OF DATA_FILTER!!!! OTHERWISE IT WILL SAY ERROR!!!
source("AYL_OSF.R")


# 0. form baseline models
model_ft_dem <- lm_robust(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre, data = data_clean)
summary(model_ft_dem)
model_ft_rep <- lm_robust(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre, data = data_clean)
summary(model_ft_rep)

# |--------------------------------|
#         EXP_GROUP_CATEGORY
# |--------------------------------|
# 1.1: plot for Democratic Party FT
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

# 1.2: plot for Republican Party FT
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

# |--------------------------------|
#            PARTY_BIN
# |--------------------------------|
# 2.1: plot for Democratic Party FT
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
# 2.2: plot for Democratic Party FT   
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

# |--------------------------------|
#            PID_DUMMY
# |--------------------------------|
# 3.1. Export models
model_ft_D <- lm(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre, data = data_clean)
model_ft_R <- lm(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre, data = data_clean)

starprep(model_ft_D, model_ft_R,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(model_ft_D, model_ft_R,
          type = "html",
          se = starprep(model_ft_D, model_ft_R),
          p  = starprep(model_ft_D, model_ft_R),
          title = "Table 7: Regression Results for Post-Treatment Affective Ratings of Parties by Pre-Treatment Ratings",
          dep.var.labels = c("Democratic Party", "Republican Party"),
          covariate.labels = c(
            "Treatment",   
            "Party ID",         
            "FT Republican (Pre)",   
            "FT Democrat (Pre)",   
            "Treatment x Party ID",   
            "Treatment x FT Democrat (Pre)",   
            "Party ID x FT Democrat (Pre)",   
            "Treatment x Party ID x FT Democrat (Pre)",
            "Treatment x FT Republican (Pre)",   
            "Party ID x FT Republican (Pre)",   
            "Treatment x Party ID x FT Republican (Pre)"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "FeelingTherm.htm")

# |--------------------------------|
#            PID STRENGTH
# |--------------------------------|
# 4.1: add interaction model for Democratic Party FT and calculate ATE
lm_dem <- lm_robust(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre
                    + condition_dummy*PID_dummy*PID_strength, data = data_clean, se_type = "HC2")
summary(lm_dem)
ate_dem <- coef(lm_dem)["condition_dummy"]
print(ate_dem)

# 4.2: plot for Democratic Party FT
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

# 4.3: add interaction model for Republican Party FT and calculate ATE
lm_rep <- lm_robust(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre
                    + condition_dummy*PID_dummy*PID_strength, data = data_clean, se_type = "HC2")
summary(lm_rep)
ate_rep <- coef(lm_rep)["condition_dummy"]
print(ate_rep)

# 4.4: plot for Republican Party FT
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

# 4.5: models and export
lm_D_PID <- lm(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre
               + condition_dummy*PID_dummy*PID_strength, data = data_clean)
lm_R_PID <- lm(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre
               + condition_dummy*PID_dummy*PID_strength, data = data_clean)

starprep(lm_D_PID, lm_R_PID,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_D_PID, lm_R_PID,
          type = "html",
          se = starprep(lm_D_PID, lm_R_PID),
          p  = starprep(lm_D_PID, lm_R_PID),
          title = "Table 7: Regression Results for Post-Treatment Affective Ratings of Parties by PID Strength",
          dep.var.labels = c("Democratic Party", "Republican Party"),
          covariate.labels = c(
            "Treatment",   
            "Party ID", 
            "FT Democrat (Pre)",   
            "FT Republican (Pre)",   
            "PID Strength",   
            "Treatment x Party ID",   
            "Treatment x FT Democrat (Pre)",   
            "Party ID x FT Democrat (Pre)",   
            "Treatment x FT Republican (Pre)",   
            "Party ID x FT Republican (Pre)",   
            "Treatment x PID Strength",   
            "Party ID x PID Strength",  
            "Treatment x Party ID x FT Democrat (Pre)",
            "Treatment x Party ID x FT Republican (Pre)",
            "Treatment x Party ID x PID Strength"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "FeelingThermPID.htm")


# |--------------------------------|
#                MAGA
# |--------------------------------|
# 5.1: add interaction model for Republican Party FT and calculate ATE
lm_maga_R <- lm_robust(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre
                       + maga*condition_dummy*PID_dummy, data = data_clean)
summary(lm_maga_R)
ate_maga_R <- coef(lm_maga_R)["condition_dummy"]
print(ate_maga_R)

# 5.2: plot for Republican Party FT
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

# 5.3: add interaction model for Democratic Party FT and calculate ATE
lm_maga_D <- lm_robust(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre
                       + maga*condition_dummy*PID_dummy, data = data_clean)
summary(lm_maga_D)
ate_maga_D <- coef(lm_maga_D)["condition_dummy"]
print(ate_maga_D)

# 5.4: plot for Democratic Party FT
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

# 5.5: models and export
lm_D_MAGA <- lm(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre +
                  condition_dummy*maga*PID_dummy, data = data_clean)
lm_R_MAGA <- lm(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre +
                  condition_dummy*maga*PID_dummy, data = data_clean)

starprep(lm_D_MAGA, lm_R_MAGA,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_D_MAGA, lm_R_MAGA,
          type = "html",
          se = starprep(lm_D_MAGA, lm_R_MAGA),
          p  = starprep(lm_D_MAGA, lm_R_MAGA),
          title = "Table 7: Regression Results for Post-Treatment Affective Ratings of Parties by MAGA Support",
          dep.var.labels = c("Democratic Party", "Republican Party"),
          covariate.labels = c(
            "Treatment",   
            "Party ID", 
            "FT Democrat (Pre)",   
            "FT Republican (Pre)",   
            "MAGA",   
            "Treatment x Party ID",   
            "Treatment x FT Democrat (Pre)",   
            "Party ID x FT Democrat (Pre)",   
            "Treatment x FT Republican (Pre)",   
            "Party ID x FT Republican (Pre)",   
            "Treatment x MAGA",   
            "Party ID x MAGA",  
            "Treatment x Party ID x FT Democrat (Pre)",
            "Treatment x Party ID x FT Republican (Pre)",
            "Treatment x Party ID x MAGA"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "FeelingTherMAGA.htm")

# |--------------------------------|
#              IDEOLOGY
# |--------------------------------|
# 6.1: add interaction model for Democratic Party FT and calculate ATE
lm_ideology_selfreport_D <- lm_robust(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre +
                                        + condition_dummy*PID_dummy*ideology_selfreport, data = data_clean)
summary(lm_ideology_selfreport_D)
ate_ideology_selfreport_D <- coef(lm_ideology_selfreport_D)["condition_dummy"]
print(ate_ideology_selfreport_D)

# 6.2: plot for Democratic Party FT
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

# 6.3: add interaction model for Republican Party FT and calculate ATE
lm_ideology_selfreport_R <- lm_robust(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre +
                                        + condition_dummy*PID_dummy*ideology_selfreport, data = data_clean)
summary(lm_ideology_selfreport_R)
ate_ideology_selfreport_R <- coef(lm_ideology_selfreport_R)["condition_dummy"]
print(ate_ideology_selfreport_R)

# 6.4: plot for Republican Party FT
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

# 6.5: models and export
lm_D_ideology_selfreport <- lm(ft_dem_post ~ condition_dummy*PID_dummy*ft_dem_pre
                               + condition_dummy*PID_dummy*ideology_selfreport, data = data_clean)
lm_R_ideology_selfreport <- lm(ft_rep_post ~ condition_dummy*PID_dummy*ft_rep_pre
                               + condition_dummy*PID_dummy*ideology_selfreport, data = data_clean)

starprep(lm_D_ideology_selfreport, lm_R_ideology_selfreport,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_D_ideology_selfreport, lm_R_ideology_selfreport,
          type = "html",
          se = starprep(lm_D_ideology_selfreport, lm_R_ideology_selfreport),
          p  = starprep(lm_D_ideology_selfreport, lm_R_ideology_selfreport),
          title = "Table 7: Regression Results for Post-Treatment Affective Ratings of Parties by Self-Reported Ideology",
          dep.var.labels = c("Democratic Party", "Republican Party"),
          covariate.labels = c(
            "Treatment",   
            "Party ID", 
            "FT Democrat (Pre)",   
            "FT Republican (Pre)",   
            "Ideology",   
            "Treatment x Party ID",   
            "Treatment x FT Democrat (Pre)",   
            "Party ID x FT Democrat (Pre)",   
            "Treatment x FT Republican (Pre)",   
            "Party ID x FT Republican (Pre)",   
            "Treatment x Ideology",   
            "Party ID x Ideology",  
            "Treatment x Party ID x FT Democrat (Pre)",
            "Treatment x Party ID x FT Republican (Pre)",
            "Treatment x Party ID x Ideology"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "FeelingThermIdeology.htm")

# |--------------------------------|
#            PARTY_NUMERIC
# |--------------------------------|
# 7.1: add interaction model for Democratic Party FT and calculate ATE
lm_party_numeric_D <- lm_robust(ft_dem_post ~ condition_dummy*party_numeric*ft_dem_pre, data = data_clean)
summary(lm_party_numeric_D)
ate_party_numeric_D <- coef(lm_party_numeric_D)["condition_dummy"]
print(ate_party_numeric_D)

# 7.2: plot for Democratic Party FT
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

# 7.3: add interaction model for Republican Party FT and calculate ATE
lm_party_numeric_R <- lm_robust(ft_rep_post ~ condition_dummy*party_numeric*ft_rep_pre, data = data_clean)
summary(lm_party_numeric_R)
ate_party_numeric_R <- coef(lm_party_numeric_R)["condition_dummy"]
print(ate_party_numeric_R)

# 7.4: plot for Republican Party FT
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

# 7.5: models and export
lm_D_party_numeric <- lm(ft_dem_post ~ condition_dummy*party_numeric*ft_dem_pre, data = data_clean)
lm_R_party_numeric <- lm(ft_rep_post ~ condition_dummy*party_numeric*ft_rep_pre, data = data_clean)

starprep(lm_D_party_numeric, lm_R_party_numeric,
         stat = c("std.error", "statistic", "p.value", "ci", "df"),
         se_type = NULL,
         clusters = NULL,
         alpha = 0.05)
stargazer(lm_D_party_numeric, lm_R_party_numeric,
          type = "html",
          se = starprep(lm_D_party_numeric, lm_R_party_numeric),
          p  = starprep(lm_D_party_numeric, lm_R_party_numeric),
          title = "Table 7: Regression Results for Post-Treatment Affective Ratings of Parties by Strength of Party Affiliation",
          dep.var.labels = c("Democratic Party", "Republican Party"),
          covariate.labels = c(
            "Treatment",   
            "Party Affiliation Strength", 
            "FT Democrat (Pre)",   
            "FT Republican (Pre)",   
            "Treatment x Party Affiliation Strength",   
            "Treatment x FT Democrat (Pre)",   
            "Party Affiliation Strength x FT Democrat (Pre)",   
            "Treatment x FT Republican (Pre)",   
            "Party Affiliation Strength x FT Republican (Pre)",   
            "Treatment x Party Affiliation Strength x FT Democrat (Pre)",
            "Treatment x Party Affiliation Strength x FT Republican (Pre)"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          out = "FeelingThermPartyAffil.htm")
