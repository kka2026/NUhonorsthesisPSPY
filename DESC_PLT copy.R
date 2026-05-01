library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(stargazer)
library(modelsummary)

source("CLN_PLT.R")

# ------ DESCRIPTIVE STATISTICS

# reliability of shared partisan identity, moral disengagement, individual SPV and both meta-SPV scales
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(PID_strengthR_1:PID_strengthR_4), as.numeric)))
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(agg1:agg4), as.numeric)))
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(md_R2:md_R3), as.numeric)))
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(md_R2:md_R3), as.numeric)))
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(c(spv_Rprotest_1, spv_Rvandalize_1, spv_Rmessage_1,
                                        spv_Rassault_1, spv_Rjustify_1, spv_Rpredict_1)), as.numeric)))
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(c(metaSPV_Rprotest_1, metaSPV_Rvandalize_1, metaSPV_Rmessage_1,
                                      metaSPV_Rassault_1, metaSPV_Rjustify_1, metaSPV_Rpredict_1)), as.numeric)))
psych::alpha(as.data.frame(lapply(pilot_clean %>% select(c(metaSPV_Dprotest_1, metaSPV_Dvandalize_1, metaSPV_Dmessage_1,
                                      metaSPV_Dassault_1, metaSPV_Djustify_1, metaSPV_Dpredict_1)), as.numeric)))

descriptive_stats <- pilot_clean %>%
  group_by(exp_group) %>%
  dplyr::summarise(
    mean_PID_strength = mean(PID_strength, na.rm = TRUE),
    sd_PID_strength = sd(PID_strength, na.rm = TRUE),
    mean_ideology_selfreport = mean(as.numeric(ideology_selfreport), na.rm = TRUE),
    sd_ideology_selfreport = sd(as.numeric(ideology_selfreport), na.rm = TRUE),
    mean_ft_diff_R = mean(ft_diff_R, na.rm = TRUE),
    mean_ft_diff_D = mean(ft_diff_D, na.rm = TRUE),
    sd_ft_diff_R = sd(ft_diff_R, na.rm = TRUE),
    sd_ft_diff_D = sd(ft_diff_D, na.rm = TRUE),
    mean_ft_diff_diff = mean((ft_diff_diff_PID), na.rm = TRUE),
    sd_ft_diff_diff = sd((ft_diff_diff_PID), na.rm = TRUE),
    mean_trait_agg = mean(trait_agg, na.rm = TRUE),
    sd_trait_agg = sd(trait_agg, na.rm = TRUE),
    mean_moral_out = mean(moral_disengagement_outgroup, na.rm = TRUE),
    sd_moral_out = sd(moral_disengagement_outgroup, na.rm = TRUE),
    mean_SPV_individual = mean(SPV_individual, na.rm = TRUE),
    sd_SPV_individual = sd(SPV_individual, na.rm = TRUE),
    SPV_out = mean(SPV_meta_outgroup, na.rm = TRUE),
    sd_SPV_out = sd(SPV_meta_outgroup, na.rm = TRUE),
    SPV_in = mean(SPV_meta_ingroup, na.rm = TRUE),
    sd_SPV_in = sd(SPV_meta_ingroup, na.rm = TRUE),
    count = n()
  )
          
items <- list(
  "Strength of Partisan Identity" = c("mean_PID_strength", "sd_PID_strength"),
  "Self-Reported Ideology" = c("mean_ideology_selfreport", "sd_ideology_selfreport"),
  "Difference in Affective Ratings: Republican Party" = c("mean_ft_diff_R", "sd_ft_diff_R"),
  "Difference in Affective Ratings: Democratic Party" = c("mean_ft_diff_D", "sd_ft_diff_D"),
  "Difference in Affective Ratings Between Parties" = c("mean_ft_diff_diff", "sd_ft_diff_diff"),
  "Trait Aggression" = c("mean_trait_agg", "sd_trait_agg"),
  "Moral Disengagement: Democratic Party" = c("mean_moral_out", "sd_moral_out"),
  "Individual SPV" = c("mean_SPV_individual", "sd_SPV_individual"),
  "Meta-Perceptions of Democratic SPV" = c("SPV_out", "sd_SPV_out"),
  "Meta-Perceptions of Republican SPV" = c("SPV_in", "sd_SPV_in"),
  "N" = c("count", "count")
)
apa_rows <- lapply(names(items), function(label) {
  cols <- items[[label]]
  mean_col <- cols[1]; sd_col <- cols[2]
  g0 <- descriptive_stats %>% filter(exp_group == 0)
  g1 <- descriptive_stats %>% filter(exp_group == 1)
  val0 <- if (!is.na(g0[[mean_col]])) {
    if (mean_col == "count") sprintf("%d", g0[[mean_col]]) else sprintf("%.2f (%.2f)", g0[[mean_col]], g0[[sd_col]])
  } else NA_character_
  val1 <- if (!is.na(g1[[mean_col]])) {
    if (mean_col == "count") sprintf("%d", g1[[mean_col]]) else sprintf("%.2f (%.2f)", g1[[mean_col]], g1[[sd_col]])
  } else NA_character_
  tibble::tibble(Variable = label, Group_Control = val0, Group_Treatment = val1)
}) %>% dplyr::bind_rows()
# Put sample sizes in column headers if available
n0 <- descriptive_stats %>% filter(exp_group == 0) %>% pull(count) %>% unique()
n1 <- descriptive_stats %>% filter(exp_group == 1) %>% pull(count) %>% unique()
colnames(apa_rows)[2:3] <- c(paste0("Control (n=", ifelse(length(n0)>0, n0, "NA"), ")"),
                             paste0("Treatment (n=", ifelse(length(n1)>0, n1, "NA"), ")"))

apa_text <- knitr::kable(apa_rows, format = "html")
cat(apa_text, file = "Descriptive_Statistics.htm", sep = "\n")
