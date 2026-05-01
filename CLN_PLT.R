library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(janitor)

pilot_raw <- read_excel('PATH TO DATA FILE')
pilot_raw <- pilot_raw[-1,]
View(pilot_raw)

# create group variables for condition x speaker party
pilot_group <- pilot_raw %>%
  mutate(
    exp_group = case_when(
      condition == "control" ~ 0,
      condition == "treatment" ~ 1,
      TRUE ~ NA_real_
  )
)
View(pilot_group)

# filter out failed attention checks
pilot_filter <- pilot_group %>%   
    mutate(attn_pass = ifelse(attn1 == 2, 0, 1)) %>%
    filter(attn_pass == 1)
View(pilot_filter)

# making the composite calculations
# keep `condition` as-is, convert other columns to numeric
pilot_clean <- pilot_filter %>%
# convert all columns except `condition` to numeric
  mutate(across(-condition, ~as.numeric(as.character(.))),
# add column for average of PID strength questions based on party ID   
    PID_strength = rowMeans(across(PID_strengthR_1:PID_strengthR_4), na.rm = TRUE),
  # trait aggression composite
    trait_agg = rowSums(across(agg1:agg4), na.rm = TRUE),
  # in-party and out-party ft composites
    ft_pre_in = ft_rep_pre_1,
    ft_post_in = ft_rep_post_1,
    ft_pre_out = ft_dem_pre_1,
    ft_post_out = ft_dem_post_1,
  # difference in ft_pre_R v. ft_post_R
    ft_diff_R = ft_post_in - ft_pre_in,
  # difference in ft_pre_D v. ft_post_D
    ft_diff_D = ft_post_out - ft_pre_out,
  # difference in differences
    ft_diff_diff_PID = ft_diff_R - ft_diff_D,
  # relabeling binary emotions columns
    angry_dummy = ifelse(is.na(emotion_table_39), 0, emotion_table_39),
    shame_dummy = ifelse(is.na(emotion_table_40), 0, emotion_table_40),
    guilt_dummy = ifelse(is.na(emotion_table_41), 0, emotion_table_41),
    disgust_dummy = ifelse(is.na(emotion_table_42), 0, emotion_table_42),
    fear_dummy = ifelse(is.na(emotion_table_43), 0, emotion_table_43),
    anxiety_dummy = ifelse(is.na(emotion_table_44), 0, emotion_table_44),
    sadness_dummy = ifelse(is.na(emotion_table_45), 0, emotion_table_45),
    happiness_dummy = ifelse(is.na(emotion_table_46), 0, emotion_table_46),
    none_dummy = ifelse(is.na(emotion_table_47), 0, emotion_table_47),
  # moral disengagement scores for Republicans and Democrats
    moral_disengagement_R = rowMeans(across(md_R2:md_R3), na.rm = TRUE),
    moral_disengagement_D = rowMeans(across(md_D2:md_D3), na.rm = TRUE),
  # moral disengagement for out-party
    moral_disengagement_outgroup = rowMeans(across(md_D2:md_D3), na.rm = TRUE),
  # individual SPV composite
    SPV_individual = rowMeans(across(c(spv_Rprotest_1, spv_Rvandalize_1, spv_Rmessage_1,
                                       spv_Rassault_1, spv_Rjustify_1, spv_Rpredict_1)), na.rm = TRUE),
  # meta-perceptions by party
    SPV_meta_R = rowMeans(across(c(metaSPV_Rprotest_1, metaSPV_Rvandalize_1, metaSPV_Rmessage_1,
                                    metaSPV_Rassault_1, metaSPV_Rjustify_1, metaSPV_Rpredict_1)), na.rm = TRUE),
    SPV_meta_D = rowMeans(across(c(metaSPV_Dprotest_1, metaSPV_Dvandalize_1, metaSPV_Dmessage_1,
                                    metaSPV_Dassault_1, metaSPV_Djustify_1, metaSPV_Dpredict_1)), na.rm = TRUE),
  # ingroup metaSPV: Republicans
    SPV_meta_ingroup = rowMeans(across(c(metaSPV_Rprotest_1, metaSPV_Rvandalize_1, metaSPV_Rmessage_1,
                                         metaSPV_Rassault_1, metaSPV_Rjustify_1, metaSPV_Rpredict_1)), na.rm = TRUE),
  # outgroup metaSPV: Democrats
    SPV_meta_outgroup = rowMeans(across(c(metaSPV_Dprotest_1, metaSPV_Dvandalize_1, metaSPV_Dmessage_1,
                                           metaSPV_Dassault_1, metaSPV_Djustify_1, metaSPV_Dpredict_1)), na.rm = TRUE)
  )

View(pilot_clean)