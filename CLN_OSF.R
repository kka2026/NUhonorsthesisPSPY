# 1. load library
library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(janitor)

# 2. LOAD FILE
data_raw <- read_csv('PATH TO DATA_DE-IDENT FILE')

# 3. create party scale variables
# *if PID_general == 2 then go off of strong-weak PID
# **if PID_general == 3 then go off of ind_lean
data_group <- data_raw %>%
  mutate(
    across(-c(condition:PID), ~ suppressWarnings(as.numeric(as.character(.)))),
    party_numeric = case_when(
      PID_general == 2 & strong_weak == 2 ~ 7,
      PID_general == 2 & strong_weak == 1 ~ 6,
      PID_general == 3 & ind_lean == 3 ~ 5,
      PID_general == 3 & ind_lean == 2 ~ 4,
      PID_general == 3 & ind_lean == 1 ~ 3,
      PID_general == 1 & strong_weak == 1 ~ 2,
      PID_general == 1 & strong_weak == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    party_scale = factor(party_numeric, 
                         levels = c(1, 2, 3, 5, 6, 7),
                         labels = c("Strong D", "Weak D", "Lean D", 
                                    "Lean R", 
                                    "Weak R", "Strong R")),
    
    # 4. create bigger party groupings
    party_bin = case_when(
      party_numeric >= 6 ~ 2,
      party_numeric <= 2 ~ 1,
      party_numeric >=3 & party_numeric <=5 ~ 0,
      TRUE ~ NA_real_
    ),
    party_category = factor(party_bin, 
                            levels = 0:2,
                            labels = c("Independent", "Democrat", "Republican")),
    PID_dummy = case_when(
      PID == "Democrat" ~ 0,
      PID == "Republican" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # 5. add in MAGA
    maga = replace_na(maga, 0),
    maga_category = factor(maga, 
                           levels = 0:1,
                           labels = c("Non-MAGA", "MAGA")),
    ideology = factor(ideology_selfreport, 
                      levels = 1:7,
                      labels = c("Very liberal", "Somewhat liberal", "Slightly liberal", 
                                 "Moderate", "Slightly conservative", 
                                 "Somewhat conservative", "Very conservative")),
    
    # 6. EXPERIMENTAL GROUPINGS
    # recode treatment groups from characters to numeric
    condition_dummy = case_when(
      condition == "control" ~ 0,
      condition == "treatment" ~ 1,
      TRUE ~ NA_real_),
    # create group variables for condition x speaker party
    exp_group_numeric = case_when(
      condition == "control" & party_numeric >= 5 ~ 1,
      condition == "control" & party_numeric <= 3 ~ 2,
      condition == "treatment" & party_numeric >=5 ~ 3,
      condition == "treatment" & party_numeric <=3 ~ 4,
      TRUE ~ NA_real_
    ),
    exp_group_category = factor(exp_group_numeric, 
                                levels = 1:4,
                                labels = c("Control R", "Control D", 
                                           "Treatment R", "Treatment D")),
    
    # dummy for match (exp_group is 4 or 5) or no match (exp_group is 3 or 6)
    speaker_match = case_when(
      exp_group_numeric == 3 ~ 1,
      exp_group_numeric == 4 ~ 0,
      exp_group_numeric == 1 ~ 0,
      exp_group_numeric == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # clump conditions by match or no match
    exp_group = case_when(
      condition_dummy == 0 ~ 0,
      condition_dummy == 1 & speaker_match == 0 ~ 1,
      condition_dummy == 1 & speaker_match == 1 ~ 2,
      TRUE ~ NA_real_
    ),
    exp_group_labels = factor(exp_group,levels = c(0:2),
                              labels = c("Control", "Mismatch", "Match")),
    
    # 7. convert all columns except categories to numeric
    across(-c(condition, PID, party_scale, party_category, maga_category, ideology, exp_group_category, exp_group_labels),
           ~as.numeric(as.character(.))))

# 8. filter out failed attention checks
data_filter <- data_group %>%
  filter(attn1 == 2 | party_numeric == 4)

# 9. RENAMING SCALE VARIABLES 
data_rename <- data_filter %>%
  mutate(
    
    # 9.1: FT RENAMING
    ft_rep_pre = ft_rep_pre_1,
    ft_dem_pre = ft_dem_pre_1,
    ft_rep_post = ft_rep_post_1,
    ft_dem_post = ft_dem_post_1,
    
    # 9.2: SPV RENAMING
    # A: INDIVIDUAL
    # 1: REPUBLICANS
    spv_Rprotest = spv_Rprotest_1,
    spv_Rvandalize = spv_Rvandalize_1,
    spv_Rmessage = spv_Rmessage_1,
    spv_Rassault = spv_Rassault_1,
    spv_Rjustify = spv_Rjustify_1,
    spv_Rpredict = spv_Rpredict_1,
    # 2: DEMOCRATS
    spv_Dprotest = spv_Dprotest_1,
    spv_Dvandalize = spv_Dvandalize_1,
    spv_Dmessage = spv_Dmessage_1,
    spv_Dassault = spv_Dassault_1,
    spv_Djustify = spv_Djustify_1,
    spv_Dpredict = spv_Dpredict_1,
    # B: META
    # 1: REPUBLICANS
    metaSPV_Rprotest = metaSPV_Rprotest_1,
    metaSPV_Rvandalize = metaSPV_Rvandalize_1,
    metaSPV_Rmessage = metaSPV_Rmessage_1,
    metaSPV_Rassault = metaSPV_Rassault_1,
    metaSPV_Rjustify = metaSPV_Rjustify_1,
    metaSPV_Rpredict = metaSPV_Rpredict_1,
    # 2: DEMOCRATS
    metaSPV_Dprotest = metaSPV_Dprotest_1,
    metaSPV_Dvandalize = metaSPV_Dvandalize_1,
    metaSPV_Dmessage = metaSPV_Dmessage_1,
    metaSPV_Dassault = metaSPV_Dassault_1,
    metaSPV_Djustify = metaSPV_Djustify_1,
    metaSPV_Dpredict = metaSPV_Dpredict_1)

# 10. COMPILED AND CLEANED 
data_clean <- data_rename %>%
  mutate(
    # 10.1: STRENGTH OF PID
    # A: add column for average of PID strength questions based on party ID
    PID_strength = case_when(
      PID_general == 2 ~ rowMeans(across(PID_strengthR_1:PID_strengthR_4)),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(PID_strengthR_1:PID_strengthR_4)),
      PID_general == 1 ~ rowMeans(across(PID_strengthD_1:PID_strengthD_4)),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(PID_strengthD_1:PID_strengthD_4)),
      TRUE ~ NA_real_
    ),
    # B: create bins for PID strength
    PID_bin_category = case_when(
      PID_strength >= 3 ~ "strong",
      PID_strength <= 1.5 ~ "weak",
      PID_strength <=3 & PID_strength >=1.5 ~ "medium",
      TRUE ~ NA_character_
    ),
    # C: numeric version of above
    PID_bin = case_when(
      PID_strength >= 3 ~ 2,
      PID_strength <= 1 ~ 0,
      PID_strength <=3 & PID_strength >=1.5 ~ 1,
      TRUE ~ NA_real_),
    
    # 10.2: SOCIAL CLOSENESS SCALE
    social_closeness = case_when(
      PID_general == 2 ~ rowMeans(across(sc1_D:sc3_D)),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(sc1_D:sc3_D)),
      PID_general == 1 ~ rowMeans(across(sc1_R:sc3_R)),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(sc1_R:sc3_R)),
      TRUE ~ NA_real_),
    
    # 10.3: EMOTIONS
    angry_dummy = ifelse(is.na(emotion_table_1), 0, emotion_table_1),
    shame_dummy = ifelse(is.na(emotion_table_2), 0, emotion_table_2),
    guilt_dummy = ifelse(is.na(emotion_table_3), 0, emotion_table_3),
    disgust_dummy = ifelse(is.na(emotion_table_4), 0, emotion_table_4),
    fear_dummy = ifelse(is.na(emotion_table_5), 0, emotion_table_5),
    anxiety_dummy = ifelse(is.na(emotion_table_6), 0, emotion_table_6),
    sadness_dummy = ifelse(is.na(emotion_table_7), 0, emotion_table_7),
    happiness_dummy = ifelse(is.na(emotion_table_8), 0, emotion_table_8),
    none_dummy = ifelse(is.na(emotion_table_9), 0, emotion_table_9),
    
    # 10.4: IN-PARTY FEELING THERMOMETERS
    ft_pre_in = case_when(
      PID_general == 2 ~ ft_rep_pre,
      PID_general == 3 & ind_lean == 3 ~ ft_rep_pre,
      PID_general == 1 ~ ft_dem_pre,
      PID_general == 3 & ind_lean == 1 ~ ft_dem_pre,
      TRUE ~ NA_real_),
    ft_post_in = case_when(
      PID_general == 2 ~ ft_rep_post,
      PID_general == 3 & ind_lean == 3 ~ ft_rep_post,
      PID_general == 1 ~ ft_dem_post,
      PID_general == 3 & ind_lean == 1 ~ ft_dem_post,
      TRUE ~ NA_real_),
    
    # 10.5: OUT-PARTY FEELING THERMOMETERS
    ft_pre_out = case_when(
      PID_general == 2 ~ ft_dem_pre,
      PID_general == 3 & ind_lean == 3 ~ ft_dem_pre,
      PID_general == 1 ~ ft_rep_pre,
      PID_general == 3 & ind_lean == 1 ~ ft_rep_pre,
      TRUE ~ NA_real_),
    ft_post_out = case_when(
      PID_general == 2 ~ ft_dem_post,
      PID_general == 3 & ind_lean == 3 ~ ft_dem_post,
      PID_general == 1 ~ ft_rep_post,
      PID_general == 3 & ind_lean == 1 ~ ft_rep_post,
      TRUE ~ NA_real_),
    
    # 10.6: DIFFERENCES BETWEEN PRE- AND POST PID
    # A: difference in ft_pre_R v. ft_post_R
    ft_diff_R = ft_rep_post - ft_rep_pre,
    # B: difference in ft_pre_D v. ft_post_D
    ft_diff_D = ft_dem_post - ft_dem_pre,
    # C: difference in differences
    ft_diff_diff_PID = ft_diff_R - ft_diff_D,
    
    # 10.7: DIFFERENCES BETWEEN PRE- AND POST IN V. OUT-PARTY
    # A: difference in ft_pre_in v. ft_post_in
    ft_diff_in = ft_post_in - ft_pre_in,
    # B: difference in ft_pre_out v. ft_post_out
    ft_diff_out = ft_post_out - ft_pre_out,
    # C: difference in differences
    ft_diff_diff_inout = ft_diff_in - ft_diff_out,
    
    # 10.8: moral disengagement scores for outgroup
    moral_disengagement_outgroup = case_when(
      PID_general == 2 ~ rowMeans(across(md_D1:md_D3)),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(md_D1:md_D3)),
      PID_general == 1 ~ rowMeans(across(md_R1:md_R3)), 
      PID_general == 3 & ind_lean == 1 ~ rowSums(across(md_R1:md_R3)),
      TRUE ~ NA_real_),
    
    # 10.9: INDIVIDUAL SPV
    SPV_individual = case_when(
      # if Republican participant (PID_general == 2, OR if PID_general == 3 then ind_lean == 1)
      # calculate mean across 4 SPV questions
      PID_general == 2 ~ rowMeans(across(c(spv_Rprotest, spv_Rvandalize, spv_Rmessage,
                                           spv_Rassault, spv_Rjustify, spv_Rpredict))),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(spv_Rprotest, spv_Rvandalize, spv_Rmessage,
                                                           spv_Rassault, spv_Rjustify, spv_Rpredict))),
      # if Democratic participant (PID_general == 1, OR if PID_general == 3 then ind_lean == 3)
      # calculate mean across 4 SPV questions
      PID_general == 1 ~ rowMeans(across(c(spv_Dprotest, spv_Dvandalize, spv_Dmessage,
                                           spv_Dassault, spv_Djustify, spv_Dpredict))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(spv_Dprotest, spv_Dvandalize, spv_Dmessage,
                                                           spv_Dassault, spv_Djustify, spv_Dpredict))),
      TRUE ~ NA_real_),
    
    # 10.10: META-PERCEPTIONS OF SPV
    SPV_meta_R = case_when(
      PID_general == 1 ~ rowMeans(across(c(metaSPV_Dprotest, metaSPV_Dvandalize, metaSPV_Dmessage,
                                           metaSPV_Dassault, metaSPV_Djustify, metaSPV_Dpredict))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(metaSPV_Dprotest, metaSPV_Dvandalize, metaSPV_Dmessage,
                                                           metaSPV_Dassault, metaSPV_Djustify, metaSPV_Dpredict))),
      PID_general == 2 ~ rowMeans(across(c(metaSPV_Rprotest, metaSPV_Rvandalize, metaSPV_Rmessage,
                                           metaSPV_Rassault, metaSPV_Rjustify, metaSPV_Rpredict)), na.rm = TRUE),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(metaSPV_Rprotest, metaSPV_Rvandalize, metaSPV_Rmessage,
                                                           metaSPV_Rassault, metaSPV_Rjustify, metaSPV_Rpredict))),
      TRUE ~ NA_real_),
    SPV_meta_outgroup = case_when(
      PID_general == 1 ~ rowMeans(across(c(metaSPV_Dprotest, metaSPV_Dvandalize, metaSPV_Dmessage,
                                           metaSPV_Dassault, metaSPV_Djustify, metaSPV_Dpredict))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(metaSPV_Dprotest, metaSPV_Dvandalize, metaSPV_Dmessage,
                                                           metaSPV_Dassault, metaSPV_Djustify, metaSPV_Dpredict))),
      TRUE ~ NA_real_),
    SPV_meta_ingroup = case_when(
      PID_general == 2 ~ rowMeans(across(c(metaSPV_Rprotest, metaSPV_Rvandalize, metaSPV_Rmessage,
                                           metaSPV_Rassault, metaSPV_Rjustify, metaSPV_Rpredict)), na.rm = TRUE),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(metaSPV_Rprotest, metaSPV_Rvandalize, metaSPV_Rmessage,
                                                           metaSPV_Rassault, metaSPV_Rjustify, metaSPV_Rpredict))),
      TRUE ~ NA_real_),
    
    # 10.11: SPV BINS BY QUESTION GROUPS
    SPV_indiv_scenario = case_when(
      PID_general == 2 ~ rowMeans(across(c(spv_Rprotest, spv_Rvandalize, spv_Rmessage,
                                           spv_Rassault))),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(spv_Rprotest, spv_Rvandalize, spv_Rmessage,
                                                           spv_Rassault))),
      PID_general == 1 ~ rowMeans(across(c(spv_Dprotest, spv_Dvandalize, spv_Dmessage,
                                           spv_Dassault))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(spv_Dprotest, spv_Dvandalize, spv_Dmessage,
                                                           spv_Dassault))),
      TRUE ~ NA_real_),
    SPV_indiv_broad = case_when(
      PID_general == 2 ~ rowMeans(across(c(spv_Rjustify, spv_Rpredict))),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(spv_Rjustify, spv_Rpredict))),
      PID_general == 1 ~ rowMeans(across(c( spv_Djustify, spv_Dpredict))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(spv_Djustify, spv_Dpredict))),
      TRUE ~ NA_real_),
    SPV_meta_scenario = case_when(
      PID_general == 2 ~ rowMeans(across(c(metaSPV_Rprotest, metaSPV_Rvandalize, metaSPV_Rmessage,
                                           metaSPV_Rassault))),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(metaSPV_Rprotest, metaSPV_Rvandalize, metaSPV_Rmessage,
                                                           metaSPV_Rassault))),
      PID_general == 1 ~ rowMeans(across(c(metaSPV_Dprotest, metaSPV_Dvandalize, metaSPV_Dmessage,
                                           metaSPV_Dassault))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(metaSPV_Dprotest, metaSPV_Dvandalize, metaSPV_Dmessage,
                                                           metaSPV_Dassault))),
      TRUE ~ NA_real_),
    SPV_meta_broad = case_when(
      PID_general == 2 ~ rowMeans(across(c(metaSPV_Rjustify, metaSPV_Rpredict))),
      PID_general == 3 & ind_lean == 3 ~ rowMeans(across(c(metaSPV_Rjustify, metaSPV_Rpredict))),
      PID_general == 1 ~ rowMeans(across(c( metaSPV_Djustify, metaSPV_Dpredict))),
      PID_general == 3 & ind_lean == 1 ~ rowMeans(across(c(metaSPV_Djustify, metaSPV_Dpredict))),
      TRUE ~ NA_real_),
    
    # 10.12: SPV (indiv) BY TYPE OF ACTION
    SPV_indiv_protest = case_when(
      PID_general == 2 ~ spv_Rprotest,
      PID_general == 3 & ind_lean == 3 ~ spv_Rprotest,
      PID_general == 1 ~ spv_Dprotest,
      PID_general == 3 & ind_lean == 1 ~ spv_Dprotest,
      TRUE ~ NA_real_),
    SPV_indiv_vandalize = case_when(
      PID_general == 2 ~ spv_Rvandalize,
      PID_general == 3 & ind_lean == 3 ~ spv_Rvandalize,
      PID_general == 1 ~ spv_Dvandalize, 
      PID_general == 3 & ind_lean == 1 ~ spv_Dvandalize,
      TRUE ~ NA_real_),
    SPV_indiv_message = case_when(
      PID_general == 2 ~ spv_Rmessage, 
      PID_general == 3 & ind_lean == 3 ~ spv_Rmessage, 
      PID_general == 1 ~ spv_Dmessage, 
      PID_general == 3 & ind_lean == 1 ~ spv_Dmessage,
      TRUE ~ NA_real_),
    SPV_indiv_assault = case_when(
      PID_general == 2 ~ spv_Rassault, 
      PID_general == 3 & ind_lean == 3 ~ spv_Rassault,
      PID_general == 1 ~ spv_Dassault, 
      PID_general == 3 & ind_lean == 1 ~ spv_Dassault,
      TRUE ~ NA_real_),
    SPV_indiv_justify = case_when(
      PID_general == 2 ~ spv_Rjustify,
      PID_general == 3 & ind_lean == 3 ~ spv_Rjustify, 
      PID_general == 1 ~ spv_Djustify, 
      PID_general == 3 & ind_lean == 1 ~ spv_Djustify,
      TRUE ~ NA_real_),
    SPV_indiv_predict = case_when(
      PID_general == 2 ~ spv_Rpredict, 
      PID_general == 3 & ind_lean == 3 ~ spv_Rpredict, 
      PID_general == 1 ~ spv_Dpredict, 
      PID_general == 3 & ind_lean == 1 ~ spv_Dpredict,
      TRUE ~ NA_real_),
    
    # 10.13: SPV (meta) BY TYPE OF ACTION
    SPV_meta_protest = case_when(
      PID_general == 2 ~ metaSPV_Rprotest,
      PID_general == 3 & ind_lean == 3 ~ metaSPV_Rprotest,
      PID_general == 1 ~ metaSPV_Dprotest,
      PID_general == 3 & ind_lean == 1 ~ metaSPV_Dprotest,
      TRUE ~ NA_real_),
    SPV_meta_vandalize = case_when(
      PID_general == 2 ~ metaSPV_Rvandalize,
      PID_general == 3 & ind_lean == 3 ~ metaSPV_Rvandalize,
      PID_general == 1 ~ metaSPV_Dvandalize, 
      PID_general == 3 & ind_lean == 1 ~ metaSPV_Dvandalize,
      TRUE ~ NA_real_),
    SPV_meta_message = case_when(
      PID_general == 2 ~ metaSPV_Rmessage, 
      PID_general == 3 & ind_lean == 3 ~ metaSPV_Rmessage, 
      PID_general == 1 ~ metaSPV_Dmessage, 
      PID_general == 3 & ind_lean == 1 ~ metaSPV_Dmessage,
      TRUE ~ NA_real_),
    SPV_meta_assault = case_when(
      PID_general == 2 ~ metaSPV_Rassault, 
      PID_general == 3 & ind_lean == 3 ~ metaSPV_Rassault,
      PID_general == 1 ~ metaSPV_Dassault, 
      PID_general == 3 & ind_lean == 1 ~ metaSPV_Dassault,
      TRUE ~ NA_real_),
    SPV_meta_justify = case_when(
      PID_general == 2 ~ metaSPV_Rjustify,
      PID_general == 3 & ind_lean == 3 ~ metaSPV_Rjustify, 
      PID_general == 1 ~ metaSPV_Djustify, 
      PID_general == 3 & ind_lean == 1 ~ metaSPV_Djustify,
      TRUE ~ NA_real_),
    SPV_meta_predict = case_when(
      PID_general == 2 ~ metaSPV_Rpredict, 
      PID_general == 3 & ind_lean == 3 ~ metaSPV_Rpredict, 
      PID_general == 1 ~ metaSPV_Dpredict, 
      PID_general == 3 & ind_lean == 1 ~ metaSPV_Dpredict,
      TRUE ~ NA_real_))

# 11. View data set!
View(data_clean)
