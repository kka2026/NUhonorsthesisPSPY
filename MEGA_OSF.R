# ============================================================================================================================
#                                                              CLEANING
# ===========================================================================================================================
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


# ============================================================================================================================
#                                                              DEMOGRAPHICS
# ============================================================================================================================
library(readr)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(janitor)

demo_raw <- read_csv('/Users/katiekeil/Desktop/Northwestern/Honors/R/FIRST RUN-THRU 3:14/prolific_demographic_export_699cd03a5f7c64e418b72a8b.csv')
View(demo_raw)

demo_filter <- filter(demo_raw, Status == "APPROVED")
View(demo_filter)

demo_clean <- demo_filter %>%
  mutate(across(-Age, ~as.character(.)),
         trump_vote = ifelse(`2020 us presidential election` == "Donald Trump"
                             & `2024 us presidential election` == "Donald Trump", 1, 0))
View(demo_clean)

cat_vars <- c(
  "Gender",
  "Ethnicity",
  "Highest education level completed",
  "Household income (usd) [us participants only]",
  "Sex",
  "2020 us presidential election",
  "2024 us presidential election"
)
num_var <- "Age"
cat_tables <- map_df(
  cat_vars,
  ~ demo_clean %>%
    filter(!is.na(.data[[.x]])) %>%
    count(Variable = .x, Category = .data[[.x]]) %>%
    mutate(
      Percent = round(100 * n / sum(n), 1),
      N = n
    ) %>%
    select(Variable, Category, N, Percent)
)
num <- as.numeric(demo_clean[[num_var]])
num_table <- tibble(
  Variable = num_var,
  Category = c("Mean (SD)", "Median", "Range"),
  N = c(
    sprintf("%.1f (%.1f)", mean(num, na.rm=TRUE), sd(num, na.rm=TRUE)),
    sprintf("%.1f", median(num, na.rm=TRUE)),
    sprintf("%d–%d", min(num, na.rm=TRUE), max(num, na.rm=TRUE))
  ),
  Percent = NA
)
cat_tables <- cat_tables %>%
  mutate(N = as.character(N))
demo_table <- bind_rows(cat_tables, num_table)

library(gt)
gt_table <- demo_table %>%
  gt(
    rowname_col = "Category",
    groupname_col = "Variable"
  ) %>%
  tab_header(
    title = "Table 4.5: Demographic Characteristics",
    subtitle = "Frequencies, Percentages, and Summary Statistics"
  ) %>%
  cols_label(
    N = "N",
    Percent = "%"
  ) %>%
  fmt_percent(
    columns = Percent,
    scale_values = FALSE,
    decimals = 1
  ) %>%
  tab_options(
    table.font.size = 12,
    row_group.as_column = TRUE,
    data_row.padding = px(4)
  )
gt_table
gtsave(gt_table, "demographics_table_main.html")


# ================================================================
#                  PARTICIPANT COUNT TABLES
# ================================================================
# 1. BUILD EACH SECTION AS A LONG TIBBLE WITH Variable + Category + N + Percent

# 1.1: BY CONDITION
count_condition <- data_clean %>%
  count(Variable = "Condition", Category = condition) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         N = as.character(n)) %>%
  select(Variable, Category, N, Percent)

# 1.2: BY PARTY BIN
count_party_bin <- data_clean %>%
  filter(!is.na(party_category)) %>%
  count(Variable = "Party Group", Category = as.character(party_category)) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         N = as.character(n)) %>%
  select(Variable, Category, N, Percent)

# 1.3: BY 7-POINT PARTY ID
count_party_numeric <- data_clean %>%
  filter(!is.na(party_scale)) %>%
  count(Variable = "7-Point Party ID", Category = as.character(party_scale)) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         N = as.character(n)) %>%
  select(Variable, Category, N, Percent)

# 2. BIND AND EXPORT AS SINGLE GT TABLE
count_table <- bind_rows(count_condition, count_party_bin, count_party_numeric)

gt_count <- count_table %>%
  gt(
    rowname_col  = "Category",
    groupname_col = "Variable"
  ) %>%
  tab_header(
    title    = "Table X: Participant Distribution",
    subtitle = "Frequencies and Percentages by Condition and Party"
  ) %>%
  cols_label(
    N       = "N",
    Percent = "%"
  ) %>%
  fmt_percent(
    columns      = Percent,
    scale_values = FALSE,
    decimals     = 1
  ) %>%
  tab_options(
    table.font.size      = 12,
    row_group.as_column  = TRUE,
    data_row.padding     = px(4)
  )

gt_count
gtsave(gt_count, "count_table_combined.html")



# ============================================================================================================================
#                                                              DESCRIPTIVE STATS
# ===========================================================================================================================
# 0. HELPER FUNCTION: correlation matrix with stars
cor_with_stars <- function(df) {
  n    <- ncol(df)
  cols <- names(df)
  # compute r and p for every pair
  r_mat <- matrix(NA_real_,    n, n, dimnames = list(cols, cols))
  p_mat <- matrix(NA_real_,    n, n, dimnames = list(cols, cols))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        r_mat[i, j] <- 1
        p_mat[i, j] <- NA
      } else {
        test         <- cor.test(df[[i]], df[[j]], use = "pairwise.complete.obs")
        r_mat[i, j]  <- test$estimate
        p_mat[i, j]  <- test$p.value
      }}}
  # paste stars onto rounded r values
  star_mat <- matrix("", n, n, dimnames = list(cols, cols))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        star_mat[i, j] <- "—"
      } else {
        r <- round(r_mat[i, j], 2)
        p <- p_mat[i, j]
        stars <- if (!is.na(p) && p < .001) "***" else
          if (!is.na(p) && p < .01)  "**"  else
            if (!is.na(p) && p < .05)  "*"   else ""
        star_mat[i, j] <- paste0(sprintf("%.2f", r), stars) %>% 
          str_replace_all("\\* \\* \\*", "***") %>%
          str_replace_all("\\* \\*", "**")}}}
  as.data.frame(star_mat)}



# 1. load library
library(readxl)
library(tidyverse)
library(interactions)
library(broom)
library(emmeans)
library(stargazer)
library(modelsummary)


# 2. compile list of descriptive stats
descriptive_stats <- data_clean %>%
  group_by(exp_group_numeric) %>%
  dplyr::summarise(
    mean_PID_strength        = mean(PID_strength, na.rm = TRUE),
    sd_PID_strength          = sd(PID_strength, na.rm = TRUE),
    mean_ideology_selfreport = mean(as.numeric(ideology_selfreport), na.rm = TRUE),
    sd_ideology_selfreport   = sd(as.numeric(ideology_selfreport), na.rm = TRUE),
    mean_ft_diff_R           = mean(ft_diff_R, na.rm = TRUE),
    mean_ft_diff_D           = mean(ft_diff_D, na.rm = TRUE),
    sd_ft_diff_R             = sd(ft_diff_R, na.rm = TRUE),
    sd_ft_diff_D             = sd(ft_diff_D, na.rm = TRUE),
    mean_ft_diff_diff        = mean(ft_diff_diff_PID, na.rm = TRUE),
    sd_ft_diff_diff          = sd(ft_diff_diff_PID, na.rm = TRUE),
    mean_moral_out           = mean(moral_disengagement_outgroup, na.rm = TRUE),
    sd_moral_out             = sd(moral_disengagement_outgroup, na.rm = TRUE),
    mean_SPV_individual      = mean(SPV_individual, na.rm = TRUE),
    sd_SPV_individual        = sd(SPV_individual, na.rm = TRUE),
    mean_SPV_meta_R          = mean(SPV_meta_R, na.rm = TRUE),
    sd_SPV_meta_R            = sd(SPV_meta_R, na.rm = TRUE),
    count                    = n(),
    .groups = "drop")
items <- list(
  "Strength of Partisan Identity"                   = c("mean_PID_strength", "sd_PID_strength"),
  "Self-Reported Ideology"                          = c("mean_ideology_selfreport", "sd_ideology_selfreport"),
  "Difference in Affective Ratings: Republican Party" = c("mean_ft_diff_R", "sd_ft_diff_R"),
  "Difference in Affective Ratings: Democratic Party" = c("mean_ft_diff_D", "sd_ft_diff_D"),
  "Difference in Affective Ratings Between Parties" = c("mean_ft_diff_diff", "sd_ft_diff_diff"),
  "Moral Disengagement: Opposing Party"              = c("mean_moral_out", "sd_moral_out"),
  "Individual SPV"                                  = c("mean_SPV_individual", "sd_SPV_individual"),
  "Meta-Perceptions of Republican SPV"              = c("mean_SPV_meta_R", "sd_SPV_meta_R"),
  "N"                                              = c("count", "count")
)

# ----------------------------------------------------------------
#         HELPER: significance stars from t-test
# ----------------------------------------------------------------
get_stars <- function(group1_vals, group2_vals) {
  test <- t.test(group1_vals, group2_vals)
  p    <- test$p.value
  if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
}

# ----------------------------------------------------------------
#         MAP ITEM LABELS TO RAW DATA_CLEAN COLUMN NAMES
# ----------------------------------------------------------------
item_cols <- list(
  "Strength of Partisan Identity"                     = "PID_strength",
  "Self-Reported Ideology"                            = "ideology_selfreport",
  "Difference in Affective Ratings: Republican Party" = "ft_diff_R",
  "Difference in Affective Ratings: Democratic Party" = "ft_diff_D",
  "Difference in Affective Ratings Between Parties"   = "ft_diff_diff_PID",
  "Moral Disengagement: Opposing Party"               = "moral_disengagement_outgroup",
  "Individual SPV"                                    = "SPV_individual",
  "Meta-Perceptions of Republican SPV"                = "SPV_meta_R"
)

# ----------------------------------------------------------------
#         COMPUTE STARS
# ----------------------------------------------------------------
stars_R <- sapply(names(item_cols), function(label) {
  col <- item_cols[[label]]
  get_stars(
    na.omit((data_clean %>% filter(exp_group_numeric == 1))[[col]]),
    na.omit((data_clean %>% filter(exp_group_numeric == 3))[[col]])
  )
})

stars_D <- sapply(names(item_cols), function(label) {
  col <- item_cols[[label]]
  get_stars(
    na.omit((data_clean %>% filter(exp_group_numeric == 2))[[col]]),
    na.omit((data_clean %>% filter(exp_group_numeric == 4))[[col]])
  )
})


# 3. create table for only Republicans Control and Treatment
  # 3.1: make groups
      gR_control <- 2
      gR_treat <- 4
      R_control <- descriptive_stats %>% filter(exp_group_numeric == gR_control)
      R_treat <- descriptive_stats %>% filter(exp_group_numeric == gR_treat)
      
  # 3.2: form column structure
      dem_rows <- lapply(names(items), function(label) {
        cols    <- items[[label]]
        mean_col <- cols[1]; sd_col <- cols[2]
        
  # 3.3: how to pull means and SRs for each column         
        valR_control <- if (!is.na(R_control[[mean_col]])) {
          if (mean_col == "count") sprintf("%d", R_control[[mean_col]]) else sprintf("%.2f (%.2f)", R_control[[mean_col]], R_control[[sd_col]])
        } else NA_character_
        star <- if (label %in% names(stars_R)) stars_R[[label]] else ""
        valR_treat <- if (!is.na(R_treat[[mean_col]])) {
          if (mean_col == "count") sprintf("%d", R_treat[[mean_col]])
          else sprintf("%.2f (%.2f)%s", R_treat[[mean_col]], R_treat[[sd_col]], star)
        } else NA_character_
        
  # 3.4: bind into table         
        tibble::tibble(Variable = label, Group_R_control = valR_control, Group_R_treat = valR_treat)
      }) %>% dplyr::bind_rows()
      
  # 3.5: make the labels for table and fill in      
      nR_control <- R_control %>% pull(count) %>% unique()
      nR_treat <- R_treat %>% pull(count) %>% unique()
      colnames(dem_rows)[2:3] <- c(
        paste0("Control x Republican"),
        paste0("Treatment x Republican"))
      dem_text <- knitr::kable(dem_rows, format = "html")
      note     <- "<tfoot><tr><td colspan='3'><em>Note:</em> * p &lt; .05, ** p &lt; .01, *** p &lt; .001 </td></tr></tfoot>"
      dem_html <- sub("</table>", paste0(note, "</table>"), dem_text)
      cat(dem_html, file = "Descriptive_Statistics_R.htm", sep = "\n")

# 4. create table for only Democrats Control and Treatment
  # 4.1: make groups
      gD_control <- 2
      gD_treat <- 4
      D_control <- descriptive_stats %>% filter(exp_group_numeric == gD_control)
      D_treat <- descriptive_stats %>% filter(exp_group_numeric == gD_treat)
      
  # 4.2: form column structure
      dem_rows <- lapply(names(items), function(label) {
        cols    <- items[[label]]
        mean_col <- cols[1]; sd_col <- cols[2]
        
  # 4.3: how to pull means and SDs for each column         
        valD_control <- if (!is.na(D_control[[mean_col]])) {
          if (mean_col == "count") sprintf("%d", D_control[[mean_col]]) else sprintf("%.2f (%.2f)", D_control[[mean_col]], D_control[[sd_col]])
        } else NA_character_
        star <- if (label %in% names(stars_D)) stars_D[[label]] else ""
        valD_treat <- if (!is.na(D_treat[[mean_col]])) {
          if (mean_col == "count") sprintf("%d", D_treat[[mean_col]])
          else sprintf("%.2f (%.2f)%s", D_treat[[mean_col]], D_treat[[sd_col]], star)
        } else NA_character_
        
  # 4.4: bind into table         
        tibble::tibble(Variable = label, Group_D_control = valD_control, Group_D_treat = valD_treat)
      }) %>% dplyr::bind_rows()
      
  # 4.5: make the labels for table and fill in      
      nD_control <- D_control %>% pull(count) %>% unique()
      nD_treat <- D_treat %>% pull(count) %>% unique()
      colnames(dem_rows)[2:3] <- c(
        paste0("Control x Democrat"),
        paste0("Treatment x Democrat"))
      dem_text <- knitr::kable(dem_rows, format = "html")
      note     <- "<tfoot><tr><td colspan='3'><em>Note:</em> * p &lt; .05, ** p &lt; .01, *** p &lt; .001 </td></tr></tfoot>"
      dem_html <- sub("</table>", paste0(note, "</table>"), dem_text)
      cat(dem_html, file = "Descriptive_Statistics_D.htm", sep = "\n")

# 5. combine both tables into one
   # 5.1: make groups
      groups <- list(
        list(num = 1, col = "Control x Republican"),
        list(num = 3, col = "Treatment x Republican"),
        list(num = 2, col = "Control x Democrat"),
        list(num = 4, col = "Treatment x Democrat"))
      
  # 5.2: form column structure   
      combined_rows <- lapply(names(items), function(label) {
        cols     <- items[[label]]
        mean_col <- cols[1]; sd_col <- cols[2]
        
  # 5.3: how to pull means and SDs for each column  
      vals <- lapply(groups, function(g) {
        grp <- descriptive_stats %>% filter(exp_group_numeric == g$num)
        if (!is.na(grp[[mean_col]])) {
          if (mean_col == "count") sprintf("%d", grp[[mean_col]]) 
          else sprintf("%.2f (%.2f)", grp[[mean_col]], grp[[sd_col]])
        } else NA_character_
      })
      
  # 5.4: bind into table
      row <- tibble::tibble(Variable = label)
      for (i in seq_along(groups)) row[[groups[[i]]$col]] <- vals[[i]]
      row
    }) %>% dplyr::bind_rows()
      
  # 5.5: export and fill in    
    combined_text <- knitr::kable(combined_rows, format = "html")
    cat(combined_text, file = "Descriptive_Statistics_Combined.htm", sep = "\n")

# 6. CORRELATIONS
      # 6.1: correlations between individual SPV and metaperceptions and feeling thermometers
          # A: SPV v. in-party meta-SPV
          cor.test(data_clean$SPV_individual, data_clean$SPV_meta_R)
          # B: SPV v. ft_diff_out
          cor.test(data_clean$SPV_individual, data_clean$ft_diff_out)
          # C: SPV v. ft_diff_in
          cor.test(data_clean$SPV_individual, data_clean$ft_diff_in)
          # D: in-party meta-SPV v. ft_diff_in
          cor.test(data_clean$SPV_meta_R, data_clean$ft_diff_in)
          # E: out-party meta-SPV v. ft_diff_out
          cor.test(data_clean$SPV_meta_R, data_clean$ft_diff_out)
          # F: broad v. specific SPV
          cor.test(data_clean$SPV_indiv_scenario, data_clean$SPV_indiv_broad)
          cor.test(data_clean$SPV_meta_scenario, data_clean$SPV_meta_broad)
          
     # 6.2: make pre-treatment variables correlation table using stargazer
         # A: select variables
          pretreat_matrix <- data_clean %>%
            dplyr::select (condition_dummy, 
                           party_numeric, ideology_selfreport, maga, PID_strength, social_closeness, ft_rep_pre, ft_dem_pre)
         # B: select pretreat correlation method
          pretreat_results <- round(cor(pretreat_matrix, use = "pairwise.complete.obs"), 2)
         # C: add in labels and export
          pretreat_names <- c("Treatment",                                     # condition_dummy
                              "Party Affiliation Strength",                    # party_numeric
                              "Ideology",                                      # ideology_selfreport
                              "MAGA Support",                                  # maga
                              "PID Strength",                                  # PID_strength
                              "Social Closeness",                              # social_closeness
                              "FT Republican (Pre-Treatment)",                 # ft_rep_pre
                              "FT Democratic (Pre-Treatment)")                 # ft_dem_pre
          pretreat_stars           <- cor_with_stars(pretreat_matrix)
          rownames(pretreat_stars) <- pretreat_names
          colnames(pretreat_stars) <- pretreat_names
          
          out_html <- capture.output(
            stargazer(pretreat_stars, type = "html", summary = FALSE,
                      title = "Correlation Matrix For Pre-Treatment Variables",
                      notes = "* p < .05, ** p < .01, *** p < .001")
          )
          out_html <- gsub("\\* \\* \\*", "***", out_html)
          out_html <- gsub("\\* \\*",     "**",  out_html)
          cat(out_html, file = "PreTreat_Matrix.htm", sep = "\n")
          
     # 6.3: make post-treatment variables correlation table using stargazer
         # A: select variables
          postreat_matrix <- data_clean %>%
            dplyr::select (condition_dummy,
                           ft_rep_post, ft_dem_post, ft_diff_in, ft_diff_out,
                           moral_disengagement_outgroup, SPV_individual, SPV_meta_R, SPV_indiv_scenario, SPV_indiv_broad, SPV_meta_scenario, SPV_meta_broad)
         # B: select postreat correlation method
          postreat_results <- round(cor(postreat_matrix, use = "pairwise.complete.obs"), 2)
         # C: add in labels and export
          postreat_names <- c("Treatment",                                                  # condition_dummy
                               "FT Republican (Post-Treatment)",                            # ft_rep_post
                               "FT Democratic (Post-Treatment)",                            # ft_dem_post
                               "Difference in FT Ratings: In-Party",                        # ft_diff_in
                               "Difference in FT Ratings: Out-Party",                       # ft_diff_out
                               "Moral Disengagement (Outgroup)",                            # moral_disengagement_outgroup
                               "Individual SPV",                                            # SPV_individual
                               "Meta-Perceptions of Republicans' SPV",                      # SPV_meta_R
                               "Individual SPV: Scenario-Based",                            # SPV_indiv_scenario
                               "Individual SPV: Broad-Based",                               # SPV_indiv_broad
                               "Meta-Perceptions of Republicans' SPV: Scenario-Based",      # SPV_meta_scenario
                               "Meta-Perceptions of Republicans' SPV: Broad-Based")         # SPV_meta_broad
          postreat_stars           <- cor_with_stars(postreat_matrix)
          rownames(postreat_stars) <- postreat_names
          colnames(postreat_stars) <- postreat_names
          
          out_html <- capture.output(
            stargazer(postreat_stars, type = "html", summary = FALSE,
                      title = "Correlation Matrix For Post-Treatment Variables",
                      notes = "* p < .05, ** p < .01, *** p < .001")
          )
          out_html <- gsub("\\* \\* \\*", "***", out_html)
          out_html <- gsub("\\* \\*",     "**",  out_html)
          cat(out_html, file = "PostTreat_Matrix.htm", sep = "\n")
          
      # 6.4: correlation table for emotions
          # A: select variables
              emotion_matrix <- data_clean %>%
                dplyr::select (condition_dummy,
                               angry_dummy,
                               shame_dummy,
                               guilt_dummy,
                               disgust_dummy,
                               fear_dummy,
                               anxiety_dummy,
                               sadness_dummy,
                               happiness_dummy)
          # B: select correlation method
              emotion_results <- round(cor(emotion_matrix, use = "pairwise.complete.obs"), 2)
          # C: add in labels and export
              emotion_names <- c("Treatment",                  # condition_dummy
                                 "Angry",                      # angry_dummy
                                 "Shame",                      # shame_dummy
                                 "Guilt",                      # guilt_dummy
                                 "Disgust",                    # disgust_dummy
                                 "Fear",                       # fear_dummy
                                 "Anxiety",                    # anxiety_dummy
                                 "Sadness",                    # sadness_dummy
                                 "Happiness")                  # happiness_dummy
              emotion_stars           <- cor_with_stars(emotion_matrix)
              rownames(emotion_stars) <- emotion_names
              colnames(emotion_stars) <- emotion_names
              out_html <- capture.output(
                stargazer(emotion_stars, type = "html", summary = FALSE,
                          title = "Correlation Matrix for Emotions",
                          notes = "* p < .05, ** p < .01, *** p < .001")
              )
              out_html <- gsub("\\* \\* \\*", "***", out_html)
              out_html <- gsub("\\* \\*",     "**",  out_html)
              cat(out_html, file = "Emotions_Matrix.htm", sep = "\n")

              

              

# ============================================================================================================================
#                                                    MAIN ANALYSES
# ===========================================================================================================================
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

              
# ================================================================
#   H1: TREATMENT x PID -> META-PERCEPTIONS OF REPUBLICAN SPV
# ================================================================
              
# |--------------------------------|
#          SIMPLE MODELS
# |--------------------------------|
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
        
# |--------------------------------|
#          + COVARIATES
# |--------------------------------|
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
    
        
# ================================================================
#           H2: TREATMENT x PID -> MORAL DISENGAGEMENT
# ================================================================
        
# |--------------------------------|
#          SIMPLE MODELS
# |--------------------------------|
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
        
# |--------------------------------|
#          + COVARIATES
# |--------------------------------|
        
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
        
        
# ================================================================
#           H3: TREATMENT x PID ->  INDIVIDUAL SPV
# ================================================================
        
# |--------------------------------|
#          SIMPLE MODELS
# |--------------------------------|
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
    
# |--------------------------------|
#          + COVARIATES
# |--------------------------------|
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

# =======================================
#       EXPORTING PRIMARY HYPOTHESES
# =======================================
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
        

# ================================================================
#                        H4: FT_REPUBLICAN
# ================================================================
        
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

# |--------------------------------|
#          + COVARIATES
# |--------------------------------|
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
        
        
# ================================================================
#                    H5: PID STRENGTH -> FT_REP
# ================================================================
        
# |--------------------------------|
#          SIMPLE MODELS
# |--------------------------------|
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

# |--------------------------------|
#          + COVARIATES
# |--------------------------------|
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
        
      
# ================================================================
#                   H6: PID_STRENGTH ---> FT_DEM
# ================================================================
      
# |--------------------------------|
#          SIMPLE MODELS
# |--------------------------------|
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
          
# |--------------------------------|
#          + COVARIATES
# |--------------------------------|
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

# =======================================
#      EXPORTING SECONDARY HYPOTHESES
# =======================================         
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
          
          
          
          
# ============================================================================================================================
#                                                EXPLORATORY ANALYSES
# ===========================================================================================================================
                       
      
# ==================================================
#               FEELING THERMOMETERS
# ==================================================
    
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

    
# ==================================================
#                       SPV
# ==================================================
    
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

       
# ========================================================================================================================================
                                                            # EXPORTING VISUALS
# ========================================================================================================================================
library(ggplot2)

# ----------------------------------------------------------------
#         ASSIGN ALL PLOTS TO OBJECTS
# ----------------------------------------------------------------

# primary hypotheses
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

# secondary hypotheses
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

# SPV item plots
p_protest  <- ggplot(protest_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) + geom_point(size=3,position=position_dodge(0.5)) + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) + geom_hline(yintercept=0,linetype="dashed",color="grey50") + scale_color_manual(values=c("Individual Ratings"="salmon","Meta-Perceptions of Republicans' Ratings"="firebrick")) + labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Protesting Without a Permit") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
p_vandalize <- ggplot(vandalize_combined_df,aes(x=party_numeric,y=estimate,color=model,group=model)) + geom_point(size=3,position=position_dodge(0.5)) + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) + geom_hline(yintercept=0,linetype="dashed",color="grey50") + scale_color_manual(values=c("Individual Ratings"="peachpuff","Meta-Perceptions of Republicans' Ratings"="coral")) + labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Vandalizing Opposing Party's Signs") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
p_message  <- ggplot(message_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) + geom_point(size=3,position=position_dodge(0.5)) + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) + geom_hline(yintercept=0,linetype="dashed",color="grey50") + scale_color_manual(values=c("Individual Ratings"="gold","Meta-Perceptions of Republicans' Ratings"="goldenrod")) + labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Messaging Threats Online") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
p_assault  <- ggplot(assault_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) + geom_point(size=3,position=position_dodge(0.5)) + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) + geom_hline(yintercept=0,linetype="dashed",color="grey50") + scale_color_manual(values=c("Individual Ratings"="yellowgreen","Meta-Perceptions of Republicans' Ratings"="forestgreen")) + labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Physically Assaulting an Out-Partisan") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
p_justify  <- ggplot(justify_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) + geom_point(size=3,position=position_dodge(0.5)) + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) + geom_hline(yintercept=0,linetype="dashed",color="grey50") + scale_color_manual(values=c("Individual Ratings"="lightblue","Meta-Perceptions of Republicans' Ratings"="dodgerblue")) + labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: Achieving In-Party Political Goals") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
p_predict  <- ggplot(predict_combined_df,  aes(x=party_numeric,y=estimate,color=model,group=model)) + geom_point(size=3,position=position_dodge(0.5)) + geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,size=0.5,position=position_dodge(0.5)) + geom_hline(yintercept=0,linetype="dashed",color="grey50") + scale_color_manual(values=c("Individual Ratings"="thistle","Meta-Perceptions of Republicans' Ratings"="orchid")) + labs(x="Party ID",y="Difference in Means (Treatment — Control)",title="SPV: If Opposing Party Wins Next Election") + theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))

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
