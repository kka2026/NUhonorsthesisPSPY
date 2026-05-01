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

