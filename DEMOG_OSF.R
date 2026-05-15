library(readr)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(janitor)

demo_raw <- read_csv('PATH TO DATA FILE')
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

