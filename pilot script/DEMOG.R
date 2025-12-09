library(readr)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(janitor)

demo_raw <- read_csv('/Users/katieakeil/Desktop/Northwestern/Honors/R/DEMO PILOT.csv')
View(demo_raw)

demo_filter <- filter(demo_raw, Status == "APPROVED")
View(demo_filter)

demo_clean <- demo_filter %>%
  mutate(across(-Age, ~as.character(.)),
         trump_vote = ifelse(`2020 us presidential election` == "Donald Trump"
                             & `2024 us presidential election` == "Donald Trump", 1, 0))
View(demo_clean)

# ------ TABLE

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
    title = "Table 1: Demographic Characteristics",
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
gtsave(gt_table, "demographics_table.html")

