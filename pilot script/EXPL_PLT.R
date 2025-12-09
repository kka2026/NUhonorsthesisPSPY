library(readxl)
library(tidyverse)
library(psych)
library(interactions)
library(broom)
library(emmeans)
library(effsize)
library(estimatr)

source("CLN_PLT.R")
source("DESC_PLT.R")



# helper to summarize a variable by `condition` and produce mean, se, 95% CI
summarise_var <- function(data, varname) {
  data %>%
    group_by(condition) %>%
    summarise(
      n = sum(!is.na(.data[[varname]])),
      mean = mean(as.numeric(.data[[varname]]), na.rm = TRUE),
      sd = sd(as.numeric(.data[[varname]]), na.rm = TRUE),
      se = ifelse(n > 0, sd / sqrt(n), NA_real_),
      conf.low = mean - qt(0.975, df = pmax(n - 1, 1)) * se,
      conf.high = mean + qt(0.975, df = pmax(n - 1, 1)) * se,
      .groups = "drop"
    )
}

# ------- T-TESTS

t.test(moral_disengagement_outgroup ~ exp_group, data = pilot_clean, var.equal = FALSE)
t.test(SPV_individual ~ exp_group, data = pilot_clean, var.equal = FALSE)
t.test(SPV_meta_outgroup ~ exp_group, data = pilot_clean, var.equal = FALSE)
t.test(SPV_meta_ingroup ~ exp_group, data = pilot_clean, var.equal = FALSE)


# ----- BASIC DV PLOTS

# 1) moral disengagement (outgroup)
df_moral <- summarise_var(pilot_clean, "moral_disengagement_outgroup")
ggplot(df_moral, aes(x = condition, y = mean)) +
  geom_col(stat = "identity", width = 0.5, fill = "firebrick") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(x = "Experimental Condition", y = "Moral Disengagement (Mean)",
    title = "Mean Levels of Moral Disengagement by Experimental Condition") +
  ylim(0, 5) +
  theme_minimal()

# 2) individual SPV
df_spv_ind <- summarise_var(pilot_clean, "SPV_individual")
ggplot(df_spv_ind, aes(x = condition, y = mean)) +
  geom_col(stat = "identity", width = 0.5, fill = "lightpink") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(x = "Experimental Condition", y = "Individual SPV (Mean)",
    title = "Mean Levels of Individual SPV by Experimental Condition") +
  ylim(0, 100) +
  theme_minimal()

# 3) meta-SPV (outgroup)
df_spv_meta_out <- summarise_var(pilot_clean, "SPV_meta_outgroup")
ggplot(df_spv_meta_out, aes(x = condition, y = mean)) +
  geom_col(stat = "identity", width = 0.5, fill = "dodgerblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(x = "Experimental Condition", y = "Out-Party Meta-SPV (Mean)",
    title = "Mean Meta-Perceptions of Outgroup SPV by Experimental Condition") +
  ylim(0, 100) +
  theme_minimal()

# 4) meta-SPV (ingroup)
df_spv_meta_in <- summarise_var(pilot_clean, "SPV_meta_ingroup")
ggplot(df_spv_meta_in, aes(x = condition, y = mean)) +
  geom_col(stat = "identity", width = 0.5, fill = "lightblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(x = "Experimental Condition", y = "In-Party Meta-SPV (Mean)",
    title = "Mean Meta-Perceptions of Ingroup SPV by Experimental Condition") +
  ylim(0, 100) +
  theme_minimal()


