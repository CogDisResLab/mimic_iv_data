# Process the t-test results from the previous script

library(tidyverse)

t_test_results <- read_csv("results/compared_lab_values_overall_alzheimers.csv") |>
  select(-ncases, -nmatched)

summary_data <- read_csv("results/summarised_lab_values_overall_alzheimers.csv") |>
  mutate(disease_group = if_else(disease == 1, "CASE", "CTRL")) |>
  select(-disease, -valueuom)


significant_t_tests <- t_test_results |>
  filter(significant) |>
  select(label, estimate, p.value) |>
  arrange(p.value) |>
  mutate(
    label = fct_reorder(label, p.value),
    label = fct_rev(label)
  ) |>
  left_join(summary_data, by = "label") |>
  write_csv("results/compared_lab_values_overall_significant.csv")

significant_effect_sizes <- t_test_results |>
  filter(significant) |>
  select(label, effect_size, p.value) |>
  arrange(p.value) |>
  mutate(
    label = fct_reorder(label, p.value),
    label = fct_rev(label)
  ) |>
  left_join(summary_data, by = "label") |>
  write_csv("results/compared_lab_values_overall_significant.csv")

