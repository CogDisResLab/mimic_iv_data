# Process the t-test results from the previous script

library(tidyverse)

process_t_test_results <- function(disease) {

  t_test_file <- str_glue("results/compared_lab_values_{disease}_drg.csv")
  summary_file <- str_glue("results/summarised_lab_values_{disease}_drg.csv")

  t_test_results <- read_csv(t_test_file)

summary_data <- read_csv(summary_file) |>
    mutate(disease_group = if_else(disease == 1, "CASE", "CTRL")) |>
    select(-disease)


significant_t_tests <- t_test_results |>
    filter(significant) |>
    select(gender, label, cluster, estimate, p.value, prop_cases, prop_matched) |>
    arrange(p.value) |>
    mutate(
    label = fct_reorder(label, p.value),
    label = fct_rev(label)
  ) |>
  write_csv(str_glue("results/compared_lab_values_{disease}_drg_significant.csv")) |>
  select(gender, label, estimate, cluster) |>
  pivot_wider(names_from = gender, values_from = estimate) |>
  filter(!is.na(`F`), !is.na(`M`)) |>
  rename(Female = `F`, Male = `M`) |>
  mutate(Same_Direction = sign(Female) == sign(Male)) |>
  left_join(summary_data, by = c("label", "cluster")) |>
  write_csv(str_glue("results/compared_lab_values_{disease}_drg_significant_same_direction.csv"))

significant_effect_sizes <- t_test_results |>
    filter(significant) |>
    select(gender, label, effect_size, cluster, p.value) |>
    arrange(p.value) |>
    mutate(
    label = fct_reorder(label, p.value),
    label = fct_rev(label)
  ) |>
  write_csv(str_glue("results/compared_lab_values_{disease}_drg_significant_effectsize.csv")) |>
  select(gender, label, effect_size, cluster) |>
  pivot_wider(names_from = gender, values_from = effect_size) |>
  filter(!is.na(`F`), !is.na(`M`)) |>
  rename(Female = `F`, Male = `M`) |>
  mutate(Same_Direction = sign(Female) == sign(Male)) |>
  left_join(summary_data, by = c("label", "cluster")) |>
  write_csv(str_glue("results/compared_lab_values_{disease}_drg_significant_same_direction_effectsize.csv"))
}


diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

walk(diseases, process_t_test_results)
#