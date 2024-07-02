# Join the significant effect size datasets by disease and split by DRG.

library(tidyverse)
library(writexl)

result_files <- list.files("results/", full.names = TRUE) |>
  keep(~ str_detect(.x, "drg_significant_effectsize.csv"))

overall_files <- result_files |>
  keep(~ str_detect(.x, "overall"))

gendered_files <- setdiff(result_files, overall_files)

overall_data <- overall_files |>
  set_names(~ str_extract(.x, "compared_lab_values_overall_(.*)_drg_significant_effectsize.csv", 1)) |>
  map(read_csv) |>
  bind_rows(.id = "disaese") |>
  nest(.by = cluster) |>
  deframe() |>
  write_xlsx("results/overall_drg_effect_sizes.xlsx")

gendered_data <- gendered_files |>
  set_names(~ str_extract(.x, "compared_lab_values_(.*)_drg_significant_effectsize.csv", 1)) |>
  map(read_csv) |>
  bind_rows(.id = "disaese") |>
  nest(.by = cluster) |>
  deframe() |>
  write_xlsx("results/gendered_drg_effect_sizes.xlsx")
