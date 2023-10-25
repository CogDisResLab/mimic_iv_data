# Summarise Lab Values by DRG Cluster

library(tidyverse)

summarise_values <- function(disease) {

  in_dir <- file.path("data", disease)

  out_dir <- in_dir

  labvalues <- read_csv(file.path(in_dir, str_glue("matched_labvalues_selected_{disease}_drg.csv")), show_col_types = FALSE) |>
    select(-charttime)

  summarised_overall <- labvalues |>
    group_by(disease, label, cluster, valueuom) |>
    summarise(mean_value = mean(valuenum, na.rm = TRUE),
              sd_value = sd(valuenum, na.rm = TRUE),
              min_value = min(valuenum, na.rm = TRUE),
              max_value = max(valuenum, na.rm = TRUE),
              median_value = median(valuenum, na.rm = TRUE),
              count_value = n(),
              .groups = "drop") |>
    arrange(label) |>
    write_csv(str_glue("results/summarised_lab_values_overall_{disease}_drg.csv"))

  summarised_by_gender <- labvalues |>
    group_by(disease, gender, label, cluster, valueuom) |>
    summarise(mean_value = mean(valuenum, na.rm = TRUE),
              sd_value = sd(valuenum, na.rm = TRUE),
              min_value = min(valuenum, na.rm = TRUE),
              max_value = max(valuenum, na.rm = TRUE),
              median_value = median(valuenum, na.rm = TRUE),
              count_value = n(),
              .groups = "drop") |>
    arrange(gender, label) |>
    write_csv(str_glue("results/summarised_lab_values_{disease}_drg.csv"))
}

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

summarise_values(diseases[1])
summarise_values(diseases[2])
summarise_values(diseases[3])
summarise_values(diseases[4])
summarise_values(diseases[5])
summarise_values(diseases[6])
summarise_values(diseases[7])
