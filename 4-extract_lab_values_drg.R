# Calculate Summary Statistics

library(tidyverse)

extract_lab_values <- function(disease, labevents_data, admission_data, labtest_list, drg_clusters) {

  in_dir <- file.path("data", disease)

  out_dir <- in_dir

  patient_list <-
    read_csv(file.path(in_dir, str_glue("matched_patients_{disease}.csv"))) |>
    inner_join(admission_data, by = "subject_id") |>
    inner_join(drg_clusters, by = "drg_code")

  filtered_labevents <- labevents_data |>
    inner_join(patient_list, by = "subject_id") |>
    inner_join(labtest_list, by = "itemid") |>
    mutate(label = str_c(label, fluid, sep = " | ")) |>
    select(subject_id,
           gender,
           disease,
           itemid,
           label,
           value,
           valuenum,
           valueuom,
           charttime,
           cluster) |>
    filter(!is.na(value) & !is.na(valuenum)) |>
    write_csv(file = file.path(
      out_dir,
      str_glue("matched_labvalues_selected_{disease}_drg.csv")
    ))
}

labevents_data <- read_csv("data/labtest_information.csv")

labtest_list <- read_csv("data/lab_test_ids.csv")

admission_data <- read_csv("data/admission_diagnoses_drg_data.csv")

drg_clusters <- list.files("ancillary/drg_codes/", full.names = TRUE) |>
  set_names(~ basename(.x)) |>
  map(~ read_lines(.x)) |>
  enframe() |>
  unnest_longer(value) |>
  mutate(cluster = str_extract(name, "DRG-(.*)-Cluster", 1)) |>
  filter(cluster != "Excluded") |>
  select(-name, cluster, drg_code = value)

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

diseases |>
  walk(~ extract_lab_values(.x, labevents_data, admission_data, labtest_list, drg_clusters))
