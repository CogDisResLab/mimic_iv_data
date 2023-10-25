# Calculate Summary Statistics

library(tidyverse)

extract_lab_values <- function(disease, labevents_data, labtest_list) {

  in_dir <- file.path("data", disease)

  out_dir <- in_dir

  patient_list <-
    read_csv(file.path(in_dir, str_glue("matched_patients_{disease}.csv")))

  filtered_labevents <- labevents_data |>
    select(-valueuom) |>
    inner_join(patient_list, by = "subject_id") |>
    inner_join(labtest_list, by = "itemid") |>
    mutate(label = str_c(label, fluid, valueuom, sep = " | ")) |>
    select(subject_id,
           gender,
           disease,
           itemid,
           label,
           value,
           valuenum,
           valueuom,
           charttime) |>
    filter(!is.na(value) & !is.na(valuenum)) |>
    write_csv(file = file.path(
      out_dir,
      str_glue("matched_labvalues_selected_{disease}.csv")
    ))
}

labevents_data <- read_csv("data/labtest_information.csv")
labtest_list <- read_csv("data/lab_test_ids.csv")

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))


extract_lab_values(diseases[1], labevents_data, labtest_list)
extract_lab_values(diseases[2], labevents_data, labtest_list)
extract_lab_values(diseases[3], labevents_data, labtest_list)
extract_lab_values(diseases[4], labevents_data, labtest_list)
extract_lab_values(diseases[5], labevents_data, labtest_list)
extract_lab_values(diseases[6], labevents_data, labtest_list)
extract_lab_values(diseases[7], labevents_data, labtest_list)
