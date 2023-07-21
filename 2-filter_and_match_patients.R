# List and Match Patients

library(tidyverse)
library(MatchIt)

excluded_drgs <- read_lines("ancillary/drg_codes/DRG-Excluded-Cluster")

process_disease_data <- function(disease,
                                 patients_data,
                                 diagnoses_data) {

  # Load the ICD Codes
  icd_codes <- file.path("ancillary", "icd_codes", disease) |>
    readLines()

  # Create the directory to store disease data in
  out_dir <- file.path("data", disease)
  dir.create(
    out_dir,
    showWarnings = FALSE,
    recursive = TRUE,
    mode = "0755"
  )

  # Generate a list of patients with the diagnosis
  filtered_patients <- diagnoses_data |>
    filter(icd_code %in% icd_codes) |>
    select(subject_id) |>
    inner_join(patients_data, by = "subject_id") |>
    unique() |>
    select(subject_id, gender, age = anchor_age) |>
    mutate(disease = 1)

  matchable_patients <- patients_data |>
    inner_join(diagnoses_data, by = c("subject_id")) |>
    group_by(subject_id) |>
    mutate(keep = if_else(any(drg_code %in% excluded_drgs), FALSE, TRUE)) |>
    select(subject_id, gender, age = anchor_age, keep) |>
    filter(keep) |>
    select(-keep) |>
    unique() |>
    mutate(disease = 0)

  all_data <- bind_rows(filtered_patients, matchable_patients)

  matched <- matchit(disease ~ age + gender, exact = ~ age + gender, data = all_data)
  saveRDS(matched, str_glue("datastore/{disease}_matching.RDS"))

  matched_subject_list <- matched |>
    match.data() |>
    select(subject_id, age, gender, disease) |>
    write_csv(file.path(out_dir, str_glue("matched_patients_{disease}.csv")))

}

patients_data <- read_csv("data/patient_information.csv")
diagnoses_data <- read_csv("data/admission_diagnoses_drg_data.csv")

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

diseases |>
  walk(~ process_disease_data(.x, patients_data, diagnoses_data))
