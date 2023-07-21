# Filter and parse MIMIC-IV data for easier downstream analysis

library(tidyverse)

# Clean and compact the diagnoses data
diagnoses_data <-
  read_csv("raw/data/hosp/diagnoses_icd.csv.gz",
           col_types = cols(.default = col_character())) |>
  select(subject_id, hadm_id, icd_code)

# Clean and compact the DRG Data
drg_data <-
  read_csv("raw/data/hosp/drgcodes.csv.gz",
           col_types = cols(.default = col_character())) |>
  filter(drg_type == "HCFA") |>
  select(subject_id, hadm_id, drg_code)

admissions_col_spec <- cols(
  admittime = col_datetime(format = ""),
  dischtime = col_datetime(format = ""),
  deathtime = col_datetime(format = ""),
  edregtime = col_datetime(format = ""),
  edouttime = col_datetime(format = ""),
  .default = col_character()
)

# Combine the DRG and Diagnosis Data into admission data
admissions_data <- read_csv("raw/data/hosp/admissions.csv.gz", col_types = admissions_col_spec) |>
  filter(admittime == max(admittime), .by = subject_id) |>
  select(subject_id, hadm_id) |>
  left_join(drg_data, by = c("subject_id", "hadm_id")) |>
  left_join(diagnoses_data) |>
  filter(!(is.na(drg_code) | is.na(icd_code))) |>
  write_csv("data/admission_diagnoses_drg_data.csv")

patients_data <- read_csv("raw/data/hosp/patients.csv.gz") |>
  filter(subject_id %in% admissions_data$subject_id) |>
  write_csv("data/patient_information.csv")

labs_data <- read_csv("raw/data/hosp/labevents.csv.gz") |>
  select(subject_id, hadm_id, itemid, value, valuenum, valueuom, charttime) |>
  filter(subject_id %in% admissions_data$subject_id,
         hadm_id %in% admissions_data$hadm_id) |>
  filter(charttime == max(charttime), .by = c(subject_id, itemid)) |>
  write_csv("data/labtest_information.csv")
