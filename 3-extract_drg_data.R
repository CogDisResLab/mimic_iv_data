# Extract the DRGs for the matched patients

library(tidyverse)

extract_drgs <- function(disease) {
  in_dir <- file.path("data", disease)

  out_dir <- in_dir

  drg_data <- read_csv("raw/data/hosp/drgcodes.csv.gz") |>
    select(-drg_severity, -drg_mortality) |>
    filter(drg_type == "HCFA")

  patient_list <-
    read_csv(file.path(in_dir, str_glue("matched_patients_{disease}.csv"))) |>
    inner_join(drg_data) |>
    count(disease, gender, drg_code, description) |>
    write_csv(file.path(out_dir, str_glue("matched_patients_{disease}_drg.csv")))
}

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

diseases |>
  walk(~ extract_drgs(.x))
