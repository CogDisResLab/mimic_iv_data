# Generate ICD 9/10 code lists for our disorders

library(tidyverse)

source("m-icd9_codes.R")

database <- read_csv("ancillary/icd10toicd9gem.csv") |>
  filter(!str_starts(icd10cm, "0"))

combine_codes <-
  function(icd9_codes,
           database = NULL,
           disease = NA) {
    icd10_codes <- database |>
      filter(icd9cm %in% icd9_codes) |>
      pull(icd10cm) |>
      unique()

    outfile <- file.path("ancillary", "icd_codes", disease)


    all_codes <- c(icd9_codes, icd10_codes) |>
      write_lines(outfile)

  }

icd9_codes <-
  list(anxiety = anxiety,
       bipolar = bipolar,
       depression = depression,
       regional_enteritis = regional_enteritis,
       schizophrenia = schizophrenia,
       suicide = suicide)

icd9_codes |>
  iwalk(~ combine_codes(.x, database, .y))

