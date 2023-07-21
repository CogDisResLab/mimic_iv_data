# Extract the DRG Clusters

library(tidyverse)
library(readxl)

datafile <- "raw/DRG_Mappings_Smita.xlsx"
sheet <- "Matched_Final_Bins"

dir.create("ancillary/drg_codes", showWarnings = FALSE, recursive = TRUE)

dataset <- read_excel(datafile, sheet) |>
  select(-c(1,2,3)) |>
  select(contains("drg")) |>
  mutate(across(contains("drg"), ~ str_pad(.x, 3, pad = "0"))) |>
  pivot_longer(everything(), names_to = "cluster", values_to = "drg") |>
  filter(!is.na(drg)) |>
  mutate(cluster = case_when(
    cluster == "cardio_drg" ~ "Cardiology",
    cluster == "ent_drg" ~ "ENT",
    cluster == "exclusion_drg" ~ "Excluded",
    cluster == "gi_drg" ~ "Gastrointestinal",
    cluster == "infectious_drg" ~ "Infections_Immune",
    cluster == "malignancy_drg" ~ "Malignancy",
    cluster == "msk_drg" ~ "MSK_Skin",
    cluster == "neuro_drg" ~ "Neurology",
    cluster == "procedures_drg" ~ "Procedures",
    cluster == "psych_drg" ~ "Psychiatry",
    cluster == "pulm_drg" ~ "Pulmonology",
    cluster == "repro_drg" ~ "Reprodutive",
    cluster == "systemic_drg" ~ "Systemic",
    cluster == "trauma_drg" ~ "Trauma",
  )) |>
  nest(.by = cluster) |>
  deframe() |>
  map(~ pull(.x, 1)) |>
  imap(~ write_file(
    str_c(.x, collapse = "\n"),
    str_glue("ancillary/drg_codes/DRG-{.y}-Cluster")
  ))

