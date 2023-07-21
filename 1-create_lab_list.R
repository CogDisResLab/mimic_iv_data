# Process the list of lab values

library(tidyverse)
library(readxl)

selected_lab_items <-
  read_excel("raw/Disease Proportions Extracted for Lab Values.xlsx", "Final") |>
  pull(1)

detect_fluids <- function(label) {
  fluids <-
    c(
      ", Blood$",
      ", Other Body Fluid$",
      ", Ascites$",
      ", Joint Fluid$",
      ", Pleural$",
      ", Stool$",
      ", Urine$",
      ", Bone Marrow$",
      ", Cerebrospinal Fluid$",
      ", Fluid$",
      ", I$",
      ", Q$",
      ", Body Fluid$",
      ", Whole Blood$"
    )

  fluids |>
    map_lgl( ~ str_detect(label, .x)) |>
    any()
}

remove_fluid <- function(label) {
  str_remove_all(label, ",.+$")
}

v_detect_fluids <- Vectorize(detect_fluids)

v_remove_fluids <- Vectorize(remove_fluid)

lab_items_data <- read_csv("raw/data/hosp/d_labitems.csv") |>
  mutate(clean_label = if_else(v_detect_fluids(label), v_remove_fluids(label), label)) |>
  filter(clean_label %in% selected_lab_items, fluid %in% c("Blood", "Urine")) |>
  select(-category, -label, itemid, label = clean_label, fluid) |>
  write_csv("data/lab_test_ids.csv")
