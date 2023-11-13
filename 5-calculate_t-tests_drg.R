# Calculate t-tests for each lab test and disease

library(tidyverse)
library(broom)
library(lsr)

count_patients <- function(data, disease_status) {
  n <- data |> count(disease) |> filter(disease == disease_status) |> pull(n)

  if (length(n) == 0) {
    0
  } else {
    n
  }
}

calculate_statistics <- function(disease) {
  in_dir <- file.path("data", disease)

  out_dir <- in_dir

  labvalues_gendered <-
    read_csv(file.path(
      in_dir,
      str_glue("matched_labvalues_selected_{disease}_drg.csv")
    ), show_col_types = FALSE) |>
    select(disease, gender, label, cluster, value = valuenum) |>
    nest(.by = c(gender, label, cluster)) |>
    mutate(
      t_test = map(data, safely(
        ~ t.test(formula = value ~ disease, data = .x)
      )),
      cohens_d = map(data, safely(
        ~ cohensD(formula = value ~ disease, data = .x)
      )),
      successful = map(t_test, ~ pluck(.x, "result")),
      effect_size = map(cohens_d, ~ pluck(.x, "result")),
      effect_size = map_dbl(effect_size, ~ ifelse(is_null(.x), NA, .x)),
      glanced = map(successful, ~ glance(.x)),
      ncases = map_int(
        data,
        ~ count_patients(.x, 1)
      ),
      nmatched = map_int(
        data,
        ~ count_patients(.x, 0)
      ),
      ntotal = map_int(
        data,
        ~ count_patients(.x, 1) + count_patients(.x, 0)
      ),
      prop_cases = round(ncases / ntotal, 4),
      prop_matched = round(nmatched / ntotal, 4)
    ) |>
    select(-data, -t_test, -cohens_d, -successful) |>
    unnest_wider(glanced) |>
    mutate(across(where(is.numeric), ~ round(.x, 5)),
           significant = p.value <= 0.05) |>
    arrange(gender, label) |>
    write_csv(str_glue("results/compared_lab_values_{disease}_drg.csv"))


  labvalues_overall <-
    read_csv(file.path(
      in_dir,
      str_glue("matched_labvalues_selected_{disease}_drg.csv")
    ), show_col_types = FALSE) |>
    select(disease, label, cluster, value = valuenum) |>
    nest(.by = c(label, cluster)) |>
    mutate(
      t_test = map(data, safely(
        ~ t.test(formula = value ~ disease, data = .x)
      )),
      cohens_d = map(data, safely(
        ~ cohensD(formula = value ~ disease, data = .x)
      )),
      successful = map(t_test, ~ pluck(.x, "result")),
      effect_size = map(cohens_d, ~ pluck(.x, "result")),
      effect_size = map_dbl(effect_size, ~ ifelse(is_null(.x), NA, .x)),
      glanced = map(successful, ~ glance(.x)),
      ncases = map_dbl(
        data,
        ~ count_patients(.x, 1)
      ),
      nmatched = map_dbl(
        data,
        ~ count_patients(.x, 0)
      ),
      ntotal = map_int(
        data,
        ~ count_patients(.x, 1) + count_patients(.x, 0)
      ),
      prop_cases = round(ncases / ntotal, 4),
      prop_matched = round(nmatched / ntotal, 4)
    ) |>
    select(-data, -t_test, -cohens_d, -successful) |>
    unnest_wider(glanced) |>
    mutate(across(where(is.numeric), ~ round(.x, 5)),
           significant = p.value <= 0.05) |>
    arrange(label) |>
    write_csv(str_glue("results/compared_lab_values_overall_{disease}_drg.csv"))
}

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

calculate_statistics(diseases[1])
calculate_statistics(diseases[2])
calculate_statistics(diseases[3])
calculate_statistics(diseases[4])
calculate_statistics(diseases[5])
calculate_statistics(diseases[6])
calculate_statistics(diseases[7])
