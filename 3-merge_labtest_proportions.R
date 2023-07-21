# Merge the lab test proportions by disease

library(tidyverse)

files <-
  list.files("data",
             "collapsed",
             recursive = TRUE,
             full.names = TRUE)

merged_data <- files |>
  set_names( ~ basename(.x) |> str_extract("collapsed_(\\w+).csv", 1)) |>
  map( ~ read_csv(.x,
                  show_col_types = FALSE)) |>
  map( ~ select(.x, c(-disease))) |>
  imap( ~ rename_with(.x, \(x) str_c(x, .y, sep = "_"), .cols = c(
    starts_with("prop"), starts_with("count")
  ))) |>
  reduce(inner_join, by = c("label", "class")) |>
  select(-contains("diabetes")) |>
  unique() |>
  relocate(class, .after = label) |>
  write_csv("results/labtest_props_across_diseases.csv")
