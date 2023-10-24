# Calculate proportions of each group

library(tidyverse)

calculate_proportion_data <-
  function(disease,
           labevent_data) {
    in_dir <- file.path("data", disease)
    matched_file <-
      file.path(in_dir, str_glue("matched_patients_{disease}.csv"))
    out_dir <- in_dir

    # Load the Lab Item Code to Name mapping
    lab_items <- read_csv("data/lab_test_ids.csv") |>
      mutate(clean_labels = str_c(label, fluid, valueuom, sep = " | "))

    matched_data <- read_csv(matched_file, show_col_types = FALSE)

    lab_test_data <- labevent_data |>
      inner_join(matched_data, by = "subject_id") |>
      filter(!is.na(value)) |>
      select(subject_id, itemid, gender, disease) |>
      unique() |>
      inner_join(lab_items, by = "itemid") |>
      select(subject_id, itemid, gender, disease, label = clean_labels)

    num_patients <- matched_data |>
      filter(disease == 1) |>
      nrow()
    num_patients_male <- matched_data |>
      filter(gender == "M", disease == 1) |>
      nrow()
    num_patients_female <- matched_data |>
      filter(gender == "F", disease == 1) |>
      nrow()

    num_controls <- matched_data |>
      filter(disease == 0) |>
      nrow()
    num_controls_male <- matched_data |>
      filter(gender == "M", disease == 0) |>
      nrow()
    num_controls_female <- matched_data |>
      filter(gender == "F", disease == 0) |>
      nrow()

    proportion_data <- lab_test_data |>
      summarise(count = n(),
                .by = c("disease", "itemid", "gender")) |>
      pivot_wider(names_from = gender, values_from = count) |>
      rename(count_male_with_lab = M,
             count_female_with_lab = F) |>
      mutate(
        count_all_with_lab = count_male_with_lab + count_female_with_lab,
        prop_male_with_lab = if_else(
          disease == 0,
          count_male_with_lab / num_controls_male,
          count_male_with_lab / num_patients_male
        ),
        prop_female_with_lab = if_else(
          disease == 0,
          count_female_with_lab / num_controls_female,
          count_female_with_lab / num_patients_female
        ),
        prop_all_with_lab = if_else(
          disease == 0,
          count_all_with_lab / num_controls,
          count_all_with_lab /
            num_patients
        ),
        across(starts_with("prop"), ~ round(.x, 4))
      ) |>
      inner_join(lab_items, by = "itemid") |>
      select(itemid,
             disease,
             -label,
             -fluid,
             label = clean_labels,
             starts_with("count"),
             starts_with("prop")) |>
      mutate(class = if_else(disease == 0, "CTRL", "CASE")) |>
      write_csv(file = file.path(out_dir,
                                 str_glue("labtest_proportion_{disease}.csv")))

    # Collapse proportions by test name
    proportion_data_collapsed <- lab_test_data |>
      select(-itemid) |>
      unique() |>
      summarise(count = n(),
                .by = c("disease", "label", "gender")) |>
      pivot_wider(names_from = gender, values_from = count) |>
      rename(count_male_with_lab = M,
             count_female_with_lab = F) |>
      mutate(
        count_all_with_lab = count_male_with_lab + count_female_with_lab,
        prop_male_with_lab = if_else(
          disease == 0,
          count_male_with_lab / num_controls_male,
          count_male_with_lab / num_patients_male
        ),
        prop_female_with_lab = if_else(
          disease == 0,
          count_female_with_lab / num_controls_female,
          count_female_with_lab / num_patients_female
        ),
        prop_all = if_else(
          disease == 0,
          count_all_with_lab / num_controls,
          count_all_with_lab /
            num_patients
        ),
        across(starts_with("prop"), ~ round(.x, 4)),
        class = if_else(disease == 0, "CTRL", "CASE")
      ) |>
      write_csv(file = file.path(
        out_dir,
        str_glue("labtest_proportion_collapsed_{disease}.csv")
      ))

  }

labevent_data <- read_csv("data/labtest_information.csv")

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))

calculate_proportion_data(diseases[1], labevent_data)
calculate_proportion_data(diseases[2], labevent_data)
calculate_proportion_data(diseases[3], labevent_data)
calculate_proportion_data(diseases[4], labevent_data)
calculate_proportion_data(diseases[5], labevent_data)
calculate_proportion_data(diseases[6], labevent_data)
calculate_proportion_data(diseases[7], labevent_data)
