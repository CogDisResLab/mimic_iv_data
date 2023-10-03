# Calculate proportions of each group

library(tidyverse)

get_denominator_all <- function(df, diagnosis, column) {
  df |>
    filter(disease == diagnosis) |>
    pull(column)

}

v_get_denominator_all <- Vectorize(get_denominator_all, vectorize.args = "column")

get_denominator_gender <-
  function(df, diagnosis, gender_val, column) {
    df |>
      filter(disease == diagnosis, gender == gender_val) |>
      pull(column)

  }

v_get_denominator_gender <-
  Vectorize(get_denominator_gender, vectorize.args = "column")

calculate_proportion_data <-
  function(disease,
           labevent_data,
           admission_data,
           labtest_list,
           drg_clusters) {
    in_dir <- file.path("data", disease)

    matched_file <-
      file.path(in_dir, str_glue("matched_patients_{disease}.csv"))
    out_dir <- in_dir

    # Load the Lab Item Code to Name mapping
    lab_items <- read_csv("data/lab_test_ids.csv") |>
      mutate(clean_labels = str_c(label, fluid, sep = " | "))

    # Create the subject to admission to drg_cluster mapping

    admission_clusters <- admission_data |>
      select(-hadm_id, -icd_code) |>
      unique() |>
      inner_join(drg_clusters, by = "drg_code", relationship = "many-to-many")

    matched_data <-
      read_csv(matched_file, show_col_types = FALSE) |>
      inner_join(admission_clusters, by = "subject_id")

    lab_test_data <- labevent_data |>
      inner_join(matched_data, by = "subject_id", relationship = "many-to-many") |>
      filter(!is.na(value)) |>
      select(subject_id, itemid, gender, disease, cluster) |>
      unique() |>
      inner_join(lab_items, by = "itemid") |>
      select(subject_id, itemid, gender, disease, cluster, label = clean_labels)

    num_patients_by_cluster <- matched_data |>
      count(disease, cluster) |>
      pivot_wider(names_from = cluster, values_from = n)

    num_patients_by_gender_cluster <- matched_data |>
      count(disease, gender, cluster) |>
      pivot_wider(names_from = cluster, values_from = n)

    proportion_data <- lab_test_data |>
      summarise(count = n(),
                .by = c("disease", "itemid", "gender", "cluster")) |>
      pivot_wider(names_from = gender, values_from = count) |>
      rename(count_male_with_lab = M,
             count_female_with_lab = F) |>
      mutate(
        count_all_with_lab = count_male_with_lab + count_female_with_lab,
        prop_male_with_lab = if_else(
          disease == 0,
          count_male_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 0, "M", cluster),
          count_male_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 1, "M", cluster)
        ),
        prop_female_with_lab = if_else(
          disease == 0,
          count_female_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 0, "F", cluster),
          count_female_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 1, "F", cluster)
        ),
        prop_all_with_lab = if_else(
          disease == 0,
          count_all_with_lab / v_get_denominator_all(num_patients_by_cluster, 0, cluster),
          count_all_with_lab / v_get_denominator_all(num_patients_by_cluster, 1, cluster)
        ),
        across(starts_with("prop"), ~ round(.x, 4))
      ) |>
      inner_join(lab_items, by = "itemid") |>
      select(itemid,
             -label,
             -fluid,
             disease,
             label = clean_labels,
             starts_with("count"),
             starts_with("prop")) |>
      mutate(class = if_else(disease == 0, "CTRL", "CASE")) |>
      write_csv(file = file.path(out_dir,
                                 str_glue(
                                   "labtest_proportion_{disease}_drg.csv"
                                 )))

    # Collapse proportions by test name
    proportion_data_collapsed <- lab_test_data |>
      select(-itemid) |>
      unique() |>
      summarise(count = n(),
                .by = c("disease", "label", "gender", cluster)) |>
      pivot_wider(names_from = gender, values_from = count) |>
      rename(count_male_with_lab = M,
             count_female_with_lab = F) |>
      mutate(
        count_all_with_lab = count_male_with_lab + count_female_with_lab,
        prop_male_with_lab = if_else(
          disease == 0,
          count_male_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 0, "M", cluster),
          count_male_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 1, "M", cluster)
        ),
        prop_female_with_lab = if_else(
          disease == 0,
          count_female_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 0, "F", cluster),
          count_female_with_lab / v_get_denominator_gender(num_patients_by_gender_cluster, 1, "F", cluster)
        ),
        prop_all_with_lab = if_else(
          disease == 0,
          count_all_with_lab / v_get_denominator_all(num_patients_by_cluster, 0, cluster),
          count_all_with_lab / v_get_denominator_all(num_patients_by_cluster, 1, cluster)
        ),
        across(starts_with("prop"), ~ round(.x, 4)),
        class = if_else(disease == 0, "CTRL", "CASE")
      ) |>
      write_csv(file = file.path(
        out_dir,
        str_glue("labtest_proportion_collapsed_{disease}_drg.csv")
      ))

  }

labevent_data <- read_csv("data/labtest_information.csv")

labtest_list <- read_csv("data/lab_test_ids.csv")

admission_data <- read_csv("data/admission_diagnoses_drg_data.csv")

drg_clusters <-
  list.files("ancillary/drg_codes/", full.names = TRUE) |>
  set_names( ~ basename(.x)) |>
  map( ~ read_lines(.x)) |>
  enframe() |>
  unnest_longer(value) |>
  mutate(cluster = str_extract(name, "DRG-(.*)-Cluster", 1)) |>
  filter(cluster != "Excluded") |>
  select(-name, cluster, drg_code = value)

diseases <- list.files("ancillary/icd_codes/") |>
  keep(~ str_detect(.x, c("diabetes"), negate = TRUE))


calculate_proportion_data(diseases[1], labevent_data, admission_data, labtest_list, drg_clusters)
calculate_proportion_data(diseases[2], labevent_data, admission_data, labtest_list, drg_clusters)
calculate_proportion_data(diseases[3], labevent_data, admission_data, labtest_list, drg_clusters)
calculate_proportion_data(diseases[4], labevent_data, admission_data, labtest_list, drg_clusters)
calculate_proportion_data(diseases[5], labevent_data, admission_data, labtest_list, drg_clusters)
calculate_proportion_data(diseases[6], labevent_data, admission_data, labtest_list, drg_clusters)
calculate_proportion_data(diseases[7], labevent_data, admission_data, labtest_list, drg_clusters)
