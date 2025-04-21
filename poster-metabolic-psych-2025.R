# Figures for the generation of the Metabolic Psychiatry small meeting

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggpubr)
  library(lubridate)
})

data <- read_csv("results/compared_lab_values_schizophrenia_drg.csv") |>
  filter(!is.na(label))


volcanoplot_data <- data |>
  filter(
    cluster %in% c("Psychiatry", "Trauma"),
    !is.na(label), !is.na(effect_size)
  ) |>
  select(gender, cluster, label, effect_size, p.value) |>
  mutate(
    logpval = -log10(p.value + 1e-6), significant = p.value < 0.05,
    gender = if_else(gender == "F", "Female", "Male"),
    short_label = case_when(
      str_detect(label, "Alanine") ~ "sALT",
      str_detect(label, "Albumin") ~ "sALB",
      str_detect(label, "Alkaline") ~ "sALP",
      str_detect(label, "Anion") ~ "AGAP",
      str_detect(label, "Asparate") ~ "sAST",
      str_detect(label, "Bicarbonate") ~ "HCO[3]^'-'",
      str_detect(label, "Bilirubin") ~ "SBR",
      str_detect(label, "Calcium") ~ "Ca^{2*'+'}",
      str_detect(label, "Chloride \\| Blood") ~ "sCl^'-'",
      str_detect(label, "Chloride \\| Urine") ~ "uCl^'-'",
      str_detect(label, "Creatinine \\| Blood") ~ "sCr",
      str_detect(label, "Creatinine \\| Urine") ~ "uCr",
      str_detect(label, "Glucose \\| Blood") ~ "sGLU",
      str_detect(label, "Glucose \\| Urine") ~ "uGLU",
      str_detect(label, "Ketone") ~ "uK",
      str_detect(label, "Magnesium") ~ "Mg^{2*'+'}",
      str_detect(label, "Phosphate") ~ "PO[4]^'-'",
      str_detect(label, "Potassium \\| Blood") ~ "sK^'+'",
      str_detect(label, "Potassium \\| Urine") ~ "uK^'+'",
      str_detect(label, "Sodium \\| Blood") ~ "sNa^'+'",
      str_detect(label, "Sodium \\| Urine") ~ "uNa^'+'",
      str_detect(label, "Urea Nitrogen \\| Blood") ~ "BUN",
      str_detect(label, "Urea Nitrogen \\| Urine") ~ "UUN",
      str_detect(label, "pH \\| Blood") ~ "spH",
      str_detect(label, "pH \\| Urine") ~ "upH"
    ),
    fluid = str_extract(label, "\\|(.*)\\|", 1) |> str_squish() |> as_factor()
  )

volcanoplot <- ggplot(
  volcanoplot_data,
  aes(
    x = effect_size, y = logpval, color = fluid, shape = fluid,
    label = short_label
  )
) +
  geom_point(position = "jitter") +
  geom_label(parse = TRUE, ) +
  geom_hline(yintercept = -log10(0.05), color = "grey50", lty = "dashed") +
  scale_color_manual(
    name = "", breaks = c("Blood", "Urine"),
    values = c("#005AB5", "#DC3220")
  ) +
  scale_shape_manual(
    name = "", breaks = c("Blood", "Urine"),
    values = c(15, 19)
  ) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 15, 1), oob = scales::squish) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 15, 1), oob = scales::squish) +
  labs(x = "Effect Size", y = expression(-log[10](p))) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 24)) +
  guides(label = "none") +
  facet_grid(gender ~ cluster)

volcanoplot <- ggscatter(volcanoplot_data,
  x = "effect_size",
  y = "logpval",
  color = "fluid",
  shape = "fluid",
  label = "short_label",
  repel = TRUE,
  parse = TRUE,
  facet.by = c("gender", "cluster"),
  palette = c("#005AB5", "#DC3220"),
  xlab = "Effect Size (Cohen's d)"
) +
  geom_hline(yintercept = -log10(0.05), color = "grey50", lty = "dashed") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 15, 1), oob = scales::squish) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 15, 1), oob = scales::squish) +
  theme(legend.position = "bottom", text = element_text(size = 24)) +
  ylab(expression(-log[10](p))) +
  labs(color = "") +
  guides(shape = "none")


ggsave("significant_schizophrenia_volcanoplot.svg",
  plot = volcanoplot, bg = "white",
  width = 10, height = 10, unit = "in", path = "figures"
)

ggsave("significant_schizophrenia_volcanoplot.png",
  plot = volcanoplot, bg = "white",
  width = 10, height = 10, unit = "in", path = "figures"
)

piechart_data <- data |>
  filter(cluster %in% c("Psychiatry")) |>
  count(
    gender, cluster,
    significant
  ) |>
  select(-cluster) |>
  group_by(gender) |>
  mutate(
    percentage = round((n / sum(n)) * 100, 2),
    clean_name = case_when(
      significant ~ "Significant",
      !significant ~ "Not Significant",
      is.na(significant) ~ "Insufficient Data"
    ),
    label = str_glue("{clean_name}\nn={n} ({percentage}%)"),
    gender = if_else(gender == "M", "Male Lab Tests", "Female Lab Tests")
  ) |>
  arrange(gender, desc(clean_name)) |>
  mutate(
    fraction = n / sum(n),
    # Calculate start and end angles
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0),
    # Find the middle angle for each segment
    pos = (ymin + ymax) / 2,
    # Calculate the label position
    label_x = 0, # center of pie
    label_y = pos * sum(percentage) # scaled position
  ) |>
  ungroup() |>
  select(-significant)

piechart <- ggplot(piechart_data, aes(x = "", y = percentage, fill = clean_name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  # facet_grid(row = vars(gender)) +
  facet_wrap(~gender, ncol = 1) +
  geom_text(
    aes(y = label_y, label = label),
    color = "white", size = 5
  ) +
  scale_fill_manual(values = c("#332288", "#117733", "#44AA99")) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 24, face = "bold")
  )

ggsave("significant_schizophrenia_piechart.svg",
  plot = piechart, bg = "white",
  width = 5, height = 12, unit = "in", path = "figures"
)

ggsave("significant_schizophrenia_piechart.png",
  plot = piechart, bg = "white",
  width = 5, height = 12, unit = "in", path = "figures"
)


distribution_tests <- data |>
  filter(
    significant,
    cluster %in% c("Psychiatry", "Trauma"),
    !is.na(label), !is.na(effect_size)
  ) |>
  group_by(gender, cluster) |>
  slice_max(n = 5, order_by = effect_size) |>
  pull(label) |>
  unique()

selected_tests <- distribution_tests[c(1, 3)]

distribution_data <- read_csv("data/schizophrenia/matched_labvalues_selected_schizophrenia_drg.csv") |>
  unique() |>
  filter(cluster %in% c("Psychiatry", "Trauma"), label %in% selected_tests) |>
  mutate(event_timepoint = lubridate::as_datetime(charttime)) |>
  group_by(subject_id, label, cluster) |>
  filter(event_timepoint == max(event_timepoint)) |>
  select(subject_id, gender, disease, label, cluster, value = valuenum) |>
  mutate(disease = if_else(disease == 0, "Control", "Case"))

violin_glucose <- ggviolin(
  {
    distribution_data |> filter(label == selected_tests[1])
  },
  x = "disease",
  y = "value",
  fill = "disease",
  panel.labs = list(gender = c("Female", "Male")),
  facet.by = c("gender", "cluster"),
  add = c("median_q1q3", "boxplot"),
  add.params = list(fill = "white"),
  xlab = "Disease Status",
  ylab = "Blood Glucose Levels (mg/dl)"
) +
  scale_fill_manual(
    name = "", breaks = c("Control", "Case"),
    labels = c("Control", "Case"),
    values = c("#005AB5", "#DC3220")
  ) +
  stat_compare_means(
    method = "t.test", label.x = 1.5, comparisons = list(c("Case", "Control")),
    label = "p.signif", hide.ns = TRUE, size = 10
  ) +
  theme(text = element_text(size = 28))

ggsave("violinplot_blood_glucose_schizophrenia.svg",
  plot = violin_glucose, bg = "white",
  width = 15, height = 15, unit = "in", path = "figures"
)

ggsave("violinplot_blood_glucose_schizophrenia.png",
  plot = violin_glucose, bg = "white",
  width = 15, height = 15, unit = "in", path = "figures"
)

violin_ph <- ggviolin(
  {
    distribution_data |> filter(label == selected_tests[2])
  },
  x = "disease",
  y = "value",
  fill = "disease",
  panel.labs = list(gender = c("Female", "Male")),
  facet.by = c("gender", "cluster"),
  add = c("median_q1q3", "boxplot"),
  add.params = list(fill = "white"),
  xlab = "Disease Status",
  ylab = "Urine pH"
) +
  scale_fill_manual(
    name = "", breaks = c("Control", "Case"),
    labels = c("Control", "Case"),
    values = c("#005AB5", "#DC3220")
  ) +
  stat_compare_means(
    method = "t.test", label.x = 1.5, comparisons = list(c("Case", "Control")),
    label = "p.signif", hide.ns = TRUE, size = 10
  ) +
  theme(text = element_text(size = 28))

ggsave("violinplot_urine_ph_schizophrenia.svg",
  plot = violin_ph, bg = "white",
  width = 15, height = 15, unit = "in", path = "figures"
)

ggsave("violinplot_urine_ph_schizophrenia.png",
  plot = violin_ph, bg = "white",
  width = 15, height = 15, unit = "in", path = "figures"
)

ranked <- data |>
  filter(
    cluster %in% c("Psychiatry"),
    !is.na(label), !is.na(effect_size),
    significant
  ) |>
  select(gender, cluster, label, effect_size, p.value) |>
  group_by(gender) |>
  arrange(desc(effect_size)) |>
  mutate(rank = row_number()) |>
  ungroup()

top10 <- ranked |>
  filter(rank <= 10)

common <- top10 |>
  count(label) |>
  filter(n == 2)

top10_data <- ranked |>
  filter(label %in% top10$label) |>
  select(gender, label, rank, effect_size, p.value)
