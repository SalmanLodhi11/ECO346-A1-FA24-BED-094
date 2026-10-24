
# ECO346 Assignment 1 - Question 3
# Variant 6: Balochistan rural
# Weighted literacy rates by age and sex

library(readr)
library(dplyr)
library(ggplot2)

# 1. Read the datasets
person <- read_csv("data/pslm_person_subset.csv")
household <- read_csv("data/pslm_hh_subset.csv")

# 2. Attach province, area and household weight
data <- person %>%
  left_join(
    household %>%
      select(hhid, prov_code, urban_rural, hhweight),
    by = "hhid"
  )

# 3. Keep Variant 6: Balochistan rural
data <- data %>%
  filter(
    prov_code == 4,
    urban_rural == 1
  )

# 4. Keep the literacy sample
literacy_data <- data %>%
  filter(
    age >= 10,
    age <= 64,
    !is.na(literate),
    !is.na(sex),
    !is.na(hhweight),
    hhweight > 0,
    sex %in% c(1, 2)
  )

# 5. Create five-year age bands
literacy_data <- literacy_data %>%
  mutate(
    age_band = cut(
      age,
      breaks = seq(10, 65, by = 5),
      right = FALSE,
      labels = c(
        "10-14", "15-19", "20-24", "25-29",
        "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64"
      )
    ),
    age_midpoint = as.numeric(age_band) * 5 + 7
  )

# 6. Check the prepared sample
cat("Number of people in literacy sample:",
    nrow(literacy_data), "\n")

cat("Sex codes:\n")
print(table(literacy_data$sex))

cat("Age bands:\n")
print(table(literacy_data$age_band))


# 7. Calculate weighted literacy rates by age band and sex

literacy_table <- literacy_data %>%
  group_by(age_band, age_midpoint, sex) %>%
  summarise(
    n_persons = n(),
    weighted_literacy_rate =
      sum(literate * hhweight) / sum(hhweight) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    sex_label = if_else(sex == 2, "Women", "Men"),
    thin_cell = n_persons < 30
  ) %>%
  arrange(age_midpoint, sex)

# Show the complete table
print(literacy_table, n = Inf)



# 8. Find the age band with the largest gender gap

gap_table <- literacy_table %>%
  select(age_band, age_midpoint, sex_label, weighted_literacy_rate) %>%
  tidyr::pivot_wider(
    names_from = sex_label,
    values_from = weighted_literacy_rate
  ) %>%
  mutate(
    gap = abs(Men - Women)
  ) %>%
  arrange(desc(gap))

# Display all age groups from largest to smallest gap
print(gap_table, n = Inf)

# Select the age group with the largest gap
largest_gap <- gap_table %>%
  slice_max(gap, n = 1, with_ties = FALSE)

print(largest_gap)


# 9. Create the literacy rate graph

p3 <- ggplot(
  literacy_table,
  aes(
    x = age_midpoint,
    y = weighted_literacy_rate,
    color = sex_label,
    group = sex_label
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(
    aes(shape = thin_cell),
    size = 2.8
  ) +
  scale_shape_manual(
    values = c("FALSE" = 16, "TRUE" = 1),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("Men" = "#2878B5", "Women" = "#D55E00")
  ) +
  scale_x_continuous(
    breaks = seq(12, 62, by = 5),
    labels = c(
      "10–14", "15–19", "20–24", "25–29",
      "30–34", "35–39", "40–44", "45–49",
      "50–54", "55–59", "60–64"
    ),
    limits = c(12, 66)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100)
  ) +
  geom_text(
    data = literacy_table %>%
      filter(age_midpoint == 62),
    aes(label = sex_label),
    hjust = -0.15,
    show.legend = FALSE,
    size = 4
  ) +
  
  annotate(
    "text",
    x = 42,
    y = 25,
    label = paste0(
      "Largest gap: ",
      as.character(largest_gap$age_band),
      "\n",
      round(largest_gap$gap, 1),
      " percentage points"
    ),
    color = "black",
    size = 3.5
  ) +

  labs(
    title = "The gender literacy gap is largest among older adults",
    subtitle = "Weighted literacy rates in rural Balochistan",
    x = "Age band (years)",
    y = "Literacy rate (%)",
    caption = paste0(
      "Source: PSLM 2019–20 person and household datasets. ",
      "Sample: people aged 10–64 with non-missing literacy values. ",
      "Weighted by household weight. ",
      "Hollow markers indicate cells with fewer than 30 people."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 8),
    plot.margin = margin(10, 35, 10, 10)
  )

# Display the graph
print(p3)

# Save the graph as SVG
ggsave(
  "figures/q3_literacy_by_age.svg",
  plot = p3,
  width = 11,
  height = 7
)


# Q3: Create the full age-band table for README.md

readme_table <- literacy_table %>%
  mutate(
    weighted_literacy_rate = round(weighted_literacy_rate, 1)
  ) %>%
  select(
    age_band,
    sex_label,
    n_persons,
    weighted_literacy_rate
  ) %>%
  tidyr::pivot_wider(
    names_from = sex_label,
    values_from = c(n_persons, weighted_literacy_rate),
    names_glue = "{sex_label}_{.value}"
  ) %>%
  arrange(age_band)

print(readme_table, n = Inf)

