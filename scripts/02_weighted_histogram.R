
# ECO346 Assignment 1 - Question 2
# Variant 6: Balochistan rural

library(readr)
library(dplyr)

# Read the two datasets
person <- read_csv("data/pslm_person_subset.csv")
household <- read_csv("data/pslm_hh_subset.csv")

# Check the columns in both datasets
names(person)
names(household)

# Check the first six rows
head(person)
head(household)


# Join person and household data using hhid
data <- person %>%
  left_join(
    household %>%
      select(hhid, prov_code, urban_rural, hhweight),
    by = "hhid"
  )

# Keep Variant 6: Balochistan rural
data <- data %>%
  filter(
    prov_code == 4,
    urban_rural == 1
  )

# Check the filtered data
nrow(data)
head(data)


# Keep people with positive annual education spending
students <- data %>%
  filter(
    !is.na(ed_total),
    ed_total > 0,
    !is.na(hhweight),
    hhweight > 0
  )

# Check the number of students
nrow(students)

# Check the spending and weight columns
head(students %>% select(ed_total, hhweight))

# Question 2: Calculate weighted spending statistics

# Function to calculate a weighted quantile
weighted_quantile <- function(x, w, p) {
  # Sort spending values from smallest to largest
  order_index <- order(x)
  x <- x[order_index]
  w <- w[order_index]
  
  # Calculate cumulative share of weights
  cumulative_weight <- cumsum(w) / sum(w)
  
  # Find the first value reaching the requested percentile
  x[which(cumulative_weight >= p)[1]]
}

# Calculate weighted statistics
weighted_median <- weighted_quantile(
  students$ed_total, students$hhweight, 0.50
)

weighted_p10 <- weighted_quantile(
  students$ed_total, students$hhweight, 0.10
)

weighted_p90 <- weighted_quantile(
  students$ed_total, students$hhweight, 0.90
)

# Calculate the ordinary (unweighted) median
unweighted_median <- median(students$ed_total)

# Calculate the ratio
p90_median_ratio <- weighted_p90 / weighted_median

# Display the results
cat("Number of students:", nrow(students), "\n")
cat("Weighted median (Rs):", weighted_median, "\n")
cat("Unweighted median (Rs):", unweighted_median, "\n")
cat("Weighted 10th percentile (Rs):", weighted_p10, "\n")
cat("Weighted 90th percentile (Rs):", weighted_p90, "\n")
cat("P90 / weighted median ratio:", round(p90_median_ratio, 2), "\n")

# Count spending values outside the required range

excluded <- students %>%
  filter(ed_total < 100 | ed_total > 1000000)

cat("Observations excluded from histogram:", nrow(excluded), "\n")

# Keep observations inside the required range
hist_data <- students %>%
  filter(ed_total >= 100, ed_total <= 1000000)

cat("Observations included in histogram:", nrow(hist_data), "\n")


# Question 2: Weighted histogram

library(ggplot2)

p <- ggplot(
  hist_data,
  aes(x = log10(ed_total), weight = hhweight)
) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 2,
    aes(y = after_stat(density)),
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(
    xintercept = log10(weighted_median),
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = log10(weighted_p90),
    linetype = "dashed",
    color = "darkgreen",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = log10(weighted_median),
    y = Inf,
    label = paste0("Median: Rs ", weighted_median),
    color = "red",
    angle = 90,
    vjust = 1.3,
    hjust = 1.1,
    size = 3
  ) +
  annotate(
    "text",
    x = log10(weighted_p90),
    y = Inf,
    label = paste0("P90: Rs ", weighted_p90),
    color = "darkgreen",
    angle = 90,
    vjust = 1.3,
    hjust = 1.1,
    size = 3
  ) +
  scale_x_continuous(
    breaks = 2:6,
    labels = c(
      "Rs 100", "Rs 1,000", "Rs 10,000",
      "Rs 100,000", "Rs 1,000,000"
    )
  ) +
  labs(
    title = "Annual education spending varies widely among students",
    subtitle = "Variant 6: Balochistan rural",
    x = "Annual education spending (rupees, log scale)",
    y = "Weighted density per log10 unit",
    caption = paste0(
      "Source: PSLM person and household datasets. ",
      "Sample: people with positive annual education spending. ",
      "Weighted by household weight. ",
      "Excluded outside Rs 100–1,000,000: ", nrow(excluded), "."
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 8),
    plot.caption.position = "plot",
    plot.margin = margin(10, 20, 15, 10)
  )

print(p)

ggsave(
  "figures/q2_weighted_histogram.svg",
  plot = p,
  width = 10,
  height = 7
)
