
library(readr)
library(dplyr)
library(ggplot2)

# 1. Load the datasets
person <- read_csv("data/pslm_person_subset.csv")
household <- read_csv("data/pslm_hh_subset.csv")

# 2. Add household information to the person dataset
data <- person %>%
  left_join(
    household %>%
      select(hhid, prov_code, urban_rural, hhweight),
    by = "hhid"
  )

# 3. Select Variant 6: rural Balochistan
data <- data %>%
  filter(prov_code == 4, urban_rural == 1)

# 4. Select the literacy sample
data <- data %>%
  filter(
    age >= 10,
    age <= 64,
    !is.na(literate)
  )

# 5. Draw the untouched default graph
p_default <- ggplot(data, aes(age, literate)) +
  geom_point()

# 6. Display and save the graph
print(p_default)

ggsave(
  "figures/default.svg",
  plot = p_default,
  width = 8,
  height = 6
)
