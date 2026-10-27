
library(dplyr)
library(readxl)

# Import the dataset
dataset <- read_excel(
  "mpd2023_web.xlsx",
  sheet = "Full data"
)

# Data Understanding
glimpse(dataset)
colSums(is.na(dataset))

# Dataset Description
range(dataset$year, na.rm = TRUE)


# Data Preparation

# Calculate total real GDP in billions of 2011 international dollars.
# Population is recorded in thousands, so dividing by 1,000,000
# converts GDP from thousands of dollars to billions of dollars.
datasetWithGDP <- mutate(
  dataset,
  GDP = (gdppc * pop) / 1000000
)

# Select 2006 as the pre-crisis baseline year.
gdpBy2006 <- filter(datasetWithGDP, year == 2006)

# Exclude Former USSR (SUN) because the Soviet Union dissolved in 1991.
# Although the dataset contains a 2006 observation for SUN, it does not
# represent a separate economy existing in the baseline year.
gdpRank2006Filter <- filter(
  gdpBy2006,
  countrycode != "SUN"
)

# Rank the remaining economies by total real GDP in descending order.
gdpRank2006 <- arrange(gdpRank2006Filter, desc(GDP))

# Select the five largest economies in 2006.
gdpRank2006Sliced <- slice_head(gdpRank2006, n = 5)

# Extract the country codes from the ranking.
top5Codes <- gdpRank2006Sliced$countrycode

# Create the analytical dataset for 2006-2009.
sampleCountry <- dataset |>
  filter(
    countrycode %in% top5Codes,
    year >= 2006 & year <= 2009
  )

# Add the 2006 GDP per capita baseline and calculate percentage changes.
sampleCountry <- sampleCountry |>
  group_by(countrycode) |>
  mutate(
    gdppc2006 = gdppc[year == 2006],
    pctChange = (gdppc - gdppc2006) / gdppc2006 * 100
  ) |>
  ungroup()

# Data Quality Assessment

# Check the number of observations.
nrow(sampleCountry)

# Check missing values.
colSums(is.na(sampleCountry))

# Check duplicate country-year records.
sampleCountryDuplicates <- sampleCountry |>
  count(countrycode, year) |>
  filter(n > 1)

sampleCountryDuplicates

# Check annual coverage for each economy.
sampleCountry |>
  count(countrycode)

# Check non-positive GDP per capita and population values.
sampleCountryInvalid <- sampleCountry |>
  filter(gdppc <= 0 | pop <= 0)

sampleCountryInvalid

# Identify historical entities with observations in 2006.
dataset |>
  filter(
    countrycode %in% c("SUN", "CSK", "YUG"),
    year == 2006
  )


# Final Dataset Checks
# Verify selected economy codes.
top5Codes

# Inspect the analytical dataset.
glimpse(sampleCountry)

# Check the 2006 baseline values.
sampleCountry |>
  select(countrycode, year, gdppc, gdppc2006)

# Confirm the number of observations.
nrow(sampleCountry)

# Check that percentage change is zero in the 2006 baseline year.
sampleCountry |>
  filter(year == 2006) |>
  select(countrycode, year, pctChange)

# Check percentage changes for all five economies from 2006 to 2009.
sampleCountry |>
  select(countrycode, year, pctChange)

