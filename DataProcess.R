library(dplyr)
library(readxl)
library(ggplot2)

# Import the dataset
dataset <- read_excel(
  "mpd2023_web.xlsx",
  sheet = "Full data"
)

# Data Understanding
glimpse(dataset)
colSums(is.na(dataset))

# Dataset Description
datasetYearRange <- range(dataset$year, na.rm = TRUE)
datasetYearRange

# Data Preparation

# Calculate total real GDP in billions of 2011 international dollars.
# Population is recorded in thousands, so dividing by 1,000,000
# converts real GDP from thousands of dollars to billions of dollars.
datasetWithGDP <- mutate(
  dataset,
  GDP = (gdppc * pop) / 1000000
)

# Select 2006 as the pre-crisis baseline year.
gdpBy2006 <- filter(datasetWithGDP, year == 2006)

# Rank the economies by total real GDP in descending order.
gdpRank2006 <- arrange(gdpBy2006, desc(GDP))
gdpRank2006

# Exclude Former USSR (SUN) because the Soviet Union dissolved in 1991.
# Although the dataset contains a 2006 observation for SUN, it does not
# represent a separate economy existing in the baseline year.
gdpRank2006Filter <- filter(
  gdpRank2006,
  countrycode != "SUN"
)

# Select the five largest economies in 2006.
gdpRank2006Sliced <- slice_head(gdpRank2006Filter, n = 5)
gdpRank2006Sliced

# Extract the country codes from the ranking.
top5Codes <- gdpRank2006Sliced$countrycode

# Create the analytical dataset for 2006-2009.
sampleCountry <- dataset |>
  filter(
    countrycode %in% top5Codes,
    year >= 2006 & year <= 2009
  )

# Add the 2006 real GDP per capita baseline and calculate percentage changes.
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
sampleMissingCounts <- colSums(is.na(sampleCountry))
sampleMissingCounts

# Check duplicate country-year records.
sampleCountryDuplicates <- sampleCountry |>
  count(countrycode, year) |>
  filter(n > 1)

sampleCountryDuplicates

# Check annual coverage for each economy.
sampleCountry |> count(countrycode)
sampleCountry

# Check non-positive real GDP per capita and population values.
sampleCountryInvalid <- sampleCountry |>
  filter(gdppc <= 0 | pop <= 0)
sampleCountryInvalid

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

# Check percentage changes from 2007 to 2009.
sampleChanges2007to2009 <- sampleCountry |>
  filter(year >= 2007 & year <= 2009) |>
  select(countrycode, year, pctChange)
sampleChanges2007to2009

# Modelling
# Visualise real GDP per capita trends across the five selected economies from 2006 to 2009.
ggplot(
  data = sampleCountry, 
  aes(x = year, y = gdppc, colour = country)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Real GDP per Capita Across Five Economies (2006–2009)",
    x = "Year",
    y = "Real GDP per Capita (2011 international dollars)",
    colour = "Economy") +
  scale_x_continuous(breaks = 2006:2009)

# Present real GDP per capita values for the five economies.
sampleCountry |>
  select(country, year, gdppc) |>
  tidyr::pivot_wider(names_from = year, values_from = gdppc) |>
  rename(Economy = country) |>
  knitr::kable(
    caption = "Real GDP per capita (2011 international dollars), 2006–2009",
    digits = 0
  )

# Compare percentage changes in real GDP per capita relative to 2006.
ggplot(
  data = sampleCountry |> filter(year >= 2007 & year <= 2009), 
  aes(x = factor(year), y = pctChange, fill = country)) + 
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Percentage Change in Real GDP per Capita Relative to 2006", 
        x = "Year",
        y = "Change from 2006 (%)",
        fill = "Economy")


# Present percentage changes in real GDP per capita relative to 2006.
sampleCountry |>
  filter(year >= 2007 & year <= 2009) |>
  select(country, year, pctChange) |>
  tidyr::pivot_wider(names_from = year,values_from = pctChange) |>
  rename(Economy = country) |>
  knitr::kable(
    caption = "Percentage change in real GDP per capita relative to 2006 (%)",
    digits = 2
  )





