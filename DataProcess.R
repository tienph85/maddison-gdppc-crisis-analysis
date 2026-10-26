library(dplyr)
library(readxl)

dataset <- read_excel(
  "Maddison Project Dataset.xlsx",
  sheet = "Full data"
)

# Calculate total real GDP in billions of 2011 dollars.
# Population is recorded in thousands, so dividing by 1,000,000
# converts GDP from thousands of dollars to billions of dollars.
datasetWithGDP <- mutate(dataset, GDP = (gdppc*pop)/1000000)

# Select 2006 as the pre-crisis baseline year.
gdpBy2006 <- filter(datasetWithGDP, year == 2006)

# Exclude Former USSR (SUN) because the Soviet Union dissolved in 1991.
# Although the dataset contains a 2006 observation for SUN, it does not
# represent a separate economy existing in the baseline year
gdpRank2006Filter <- filter(gdpBy2006, countrycode != "SUN")

# Rank the remaining economies by total real GDP in descending order.
gdpRank2006 <- arrange(gdpRank2006Filter, desc(GDP))

# Select the five largest economies in 2006.
slice_head(gdpRank2006, n = 5)



