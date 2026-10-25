library(dplyr)

dataset <- read_excel(
  "Maddison Project Dataset.xlsx",
  sheet = "Full data"
)

datasetWithGDP <- mutate(dataset, GDP = (gdppc*pop)/1000000)

gdpBy2006 <- filter(datasetWithGDP, year == 2006)

gdpRank2006 <- arrange(gdpBy2006, desc(GDP))

slice_head(gdpRank2006, n = 10)