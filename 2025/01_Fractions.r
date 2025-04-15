#------ #30DayChartChallenge Day 1: Fractions ----------#
# Data are from https://data.delaware.gov/Transportation/Public-Crash-Data/827n-m6xc/about_data
# Crash figures are from 2009 until 6 months before the present date. Thus figures
# are not current as of today, April 15, 2025 but sometime in late 2024. 


library(tidyverse)
install.packages("waffle")
library(waffle)
library(janitor)

d1 <- read_csv("Public_Crash_Data_20250415.csv")
d2 <- d1 |>   janitor::clean_names() |> 
  mutate(crash_year = substr(crash_datetime, 7, 10)) |> 
  filter(crash_classification_code=='04') |> 
  select(crash_year, bicycled_involved) |> 
  rename(bicycle_involved = bicycled_involved) |> 
  group_by(bicycle_involved) |> 
  summarise(n=n()) |> 
  ungroup()

# Data from d2 is used to make this dataset by hand. 
c(
  `Bicycle Involved` = 59, 
  `No Bicycle Involved` = 1609
) -> parts

waffle(
  parts = parts, 
  rows = 30, 
  size = 1, 
  colors = c("orange", "#1879bf"),
  legend_pos = "bottom",
  title="1668 Fatality Crashes in Delaware (2009-2024)"
)

