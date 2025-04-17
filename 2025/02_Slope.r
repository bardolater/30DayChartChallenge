#------ #30DayChartChallenge Day 1: Slope ----------#
# Data are from https://data.delaware.gov/Transportation/Public-Crash-Data/827n-m6xc/about_data

library(tidyverse)
library(ggtext)
library(showtext)
library(directlabels)
library(here)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
font_add_google("Roboto Condensed")
font <- "Roboto Condensed"
d1 <- read_csv("Public_Crash_Data_20250415.csv")
d2 <- d1 |>   janitor::clean_names() 

d3 <-  d2 |> 
  mutate(crash_year = substr(crash_datetime, 7, 10)) |> 
  select(crash_year, bicycled_involved, alcohol_involved, crash_classification_code, primary_contributing_circumstance_description) |> 
  rename(bicycle_involved = bicycled_involved) |> 
  filter(crash_classification_code=='04' & crash_year != '2024') |> 
  group_by(crash_year, alcohol_involved) |> 
  summarise(n=n()) |> 
  ungroup() |> 
  mutate(varlabel = case_match(alcohol_involved, 'N'~'Alcohol not involved','Y' ~'Alcohol involved'))

subtext <- "Total fatalilty crashes in 2023 is almost 25% greater than the total in 2009."
captiontext <- "Data from Delaware Public Crash Data"
d3|> 
  ggplot(aes(x=as.numeric(crash_year), y=n, fill=alcohol_involved)) +
  ylab("Fatality Crashes") +
  geom_area() +
  labs(x = '', y='', subtitle = subtext, caption = captiontext)+
  ggtitle("Total Fatality Crashes in Delaware by Whether Alcohol Was Involved") +
  scale_x_continuous(breaks=c(2009:2023))+
  scale_fill_manual(values = c("purple", "orange"))+
  geom_dl(aes(label=varlabel),method="smart.grid")+
  scale_y_continuous(breaks = c(0,25, 50, 75,100,125))+
  theme(axis.text.x = element_text(size=10, family=font),
        axis.text.y = element_text(size=10, family=font),
        plot.title = element_text(size=14, family = font),
        plot.subtitle = element_text(size=12, family = font),
        axis.title.y = element_text(size=10, family=font, angle=45,  vjust = 0, hjust=0),
        legend.position = "none")
ggsave("./30DayChartChallenge/2025/02_slope.png", dpi = 300, width = 7, height = 5, units="in", bg="#eaf3ee")
