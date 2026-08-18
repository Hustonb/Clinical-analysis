#Load necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(gtsummary)

#Import data as Tibble
Viewraw_data <-readr::read_csv("Data/survey.csv")
raw_data