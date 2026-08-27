#Load necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(gtsummary)
library(car)
library(ResourceSelection)

#Import data as tibble
raw_data <-readr::read_csv("Data/survey.csv")