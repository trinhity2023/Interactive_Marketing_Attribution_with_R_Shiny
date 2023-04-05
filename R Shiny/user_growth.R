#user_growth.R
library(shiny)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

# Read data
transcript <- read.csv("test1.csv")

# Convert dates and calculate customer tenure
d <- Sys.Date()
transcript$today <- format(d, "%Y-%m-%d")
transcript$became_member_on <- as.Date(as.character(transcript$became_member_on), format = "%Y%m%d")
transcript$customer_tenure_in_days <- as.numeric(difftime(as.Date(transcript$today), as.Date(transcript$became_member_on), units = "days"))
transcript$customer_tenure_in_years <- transcript$customer_tenure_in_days/365
transcript$year <- as.integer(format(transcript$became_member_on, "%Y"))

# Create cumulative user growth plot
cum_line_graph <- transcript %>%
  group_by(year) %>%
  summarise(Cum_Sum_of_Users = n_distinct(person)) %>%
  mutate(Cum_Sum_of_Users = cumsum(Cum_Sum_of_Users))

# Summarize total spending by cohort
cohort <- transcript %>%
  group_by(year) %>%
  summarize(`Total Spending (in dollars)` = sum(amount, na.rm = TRUE)) %>%
  rename(`Member Signup Year` = year)

