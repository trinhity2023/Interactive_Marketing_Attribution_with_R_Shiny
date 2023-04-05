#cohort.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Load data
transcript <- read_csv("test1.csv")

# Convert dates and calculate customer tenure
d <- Sys.Date()
transcript$today <- format(d, "%Y-%m-%d")
transcript$became_member_on <- as.Date(as.character(transcript$became_member_on), format = "%Y%m%d")
transcript$customer_tenure_in_days <- as.numeric(difftime(as.Date(transcript$today), as.Date(transcript$became_member_on), units = "days"))
transcript$customer_tenure_in_years <- transcript$customer_tenure_in_days/365
transcript$year <- as.integer(format(transcript$became_member_on, "%Y"))

# Summarize data
cohort_ct <- transcript %>%
  group_by(year) %>%
  summarise(count = n_distinct(person))

cohort2 <- transcript %>%
  group_by(year) %>%
  summarise(`Total Spending (in dollars)` = sum(amount, na.rm = TRUE))

cohort2 <- cohort2 %>%
  left_join(cohort_ct, by = "year") %>%
  mutate(`Avg. Spend by Member Signup Year` = `Total Spending (in dollars)` / count) %>%
  rename(`Cohort Size` = count, `Member Signup Year` = `year`,
         `Total Spending (in dollars)` = `Total Spending (in dollars)`,
         `Avg. Spend by Member Signup Year` = `Avg. Spend by Member Signup Year`)