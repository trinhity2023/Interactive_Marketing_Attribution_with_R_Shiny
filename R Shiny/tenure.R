#tenure.R
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)

# Read the csv file
transcript <- read_csv("test1.csv")

# Get the current datetime object
d <- Sys.time()

# Create a new column "today" with current date
transcript <- transcript %>% mutate(today = format(d, "%Y-%m-%d"))

# Convert the "became_member_on" column to datetime format
transcript$became_member_on <- ymd(transcript$became_member_on)

# Calculate the customer tenure in days and years
transcript <- transcript %>% 
  mutate(customer_tenure_in_days = as.numeric(difftime(ymd(today), became_member_on, units = "days")),
         customer_tenure_in_years = customer_tenure_in_days / 365.25)

# Group by customers and calculate their average tenure
unique_customers <- transcript %>% 
  group_by(person) %>% 
  summarize(customer_tenure_in_years = mean(customer_tenure_in_years)) %>% 
  ungroup()

# Create the boxplot and histogram of customer tenure
tenure_plot <- ggplot(transcript, aes(x = customer_tenure_in_years)) +
  geom_density(alpha = 0.5) +
  labs(x = "Customer Tenure (in Years)", y = "Density", title = "Starbucks Member - User Tenure Distribution") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

