#lollipop.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
# Read data
transcript <- read.csv('test1.csv')

transcript_a <- transcript[transcript$event == 'offer completed',] %>%
  group_by(offer_type_x, difficulty_x, reward_x, duration_x) %>%
  summarise(`Number of Offers Completed` = n()) %>%
  ungroup()

transcript_a <- transcript_a %>%
  rename(offer_type = offer_type_x,
         difficulty = difficulty_x,
         reward = reward_x,
         duration = duration_x)


transcript_b <- transcript[(transcript$event == 'offer received') & (transcript$offer_type_y != 'informational'),] %>%
  group_by(offer_type_y, difficulty_y, reward, duration_y) %>%
  summarise(`Number of Offers Received` = n()) %>%
  ungroup()


transcript_b <- transcript_b %>%
  rename(offer_type = offer_type_y,
         difficulty = difficulty_y,
         duration = duration_y)

response <- inner_join(transcript_a, transcript_b, by = c('offer_type', 'difficulty', 'reward', 'duration'))

response <- response %>%
  mutate(`Percent of Offers Completed/Offers Received` = round(`Number of Offers Completed` / `Number of Offers Received` * 100, 1),
         `Offer Type Variant` = paste(toupper(offer_type), ': Spend: ', difficulty, ', Reward: ', reward, ', Duration: ', duration, sep = '')) %>%
  select(`Offer Type Variant`, `Percent of Offers Completed/Offers Received`)
