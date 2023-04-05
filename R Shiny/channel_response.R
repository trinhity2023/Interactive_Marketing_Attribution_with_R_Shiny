#channel_response.R
library(dplyr)

calculate_response_rates <- function(transcript) {
  emails_completed <- sum(transcript$event == 'offer completed' & transcript$email_x == 1)
  emails_received <- sum(transcript$event == 'offer received' & transcript$offer_type_y != 'informational' & transcript$email_y == 1)
  email_response_rate <- round((emails_completed / emails_received * 100), 0)
  
  mobile_completed <- sum(transcript$event == 'offer completed' & transcript$mobile_x == 1)
  mobile_received <- sum(transcript$event == 'offer received' & transcript$offer_type_y != 'informational' & transcript$mobile_y == 1)
  mobile_response_rate <- round((mobile_completed / mobile_received * 100), 0)
  
  web_completed <- sum(transcript$event == 'offer completed' & transcript$web_x == 1)
  web_received <- sum(transcript$event == 'offer received' & transcript$offer_type_y != 'informational' & transcript$web_y == 1)
  web_response_rate <- round((web_completed / web_received * 100), 0)
  
  social_completed <- sum(transcript$event == 'offer completed' & transcript$social_x == 1)
  social_received <- sum(transcript$event == 'offer received' & transcript$offer_type_y != 'informational' & transcript$social_y == 1)
  social_response_rate <- round((social_completed / social_received * 100), 0)
  
  channel_response_rate <- c(email = email_response_rate, 
                             mobile = mobile_response_rate,
                             web = web_response_rate,
                             social = social_response_rate)
  
  channel_response_rate_df <- data.frame(channels = names(channel_response_rate),
                                         channel_response_rate = channel_response_rate,
                                         stringsAsFactors = FALSE) %>% 
    arrange(desc(channel_response_rate))
  
  return(channel_response_rate_df)
}
