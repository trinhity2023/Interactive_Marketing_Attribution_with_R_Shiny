library(magick)
library(ggplot2)

transcript <- read.csv('test1.csv')

total_members <- length(unique(transcript$person))

average_order_value <- round(sum(transcript$amount, na.rm = TRUE) / sum(!is.na(transcript$amount)), 2)

active_members <- sum(tapply(transcript$amount, transcript$person, sum, na.rm = TRUE) > average_order_value)

low_engagement_members <- sum((tapply(transcript$amount, transcript$person, sum, na.rm = TRUE) > 0) & 
                                (tapply(transcript$amount, transcript$person, sum, na.rm = TRUE) <= average_order_value))

inactive_members <- total_members - low_engagement_members - active_members

df <- data.frame(Number_Members = c(active_members, low_engagement_members, inactive_members),
                 Member_Type = c('Active Members', 'Low Engagement Members', 'Inactive Members'))


# Calculate the percentages for each member type
percentages <- round(df$Number_Members/sum(df$Number_Members)*100, 1)

my_autopct <- function(count, Member_Type) {
  pct <- round(100 * count / sum(df$Number_Members), 1)
  label <- paste0(pct, "% (", count, " Users) - \n", Member_Type)
  return(label)
}

map<-ggplot(df, aes(x='', y=Number_Members, fill=Member_Type)) +
  geom_col(width = 11) +
  geom_bar(width=1, stat='identity') +
  coord_polar(theta='y') +
  geom_text(aes(label=my_autopct(Number_Members, Member_Type), angle=45, hjust=.3), 
  #geom_text(aes(label=paste(paste0(percentages, "%"))), 
            position=position_stack(vjust=0.5), size=4) +
  scale_fill_manual(values=c('#41AB5D', '#E6550D', '#FEE08B')) +
  theme_void()+
  guides(fill = "none")

map
