#member.R
library(shiny)
library(ggplot2)
library(dplyr)

transcript <- read.csv("test1.csv")

calculate_members <- function(transcript) {
  total_members <- length(unique(transcript$person))
  
  inactive_members <- 422
  active_members <- 16578
  
  df <- data.frame(Member_Status = c('Active Members', 'Inactive Members'),
                   Number_Members = c(active_members, inactive_members))
  
  autopct_format <- function(values) {
    function(pct) {
      total <- sum(values)
      val <- round(pct * total) / 100
      ifelse(values == active_members, 
             paste0('Active Members\n',sprintf("%.1f", pct), '%\n(', val, ')'), 
             paste0('Inactive Members\n',sprintf("%.1f", pct), '%\n(', val, ')'))
    }
  }
  
  member_plot<- ggplot(df, aes(x = "", y = Number_Members, fill = Member_Status)) +
    geom_col(width = 1) +
    coord_polar(theta = "y", start = 1/4 * pi)  +
    scale_fill_manual(values = c('#66c2a5', '#fc8d62')) +
    geom_text(aes(x = 2.5, y = Number_Members/2, 
                  label = autopct_format(round(Number_Members,0))(round(Number_Members/sum(Number_Members)*100, 1))), size = 5) +
    theme_void() +
    ggtitle('Active vs Inactive Members') +
    theme(plot.title = element_text(hjust = 0.5, size = 20))+
    guides(fill=FALSE)
  
  return(member_plot)
}

member_plot
