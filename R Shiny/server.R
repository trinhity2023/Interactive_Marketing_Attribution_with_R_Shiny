#server.R 
#BLUE BARS
# Load necessary libraries
library(leaflet.extras)
library(RColorBrewer)
library(leaflet) 
library(sf)
library(magrittr) 
library(dplyr) 
library(ggplot2)
library(scales)
library(plotly)
library(readr)

source('map2.R')
source('cohort.R')
source('funnel.R')
source('tenure.R')
source("lollipop.R")
source('avg_spend.R')
source('user_growth.R')
source('channel_response.R')  

server <- function(input, output) {
  
  output$map2 <- renderLeaflet({
    map2
  })  
  
  output$member_plot <- renderPlot({
    calculate_members(transcript)
  })
  
  channel_distribution <- reactive({
    completed_offers <- read_csv("test1.csv") %>%
      filter(!is.na(offer_id))
    
    completed_offers %>%
      summarise(email = sum(email_x), 
                mobile = sum(mobile_x), 
                web = sum(web_x), 
                social = sum(social_x)) %>%
      pivot_longer(cols = everything(), names_to = "channels", values_to = "count_of_completed_offers") %>%
      mutate(channels = case_when(
        channels == "email_x" ~ "email",
        channels == "mobile_x" ~ "mobile",
        channels == "web_x" ~ "web",
        channels == "social_x" ~ "social",
        TRUE ~ channels
      )) %>%
      arrange(desc(count_of_completed_offers))
  })
  
  # Plot the data
  output$channel_frequency_plot <- renderPlot({
    ggplot(channel_distribution(), aes(x = reorder(channels, -count_of_completed_offers), y = count_of_completed_offers)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Channel Utilization Frequency \n that led to Offer Completion") +
      geom_text(aes(label = scales::comma(count_of_completed_offers)), vjust = -0.5, size = 4) +
      scale_y_continuous(labels = scales::comma) +
      ylab("Count of Completed Offers") +
      xlab("Channels") +
      theme(axis.text.x = element_text(hjust = .5, size=14),
            axis.text.y = element_text(size=14),
            axis.title = element_text(size=14),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
  
  output$cohort_plot <- renderPlot({
    ggplot(cohort2, aes(x=`Member Signup Year`, y=`Avg. Spend by Member Signup Year`)) +
      geom_bar(stat='identity', fill="steelblue") +
      #geom_line(color='indianred', size=1.5, group=1) +
      # geom_point(color='indianred', size=3) +
      scale_y_continuous(labels=scales::dollar, limits = c(0, 160)) +
      labs(title='Average Spend by Member Signup Year',
           x='Member Signup Year',
           y='Avg. Spend by Member Signup Year') +
      theme_minimal() +
      theme(axis.title = element_text(size=14),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
      geom_label(aes(label=scales::dollar(`Avg. Spend by Member Signup Year`),
                     y=`Avg. Spend by Member Signup Year`),
                 vjust=-0.5, size=4, label.padding=unit(0.35, "lines")) +
      scale_x_continuous(breaks=seq(2013, 2018, 1), labels=seq(2013, 2018, 1))
  })
  
  # Render cumulative user growth plot
  output$user_growth <- renderPlot({
    ggplot(cum_line_graph, aes(x = year, y = Cum_Sum_of_Users)) +
      geom_line() +
      labs(title = "Starbucks Member - User Growth over Time", x = "Year", y = "Cumulative Sum of Users") +xlim(2013, 2018) +scale_x_continuous(limits = c(2013, 2018), expand = c(0, 0)) +
      scale_y_continuous(labels = scales::comma, limits = c(0, 17000), breaks = seq(0, 17000, by = 1000), expand = c(0, 0)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "lines"), plot.title = element_text(hjust = 0.5, size=14,face = "bold"),
            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14))
    
  })
  
  # Plot total spending by cohort
  output$spending_plot <- renderPlot({
    ggplot(data = cohort, aes(x = `Member Signup Year`, y = `Total Spending (in dollars)`)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      # geom_line(aes(group = 1), color = "indianred", size = 1.2) +
      # geom_point(color = "indianred", size = 3) +
      scale_y_continuous(labels = scales::comma, breaks = seq(0, 700000, 100000)) +
      scale_x_continuous(breaks = seq(2013, 2018, 1), labels = seq(2013, 2018, 1)) +
      labs(title = "Total Spending in Dollars by Starbucks Member Cohort Signup Year") +
      theme_minimal() +
      geom_text(aes(label = scales::comma(`Total Spending (in dollars)`),
                    y = `Total Spending (in dollars)`),
                vjust = -0.5, size = 5) +
      theme(plot.title = element_text(hjust = 0.5, size=14,face = "bold"),
            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14)
      )
  })
  
  output$avg_spend_plot <- renderPlot({
    generate_avg_spend_plot(transcript)
  })
  
  # Create reactive function to filter data based on selected offer type
  filtered_data2 <- reactive({
    if (input$offer_type == "All") {
      return(response)
    } else if (input$offer_type == "Discount") {
      return(filter(response, grepl("DISCOUNT", `Offer Type Variant`, ignore.case = TRUE)))
    } else if (input$offer_type == "BOGO") {
      return(filter(response, grepl("BOGO", `Offer Type Variant`, ignore.case = TRUE)))
    }
  })
  
  # Render lollipop plot based on selected offer type
  output$lollipop_plot <- renderPlot({
    filtered_response <- filtered_data2()
    
    ggplot(filtered_response %>%
             arrange(`Percent of Offers Completed/Offers Received`) %>%
             mutate(`Offer Type Variant` = factor(`Offer Type Variant`, levels = (`Offer Type Variant`))),
           aes(x = `Percent of Offers Completed/Offers Received`, y = `Offer Type Variant`, color = ifelse(`Percent of Offers Completed/Offers Received` < 50, 'red', 'green'))) +
      geom_vline(xintercept = 50, color = 'black', alpha = 0.5, linetype = 'dashed') +
      geom_segment(aes(x = 0, xend = `Percent of Offers Completed/Offers Received`, y = `Offer Type Variant`, yend = `Offer Type Variant`, color = ifelse(`Percent of Offers Completed/Offers Received` < 50, 'red', 'green')), size = 2) +
      geom_point(size = 3, color = 'orange', alpha = 1) +
      labs(x = 'Response Rate Percentage', y = 'Offer Type Variant', title = paste("Response Rate of Offer Type Variant: ", input$offer_type, "\nRanked From Highest to Lowest \n\n(Green if Response Rate >=50%, Red Otherwise)")) +
      scale_color_identity() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(), axis.line = element_line(colour = "black"),
            plot.title = element_text(face = "bold", size=16),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14),
      )
  })
  
  # Render the tenure plot
  output$tenure_plot <- renderPlot({
    tenure_plot
  })
  
  # Render the funnel chart
  output$funnel_plot <- renderPlotly({
    fig
  })
  
  # Define reactive transcript_age
  transcript_age <- reactive({
    # Create age groups
    bins <- c(18, 35, 51, 68, 84, 102)
    labels <- c('18-34', '35-50', '51-67', '68-83', '84-101')
    transcript %>% 
      mutate(age_group = cut(age, breaks = bins, labels = labels, right = FALSE))
  })
  
  # Define reactive age_group_response_rate
  age_group_response_rate <- reactive({
    # Calculate age group response rate
    if (input$age_group == "All") {
      transcript_c <- transcript_filtered() %>% 
        filter(event == 'offer completed') %>% 
        group_by(age_group) %>% 
        summarise(event_count = n())
      
      transcript_d <- transcript_filtered() %>% 
        filter(event == 'offer received', 
               offer_type_y != 'informational') %>% 
        group_by(age_group) %>% 
        summarise(event_count = n())
    } else {
      transcript_c <- transcript_filtered() %>% 
        filter(event == 'offer completed', 
               age_group == input$age_group) %>% 
        group_by(age_group) %>% 
        summarise(event_count = n())
      
      transcript_d <- transcript_filtered() %>% 
        filter(event == 'offer received', 
               offer_type_y != 'informational',
               age_group == input$age_group) %>% 
        group_by(age_group) %>% 
        summarise(event_count = n())
    }
    
    transcript_c %>% 
      left_join(transcript_d, by = 'age_group') %>% 
      mutate(age_group_response_rate = round(event_count.x / event_count.y * 100, 1)) %>% 
      select(age_group, age_group_response_rate)
  })
  
  # Create plot
  output$age_group_response_rate_plot <- renderPlot({
    ggplot(age_group_response_rate(), aes(x = age_group, y = age_group_response_rate, fill = age_group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(age_group_response_rate, "%"), fontface = 'bold'),
                position = position_stack(vjust = 0.95), color = 'white') +
      
      scale_fill_manual(values = c('steelblue','steelblue','steelblue','steelblue','steelblue')) +
      labs(title = "Age Group Response Rate",
           x = "Age Group Segment",
           y = "Age Group Response Rate") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Create age groups
  bins <- c(18, 35, 51, 68, 84, 102)
  labels <- c('18-34', '35-50', '51-67', '68-83', '84-101')
  transcript <- transcript %>% 
    mutate(age_group = cut(age, breaks = bins, labels = labels, right = FALSE)) %>%
    filter(!is.na(age_group))
  
  transcript_filtered <- reactive({
    if (input$age_group == "All" && input$gender == "All" && input$income_group == "All") {
      transcript
    } else if (input$age_group == "All" && input$gender == "All") {
      transcript %>% filter(income_group == input$income_group)
    } else if (input$age_group == "All" && input$income_group == "All") {
      transcript %>% filter(gender == input$gender)
    } else if (input$gender == "All" && input$income_group == "All") {
      transcript %>% filter(age_group == input$age_group)
    } else if (input$age_group == "All") {
      transcript %>% filter(gender == input$gender, income_group == input$income_group)
    } else if (input$gender == "All") {
      transcript %>% filter(age_group == input$age_group, income_group == input$income_group)
    } else if (input$income_group == "All") {
      transcript %>% filter(age_group == input$age_group, gender == input$gender)
    } else {
      transcript %>% filter(age_group == input$age_group, gender == input$gender, income_group == input$income_group)
    }
  })
  
  
  # Calculate Gender Response Rate based on reactive data frame
  transcript_a <- reactive({
    transcript_filtered() %>%
      filter(event == "offer completed", gender %in% c("M", "F")) %>%
      group_by(gender) %>%
      summarise(count = n())
  })
  
  transcript_b <- reactive({
    transcript_filtered() %>%
      filter(event == "offer received", offer_type_y != "informational", gender %in% c("M", "F")) %>%
      group_by(gender) %>%
      summarise(count = n())
  })
  
  gender_response_rate <- reactive({
    data.frame(
      Gender = transcript_a()$gender,
      Gender_Response_Rate = round(transcript_a()$count/transcript_b()$count*100, 1)
    )
  })
  
  # Create the plot object
  output$gender_response_plot <- renderPlot({
    ggplot(gender_response_rate(), aes(x = Gender, y = Gender_Response_Rate, fill = Gender)) +
      geom_bar(stat = "identity", width = 0.5) + 
      scale_fill_manual(values = c("pink", "blue")) +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = paste(round(Gender_Response_Rate, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      ggtitle("Gender Response Rate") +
      theme(axis.text.x = element_text(angle =90),
            plot.title = element_text(hjust = 0.5))+
      xlab("Gender") + 
      ylab("Gender Response Rate") +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  })
  
  bins2 <- c(30000, 50001, 75001, 100001, 120001)
  labels2 <- c('30k-50k', '50k-75k', '75k-100k', '100k-120k')
  transcript$income_group <- cut(transcript$income, breaks = bins2, labels = labels2, right = FALSE)
  # Create reactive expression for filtered and calculated data
  
  filtered_data <- reactive({
    if (input$income_group == "All") {
      # return all data without filtering by income group
      transcript_e <- transcript_filtered() %>% 
        filter(event == 'offer completed') %>%
        group_by(income_group) %>% summarize(event_count = n())
      
      transcript_f <- transcript_filtered() %>% 
        filter(event == 'offer received', offer_type_y != 'informational') %>%
        group_by(income_group) %>% summarize(event_count = n())
    } else {
      # filter data by income group
      transcript_e <- transcript_filtered() %>% 
        filter(event == 'offer completed', income_group %in% input$income_group) %>%
        group_by(income_group) %>% summarize(event_count = n())
      
      transcript_f <- transcript_filtered() %>% 
        filter(event == 'offer received', offer_type_y != 'informational', income_group %in% input$income_group) %>%
        group_by(income_group) %>% summarize(event_count = n())
    }
    
    
    income_group_response_rate <- transcript_e %>% 
      left_join(transcript_f, by = 'income_group') %>%
      mutate(response_rate = round(event_count.x / event_count.y * 100, 1)) %>%
      select(income_group, response_rate)
    income_group_response_rate
  })
  
  
  
  # Render plot
  output$income_group_response_rate_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = income_group, y = response_rate, fill = income_group)) +
      geom_bar(stat = 'identity', color = 'black') +
      scale_fill_manual(values = c('steelblue', 'steelblue', 'steelblue', 'steelblue')) +
      geom_text(aes(label = paste0(response_rate, '%')), position = position_stack(vjust = 0.5), color = 'white', fontface = 'bold') +
      labs(title = 'Income Group Response Rate', x = 'Income Group Segment', y = 'Income Group Response Rate') +
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
    
  })
  output$channel_response_plot <- renderPlot({
    channel_response_rate_df <- calculate_response_rates(transcript)
    channel_response_rate_df$channels <- reorder(channel_response_rate_df$channels, 
                                                 channel_response_rate_df$channel_response_rate)
    
    channel_response_rate_df$channel_response_rate <- channel_response_rate_df$channel_response_rate - 6
    
    ggplot(channel_response_rate_df, aes(x = channels, y = channel_response_rate)) +
      geom_bar(stat = "identity", fill = "#74add1") +
      geom_text(aes(label = paste0(channel_response_rate, "%"), y = channel_response_rate), 
                position = position_stack(vjust = 0.5), color = "black") +
      coord_flip() +
      theme_classic() +
      ggtitle("Channel Response Rate") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14)) +
      labs(x = "Channels", y = "Channel Response Rate (in Percentages)")
  })
  
}
