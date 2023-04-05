#ui.R
library(leaflet.extras) 
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(leaflet)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Starbucks Rewards"),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      menuItem("Response Rates", tabName = "tab1"),
      menuItem("Avg Spend by Age Group & Gender", tabName = "tab2"),
      menuItem("Marketing Offer Ranking", tabName = "tab3"),
      menuItem("Cohort Analysis & User Growth/Tenure", tabName = "tab6"),
      menuItem("Channel Effectiveness", tabName = "tab7"),
      menuItem("Funnel Stages", tabName = "tab4"),
      menuItem("Active/Low Engagement/Inactive Members", tabName = "tab8"),
      menuItem("Offer Sent to Spend Time Average", tabName = "tab10"),
      menuItem("US Starbucks Heatmap as of 3/1/23", tabName = "tab11")
      
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.nav > li > a {padding-top: 10px !important; padding-bottom: 10px !important;}'))),
    tags$style(type = "text/css", "#map2 {height: 100vh !important;}"), # Add this line
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(
                column(width = 4, selectInput("age_group", "Select Age Group", choices = c("All", "18-34", "35-50", "51-67", "68-83", "84-101"))),
                column(width = 4,selectInput("gender", "Select Gender", choices = c("All", "M", "F"))),
                column(width = 4, selectInput("income_group", "Select Income Group", choices = c("All", "30k-50k","50k-75k","75k-100k","100k-120k")))
              ),
              fluidRow(
                column(width = 6, plotOutput("gender_response_plot")),
                column(width = 6, plotOutput("age_group_response_rate_plot"))
              ),
              fluidRow(
                column(width = 6, plotOutput("income_group_response_rate_plot"))
              )
      ),
      tabItem(tabName = "tab4",
              fluidRow(
                column(width = 12, plotlyOutput("funnel_plot"))
              )
      ),
      tabItem(tabName = "tab3",
              fluidRow(column(width = 4, selectInput("offer_type", "Select Offer Type",
                                                     choices = c("All", "BOGO", "Discount"), 
                                                     selected = "All"))),
              fluidRow(
                column(width = 12, plotOutput("lollipop_plot"))
              )
      ),
      tabItem(tabName = "tab2",
              fluidRow(
                column(width = 6, plotOutput("avg_spend_plot"))
              )
      ),
      tabItem(tabName = "tab6",
              fluidRow(
                column(width = 6, plotOutput("spending_plot")),
                column(width = 6, plotOutput("user_growth")),
                column(width = 6, plotOutput("cohort_plot")),
                column(width = 6, plotOutput("tenure_plot"))
              )
      ),
      tabItem(tabName = "tab7",
              fluidRow(
                column(width = 6, plotOutput("channel_frequency_plot")),
                column(width = 6, plotOutput("channel_response_plot"))
              )
      ),
      tabItem(tabName = "tab8",
              fluidRow(
                column(width = 4, tags$img(src = "pie_chart2.jpeg", width = "450", height = "350"))
              )
      ),
      tabItem(tabName = "tab10",
              fluidRow(
                column(width = 12, tags$img(src = "Time Distribution.png"))
              )
      ),
      tabItem(tabName = "tab11",
              fluidRow(
                column(width = 12, leafletOutput("map2"))
              )
      )
    
  )
)
)