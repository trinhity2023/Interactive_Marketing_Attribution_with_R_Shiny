#funnel.R
library(plotly)

# Create dataframe for funnel chart
data <- data.frame(
  number = c(76277, 57725, 33579, 138953), # divide by 1000 to convert to thousands
  Funnel_Stage = c("Offers Received", "Offers Viewed", "Offers Completed", "Transactions")
)

# Set the order of the stages in the funnel
data$Funnel_Stage <- factor(data$Funnel_Stage, levels = c("Offers Received", "Offers Viewed", "Offers Completed", "Transactions"))

# Create funnel chart
fig <- plot_ly(data, type = "funnel", x = ~number, y = ~Funnel_Stage) 

