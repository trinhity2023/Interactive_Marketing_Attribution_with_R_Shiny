#avg_spend.R
library(dplyr)
library(ggplot2)

generate_avg_spend_plot <- function(transcript) {
  bins3 <- c(18, 35, 51, 68, 84, 102)
  labels3 <- c("18-34", "35-50", "51-67", "68-83", "84-101")
  transcript$age_group2 <- cut(transcript$age, breaks = bins3, labels = labels3, right = FALSE)
  
  sum_amount_by_age_group <- transcript %>%
    group_by(age_group2, gender) %>%
    summarize(total_amount = sum(amount, na.rm = TRUE), n_unique_customers = n_distinct(person)) %>%
    mutate(avg_spend = total_amount / n_unique_customers)%>%
    filter(!is.na(age_group2), gender != "O")
  
  colnames(sum_amount_by_age_group) <- c("age_group2", "gender", "total_amount","n_unique_customers","avg_dollars_spent")
  
  ggplot(sum_amount_by_age_group, aes(x = age_group2, y = avg_dollars_spent, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("pink", "blue")) +
    theme_minimal() +
    labs(x = "Customer Age Group Segment", y = "Average Spend in Dollars", fill = "Gender") +
    ggtitle("Average Spend by Age Group and Gender") +
    theme(legend.position = "top", axis.text.x = element_text(angle = 0, vjust = 0.5),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_text(aes(label = paste0("$", round(avg_dollars_spent, 2))),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3.5)
}