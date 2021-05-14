library("dplyr")
library("ggplot2")

table8_33 <-
  read.csv("data/table8_33_clean.csv")[2:16,] %>%
    rename("Reason" = ï..Reason, "2017" = X2017, "2018" = X2018)
table8_33$Count <-
  as.numeric(gsub(",","",table8_33$Count))

reason_frequency_plot <-    
  ggplot(table8_33, aes(x = reorder(Reason, Count), y = Count, fill = Reason)) +
    geom_col(colour = "black", fill = "#6279e1") +
      coord_flip() +
        labs(x = "Reason", y = "Frequency") +
          theme(legend.position = "none")
