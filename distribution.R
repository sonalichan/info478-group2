library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(reshape2)

# Table 8.3
table8_3 <- read.csv("table8_3_clean.csv")

table8_3 <- table8_3 %>% rename(
  "18" = "X18",
  "18-25" = "X18.25",
  "26" = "X26",
  "26-49" = "X26.49",
  "50" = "X50")

table8_3_clean <- table8_3 %>%
  gather("Age", "Frequency", -Characteristic)

any <- ggplot(data=table8_3_clean, aes(x=Age, y=Frequency, fill=Characteristic)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Any Mental Health Illnesses in 2017 and 2018 by Education Levels",
       x ="Age", y = "Frequency (thousands)")


#Table 8.18
table8_18 <- read.csv("table8_18_clean.csv")


table8_18_clean<- table8_18 %>%
  gather("Illness", "Frequency", -Characteristic)

received <- ggplot(data=table8_18_clean, aes(x=Illness, y=Frequency, fill=Characteristic, main = "hi")) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Received Mental Health Services in 2017 and 2018 by Education Levels",
              x ="Amount of Illness", y = "Frequency (thousands)")


## Analysis for Bar Plot
# These graphs show the number of people (in thousands) over 18 years old who have any mental illnesses compared to
# those who had received mental health services in 2017 and 2018 combined. Any refers to any mental illnesses reported,
# Serious refers to serious mental illnesses reported, Excluding reders to any mental illnesses
# excluding serious mental illnesses, and None refers to those with no mental illnesses recorded.
# The dataset was filtered through geographic/socioeconomic characteristics, so we
# chose to look at the relationship between education levels and those getting mental health help in the US. 
# The graphs generally show that more people reported mental illnesses than the amount of people getting
# help, but we can see that those with college or associates degrees have the largest amount of mental illnesses, yet
# they're the group with the most number of people that got help. We can also use this data by mapping it out
# and comparing it to areas of the US with high and low education levels to make inferences about where access to mental 
# health services are the highest and lowest.




