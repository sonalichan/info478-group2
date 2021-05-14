library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(reshape2)

table8_18 <- read.csv("table8_18_clean.csv")

table8_18_clean<- table8_18 %>%
  gather("Illness", "Frequency", -Characteristic)

ggplot(data=table8_18_clean, aes(x=Illness, y=Frequency, fill=Characteristic, main = "hi")) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Received Mental Health Services in Past Year by Education Levels",
              x ="Amount of Illness", y = "Frequency (thousands)")
