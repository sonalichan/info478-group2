# Load Packages
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)


# Load Data 10.2
table_10 <- read.csv("../data/table10_2_clean.csv")

# Clean Data 10.2
table_10 <- table_10 %>%
  rename(
    "factor" = "Geographic.Socioeconomic.Characteristic",
    "2008" = "X2008",
    "2009" = "X2009",
    "2010" = "X2010",
    "2011" = "X2011",
    "2012" = "X2012",
    "2013" = "X2013",
    "2014" = "X2014",
    "2015" = "X2015",
    "2016" = "X2016",
    "2017" = "X2017",
    "2018" = "X2018"
  ) %>% 
  filter(factor %in% c("Large Metro", 
                       "Small Metro", 
                       "Urbanized",
                       "Less Urbanized",
                       "Completely Rural"))

# Make Row 1 Column names
table_10_clean <- table_10[,-1]
rownames(table_10_clean) <- table_10[,1]

# Flip Rows and Columns
table_10_flipped <- as.data.frame(t(table_10_clean))

# Make Row names Column 1
setDT(table_10_flipped, keep.rownames = TRUE)[]
table_10_flipped <- table_10_flipped %>%
  rename("year" = "rn",
         "large_metro" = "Large Metro",
         "small_metro" = "Small Metro",
         "urbanized_nonmetro" = "Urbanized",
         "less_urbanized_nonmetro" = "Less Urbanized",
         "completely_rural" = "Completely Rural"
  )

# Change Values to Numbers
table_10_flipped$large_metro <- as.numeric(gsub(",", "", table_10_flipped$large_metro))
table_10_flipped$small_metro <- as.numeric(gsub(",", "", table_10_flipped$small_metro))
table_10_flipped$urbanized_nonmetro <- as.numeric(gsub(",", "", table_10_flipped$urbanized_nonmetro))
table_10_flipped$less_urbanized_nonmetro <- as.numeric(gsub(",", "", table_10_flipped$less_urbanized_nonmetro))
table_10_flipped$completely_rural <- as.numeric(gsub(",", "", table_10_flipped$completely_rural))


table_10_melt<- melt(table_10_flipped, id.vars = "year", variable.name = "county_type")


# Line Plot
line_plot <- ggplot(data = table_10_melt, mapping = aes(x = year, y = value)) +
  geom_point(aes(color = county_type)) + geom_line(aes(group = county_type, color = county_type)) +
  labs(
    title = "Recorded Presence of Mental Illness Among People 18 Years or Older",
    x = "Year",
    y = "Number of People"
  )

# Change Legend
line_plot <- line_plot + scale_color_manual(name = "County Type",
                                            labels = c("Large Metropolitan",
                                                       "Small Metropolitan",
                                                       "Urbanized Non Metropolitan",
                                                       "Less Urbanized Non Metropolitan",
                                                       "Completely Rural"),
                                            values = c("large_metro" = "#F8766D",
                                                       "small_metro" = "#C49A00",
                                                       "urbanized_nonmetro" = "#FB61D7",
                                                       "less_urbanized_nonmetro" = "#00C094",
                                                       "completely_rural" = "#A58AFF"))



# Load Data 8.2
table_8_2 <- read.csv("../data/table8_2_clean.csv")



# Clean Data 8.2

# Change Values into Numbers
table_8_2$Over.18.2017 <- as.numeric(gsub(",", "", table_8_2$Over.18.2017))
table_8_2$Over.18.2018 <- as.numeric(gsub(",", "", table_8_2$Over.18.2018))
table_8_2$Aged.18.25.2017 <- as.numeric(gsub(",", "", table_8_2$Aged.18.25.2017))
table_8_2$Aged.18.25.2018 <- as.numeric(gsub(",", "", table_8_2$Aged.18.25.2018))
table_8_2$Over.26.2017 <- as.numeric(gsub(",", "", table_8_2$Over.26.2017))
table_8_2$Over.26.2018 <- as.numeric(gsub(",", "", table_8_2$Over.26.2018))
table_8_2$Aged.26.49.2017 <- as.numeric(gsub(",", "", table_8_2$Aged.26.49.2017))
table_8_2$Aged.26.49.2018 <- as.numeric(gsub(",", "", table_8_2$Aged.26.49.2018))
table_8_2$Over.50.2017 <- as.numeric(gsub(",", "", table_8_2$Over.50.2017))
table_8_2$Over.50.2018 <- as.numeric(gsub(",", "", table_8_2$Over.50.2018))



table_8_2_ethnicity <- table_8_2 %>%
  mutate("over_18" = Over.18.2017 + Over.18.2018,
         "18_25" = Aged.18.25.2017 + Aged.18.25.2018,
         "over_26" = Over.26.2017 + Over.26.2018,
         "26_49" = Aged.26.49.2017 + Aged.26.49.2018,
         "over_50" = Over.50.2017 + Over.50.2018) %>% 
  rename("factor" = "Demographic.Characteristic") %>%
  select("factor", 
         "over_18",
         "18_25",
         "over_26",
         "26_49",
         "over_50") %>% 
  filter(factor %in% c("White", 
                       "Black or African American", 
                       "AIAN",
                       "NHOPI",
                       "Asian",
                       "Two or More Races",
                       "Hispanic or Latino"))

# Make Row 1 Column names
table_8_2_clean <- table_8_2_ethnicity[,-1]
rownames(table_8_2_clean) <- table_8_2_ethnicity[,1]

# Flip Rows and Columns
table_8_2_flipped <- as.data.frame(t(table_8_2_clean))

# Make Row names Column 1
setDT(table_8_2_flipped, keep.rownames = TRUE)[]
table_8_2_flipped <- table_8_2_flipped %>%
  rename("age" = "rn")

table_8_2_melt <- melt(table_8_2_flipped, id.vars = "age", variable.name = "ethnicity")


# Box Plot
box_plot <- ggplot(data = table_8_2_melt, mapping = aes(x = age, y = value, group = ethnicity)) +
  geom_boxplot(aes(color = ethnicity)) + 
  geom_jitter(width = 0.1, aes(color = ethnicity)) +
  labs(
    title = "Average Number of People with Any Mental Illness based on Ethnicity",
    x = "Age Group",
    y = "Number of People"
  )

# Change x axis labels and order
box_plot <- box_plot + scale_x_discrete(limits = c("over_18","18_25","over_26", 
                                                   "26_29", "over_50"),
                                        labels = c("over_18" = "18+", "18_25" = "18-25",
                                                   "over_26" = "26+", "26_29" = "26-29",
                                                   "over_50" = "50+"))

# Change legend
box_plot <- box_plot + scale_color_manual(name = "Ethnicity",
                                            labels = c("White",
                                                       "Black or African American",
                                                       "American Indian or Alaska Native",
                                                       "Native Hawaiian or Other Pacific Islander",
                                                       "Asian",
                                                       "Two or More Races",
                                                       "Hispanic or Latino"),
                                            values = c("White" = "#F8766D",
                                                       "Black or African American" = "#C49A00",
                                                       "AIAN" = "#53B400",
                                                       "NHOPI" = "#00C094",
                                                       "Asian" = "#00B6EB",
                                                       "Two or More Races" = "#A58AFF",
                                                       "Hispanic or Latino" = "#FB61D7"))



# Standard Deviations
sd_over_18 <- sd(table_8_2_clean$over_18, na.rm = TRUE)
sd_18_25 <- sd(table_8_2_clean$`18_25`, na.rm = TRUE)
sd_over_26 <- sd(table_8_2_clean$over_26, na.rm = TRUE)
sd_26_49 <- sd(table_8_2_clean$`26_49`, na.rm = TRUE)
sd_over_50 <- sd(table_8_2_clean$over_50, na.rm = TRUE)

