# ----------- LOAD LIBRARIES ---------------

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyr)
library(data.table)
library(scales)

library("maps")
library("mapdata")

# ----------- READ IN DATA ---------------

<<<<<<< HEAD
=======
# ----------- SAMANTHA -------------------
>>>>>>> main
inpatient_table <- read.csv("./data/table8_19_clean.csv", fileEncoding="UTF-8-BOM")
outpatient_table <- read.csv("./data/table8_20_clean.csv", fileEncoding="UTF-8-BOM")
prescription_table <- read.csv("./data/table8_21_clean.csv", fileEncoding="UTF-8-BOM")
professionals_table <- read.csv("./data/table8_52_a_clean.csv", fileEncoding="UTF-8-BOM")
<<<<<<< HEAD
types_combined <- read.csv("./data/types_of_service_combined.csv" ,fileEncoding="UTF-8-BOM")
table8_33 <- read.csv("./data/table8_33_clean.csv", fileEncoding="UTF-8-BOM")[2:16,]
table8_18 <- read.csv("data/table8_18_clean.csv", fileEncoding="UTF-8-BOM")
=======
types_combined <- read.csv("./data/types_of_service_combined.csv", fileEncoding="UTF-8-BOM")
table8_33 <- read.csv("./data/table8_33_clean.csv", fileEncoding="UTF-8-BOM")[2:16,] %>%
  rename("2017" = X2017, "2018" = X2018)
# ----------------------------------------
>>>>>>> main

# ----------- SONALI ---------------------
demographic_mental_illness <- read.csv("./data/table8_7_b.csv", fileEncoding="UTF-8-BOM")
dem_df <- data.frame()

# ----------------------------------------

# ----------- CLEAN DATA -----------------

# ----------- SAMANTHA -------------------

# Reason Frequency Bar Chart
table8_33 <- rename(table8_33, "2017" = X2017, "2018" = X2018)
table8_33$Count <- as.numeric(gsub(",","",table8_33$Count))


# Professionals Type Pie Chart
professionals_table <- professionals_table %>%
  rename(
    "type_of_professional" = "Type.of.Professional",
    "over_18" = "Aged.18.",
    "18_25" = "Aged.18.25",
    "over_26" = "Aged.26.",
    "26_49" = "Aged.26.49",
    "over_50" = "Aged.50.") %>% 
  filter(type_of_professional %in% c("General Practitioner or Family Doctor",
                                     "Other Medical Doctor",
                                     "Psychologist",
                                     "Psychiatrist or Psychotherapist",
                                     "Social Worker",
                                     "Counselor",
                                     "Other Mental Health Professional",
                                     "Nurse, Occupational Therapist, or Other Health Professional",
                                     "Religious or Spiritual Advisor",
                                     "Herbalist, Chiropractor, Acupuncturist, or Massage Therapist"))
professionals_table$over_18 <- as.numeric(gsub(",", "", professionals_table$over_18))

<<<<<<< HEAD
# Mental Illness by Region Chart
regional_table <- 
  table8_18[3:6,] %>% 
    rename("region" = X, 
           "any_mental_illness" = Any.Mental.Illness,
           "serious_mental_illness" = Serious.Mental.Illness,
           "any_mental_illness_excluding_serious_mental_illness" = Any.Mental.Illness.Excluding.Serious.Mental.Illness,
           "no_mental_illness" = No.Mental.Illness,
           "total" = Total)
regional_table$any_mental_illness <- as.numeric(gsub(",","", regional_table$any_mental_illness))
regional_table$serious_mental_illness <- as.numeric(gsub(",","", regional_table$serious_mental_illness))
regional_table$any_mental_illness_excluding_serious_mental_illness <- as.numeric(gsub(",","", regional_table$any_mental_illness_excluding_serious_mental_illness))
regional_table$no_mental_illness <- as.numeric(gsub(",","", regional_table$no_mental_illness))
regional_table$total <- as.numeric(gsub(",","", regional_table$total))

state_df <- data.frame("state" = c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming"),
                       "region" = c("South", "West", "West", "South", "West", "West", "Northeast", "South", "South", "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast", "Northeast", "South", "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West"))

regional_table <- 
  left_join(regional_table, state_df, by = "region") %>% 
    rename("regions" = region, "region" = state)

state_shape <- 
  map_data("state") %>%
    left_join(regional_table, by = "region")

=======
# ---------------------------------------------

# ----------- SONALI --------------------------
demographic_mental_illness <- demographic_mental_illness %>%
  rename(
    "dem_char" = "Demographic.Characteristic",
    "any_2017" = "Any.2017",
    "any_2018" = "Any.2018",
    "serious_2017" = "Serious.2017",
    "serious_2018" = "Serious.2018",
    "excluding_2017" = "Excluding.2017",
    "excluding_2018" = "Excluding.2018",
    "none_2017" = "None.2017",
    "none_2018" = "None.2018"
  ) %>%
  slice(-c(1:6)) %>%
  filter(dem_char %in% c("TOTAL",
                         "18-25",
                         "26 or Older",
                         "26-49",
                         "50 or Older",
                         "Not Hispanic or Latino",
                         "White",
                         "Black",
                         "AIAN",
                         "NHOPI",
                         "Asian",
                         "Two or More Races",
                         "Hispanic or Latino"))
demographic_mental_illness$none_2017 <- as.numeric(gsub("a", "", demographic_mental_illness$none_2017))

# ---------------------------------------------

dem_df_flipped <- as.data.frame(t(demographic_mental_illness))
dem_df_flipped <- dem_df_flipped %>%
  filter(dem_char %in% c("any_2017", "any_2018",
                          "serious_2017", "serious_2018",
                          "excluding_2017", "excluding_2018",
                          "none_2017", "none_2018"))


# ----------- DEFINE THE SERVER ---------------

server <- function(input, output) {
  
#--------------- SAMANTHA ---------------------
  # Reason Frequency Bar Chart
  output$reason_frequency <- renderPlotly({
    ggplot(table8_33, aes(x = reorder(Reason, Count), y = Count, fill = Reason)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Detailed Reasons for Not Receving Mental Health Services",
        x = "Reason", 
        y = "Frequency") +
      theme(legend.position = "none")
  })
  
  # Regional Map
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
  
  output$map <- renderPlotly({
    if(input$region_severity == "any")
    {
      ggplot(state_shape) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = any_mental_illness),
                     color = "white", # show state outlines
                     size = .1) +
          coord_map() +
            scale_fill_continuous(low = "Blue", high = "Red") +
              labs(fill = "any_mental_illness") +
                blank_theme
    }
    else if(input$region_severity == "serious")
    {
      ggplot(state_shape) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = serious_mental_illness),
                     color = "white", # show state outlines
                     size = .1) +
          coord_map() +
            scale_fill_continuous(low = "Blue", high = "Red") +
              labs(fill = "serious_mental_illness") +
                blank_theme
    }
    else if(input$region_severity == "exclude_serious")
    {
      ggplot(state_shape) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = any_mental_illness_excluding_serious_mental_illness),
                     color = "white", # show state outlines
                     size = .1) +
          coord_map() +
            scale_fill_continuous(low = "Blue", high = "Red") +
              labs(fill = "any_mental_illness_excluding_serious_mental_illness") +
                blank_theme
    }
    else if(input$region_severity == "none")
    {
      ggplot(state_shape) +
        geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = no_mental_illness),
                     color = "white", # show state outlines
                     size = .1) +
          coord_map() +
            scale_fill_continuous(low = "Blue", high = "Red") +
              labs(fill = "no_mental_illness") +
                blank_theme
    }
  })
  
  # Service Type Bar Chart
  output$bar_chart <- renderPlotly({
    
    if (input$severity == "any") {
      service_df <- types_combined %>%
        select("demographic_characteristic",
               "any_inpatient",
               "any_outpatient",
               "any_prescription")
    } else if (input$severity == "serious") {
      service_df <- types_combined %>%
        select("demographic_characteristic",
               "serious_inpatient",
               "serious_outpatient",
               "serious_prescription")
    } else if (input$severity == "exclude_serious") {
      service_df <- types_combined %>%
        select("demographic_characteristic",
               "exclude_serious_inpatient",
               "exclude_serious_outpatient",
               "exclude_serious_prescription")
    } else if (input$severity == "none") {
      service_df <- types_combined %>%
        select("demographic_characteristic",
               "none_inpatient",
               "none_outpatient",
               "none_prescription")
    }
    
    if (input$insurance == "private") {
      service_df <- service_df %>%
        filter(demographic_characteristic == "Private")
    } else if (input$insurance == "medicaid") {
      service_df <- service_df %>%
        filter(demographic_characteristic == "Medicaid/CHIP")
    } else if (input$insurance == "other") {
      service_df <- service_df %>%
        filter(demographic_characteristic == "Other3")
    } else if (input$insurance == "none") {
      service_df <- service_df %>%
        filter(demographic_characteristic == "No Coverage")
    }
    
    
    service_df_flipped <- as.data.frame(t(service_df))
    setDT(service_df_flipped, keep.rownames = TRUE)[]
    service_df_flipped <- service_df_flipped %>%
      rename("service_type" = "rn", "percentage" = "V1") %>% 
      filter(service_type %in% c("any_inpatient", "any_outpatient", "any_prescription",
                                 "serious_inpatient", "serious_outpatient", "serious_prescription",
                                 "exclude_serious_inpatient", "exclude_serious_outpatient", "exclude_serious_prescription",
                                 "none_inpatient", "none_outpatient", "none_prescription"))
    
    service_df_flipped$percentage <- as.numeric(service_df_flipped$percentage)
      
    ggplot(service_df_flipped, aes(x = service_type, y = percentage)) +
      geom_bar(stat = "identity", fill = "#9468bd") +
      labs(
        title = "Percentage of Service Usage based on Mental Illness Severity",
        x = "Service Type",
        y = "Percentage of Usage (%)") +
      ylim(0, 100) +
      scale_x_discrete(labels = c("Inpatient",
                                  "Outpatient",
                                  "Prescription"))
  })
  
  # Professionals Type Pie Chart
  output$professionals_chart <- renderPlotly({
    plot_ly(professionals_table, labels = ~type_of_professional, values = ~over_18,
            type = 'pie') %>% 
      layout(title = "Types of Professionals Seen") 
  })
# -----------------------------------------------------
  
# ------------------ SONALI ---------------------------
  
  # Service Type Bar Chart
  output$dem_bar_chart <- renderPlotly({
    
    if (input$illness == "Any Mental Illness") {
      dem_df <- demographic_mental_illness %>%
        select("dem_char",
               "any_2017",
               "any_2018")
    } else if (input$illness == "Serious Mental Illnesses") {
      dem_df <- demographic_mental_illness %>%
        select("dem_char",
               "serious_2017",
               "serious_2018")
    } else if (input$illness == "Any Mental Illnesses Excluding Serious") {
      dem_df <- demographic_mental_illness %>%
        select("dem_char",
               "excluding_2017",
               "excluding_2018")
    } else if (input$illness == "No Mental Illnesses") {
      dem_df <- demographic_mental_illness %>%
        select("dem_char",
               "none_2017",
               "none_2018")
    }
    
    
    if (input$age == "18.25") {
      dem_df <- dem_df %>%
        filter(dem_df$dem_char == "18-25")
    } else if (input$age == "26") {
      dem_df <- dem_df %>%
        filter(dem_df$dem_char == "26 or Older")
    } else if (input$age == "26.49") {
      dem_df <- dem_df %>%
        filter(dem_df$dem_char == "26-49")
    } else if (input$age == "50") {
      dem_df <- dem_df %>%
        filter(dem_df$dem_char == "50 or Older")
    }
    
    dem_df_flipped <- as.data.frame(t(dem_df))
    setDT(dem_df_flipped, keep.rownames = TRUE)[]
    dem_df_flipped <- dem_df_flipped %>%
      filter(rn %in% c("Not Hispanic or Latino", "White",
                                 "AIAN", "NHOPI",
                                 "Asian", "Two or More Races",
                                 "Hispanic or Latino"))
    dem_df_flipped$V1 <- as.numeric(dem_df_flipped$V1)
    
    plotted <- ggplot(dem_df_flipped, aes(x = rn, y = V1)) +
      geom_bar(stat = "identity", fill = "#9468bd") +
      labs(
        title = "Percentage of Service Usage based on Mental Illness Severity",
        x = "Service Type",
        y = "Percentage of Usage (%)") +
      ylim(0, 100) 
    plotted <- ggplotly(plotted)
  })
# -----------------------------------------------------
}
