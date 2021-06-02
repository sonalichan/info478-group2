# ----------- LOAD LIBRARIES ---------------

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyr)
library(data.table)
library(scales)

# ----------- READ IN DATA ---------------

inpatient_table <- read.csv("./data/table8_19_clean.csv")
outpatient_table <- read.csv("./data/table8_20_clean.csv")
prescription_table <- read.csv("./data/table8_21_clean.csv")
professionals_table <- read.csv("./data/table8_52_a_clean.csv")
types_combined <- read.csv("./data/types_of_service_combined.csv")
table8_33 <- read.csv("./data/table8_33_clean.csv")[2:16,] %>%
  rename("2017" = X2017, "2018" = X2018)


# ----------- CLEAN DATA ---------------

# Reason Frequency Bar Chart
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



# ----------- DEFINE THE SERVER ---------------

server <- function(input, output) {
  
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
  
}