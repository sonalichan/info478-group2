# ----------- LOAD LIBRARIES ---------------
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyr)
library(data.table)
library(scales)

# ----------- STRUCTURE ---------------

# ----------- INTRODUCTION: SAMANTHA ---------------
introduction <- tabPanel(
  "Introduction",
  titlePanel("Mental Health Care Accessibility"),
  fluidRow(
    column(12, "Group 2: Sonali Chandra, John-Luke Dokupil, Samantha Chow")
  ),
  br(),
  sidebarLayout(             
    sidebarPanel( 
      h4("Dataset"),
      h5(a("2018 National Survey on Drug Use and Health Detailed Tables (SAMHSA)",
           href = "https://www.samhsa.gov/data/report/2018-nsduh-detailed-tables")),
      p("The results of the 2018 National Survey on Drug Use and Health are 
        described in detailed tables that present national estimates on varying 
        categories. There are tables displaying information from youths and 
        adults, and measures behaviors based on a variety of demographic and 
        geographic characteristics. There are also measures of prevalence of 
        mental health disorders, substance use, and availability of treatment.")
    ),           
    mainPanel( 
      h3("Purpose"),
      p("The purpose of this project is to better understand how accessible care 
        for mental health is in the United States. Though originally our group 
        wanted to focus on broad factors that affect one's access to general 
        healthcare, we found interesting research and data related specifically 
        to the breakdown of mental health care in the United States. We hope to 
        draw conclusions about mental health to hopefully reduce the stigma 
        associated with it, and to better understand where these services are 
        regularly utilized in hopes to bring more attention to the areas that 
        may not have as much access to mental health services."),
      h3("Working with the Data"),
      p("One of the initial difficulties we had while working on this project was
        finding the best and most relevant questions we would want to answer. 
        There were many possible questions we would want to look into due to the
        size and variability of the data. After deciding on our questions,
        we had to manually sift through all the data tables to find the most 
        relevant ones that covered the correct topics for us to focus on.
        Another challenge was that we were mostly presented with categorical
        variables and data points from this data, and we needed to create
        thoughtful visualizations from inflexible data points.")
    )),
  br(),
  fluidRow(
    column(12, 
           plotlyOutput("reason_frequency")),
    column(8, offset = 4,
           h3("Barriers Limiting Patient Access to Mental Healthcare")),
    column(8, offset = 4, 
           p("From our dataset, we compiled the above visualization to display
             the common excuses and reasonings for people not to seek care for
             their mental health. The two most frequent reasons were the cost
             of care and thinking one could handle the problem without treatment.
             According to the", a("National Alliance on Mental Illness (NAMI)",
             href = "https://www.nami.org/mhstats") , ", one in
             four people experience mental illness each year. Though so many
             people face mental illness, the access to mental healthcare is not
             the same as going to the doctor for a sore throat. According
             to" , a("Patient Engagement News",
             href = "https://patientengagementhit.com/news/key-barriers-limiting-patient-access-to-mental-healthcare") ,
             "there are four key barriers that are limiting mental healthcare access:")),
    column(8, offset = 4,
           h5("1. Mental Health Clinician Shortage")),
    column(8, offset = 4,
           p("As the nation faces a significant downturn on the number of 
             clinicians in general. There are limited numbers of qualified
             mental health professionals to meet demand. Patients sometimes have
             to travel far in order to visit a mental health clinician, and
             though telehealth has improved within the past few years, there are
             still limitations with that alternative.")),
    column(8, offset = 4,
           h5("2. Limited Mental Health Access Equality")),
    column(8, offset = 4,
           p("Patients often face challenges to pay for their care, especially
             when insurance does not always cover it. This makes it hard for
             patients to get service at an affordable price across the board.
             These large medical bills are one of the largest barriers to 
             refusing the search for help as out-of-network care is incredibly
             costly.")),
    column(8, offset = 4,
           h5("3. Fragmented Mental and Physical Health Access")),
    column(8, offset = 4,
           p("Mental healthcare has not fully integrated with physical healthcare
             offerings, and this discrepancy makes it difficult to understand
             how the patient should best proceed.")),
    column(8, offset = 4,
           h5("4. Social Stigma and Limited Awareness")),
    column(8, offset = 4,
           p("Patients often feel pressure from social stigma to avoid seeking
             mental health services. This stigma not only contributes to people
             not seeking their own help but also blocks societal awareness
             about mental health and the healthcare industry."))
    ))

# ----------- Q1: JOHN-LUKE ---------------
question_1 <- 
  tabPanel("Mental Health Across the US",
    titlePanel("How are mental health services being utilized across the United States?"),
    br(),
      sidebarLayout(             
        sidebarPanel( 
          selectInput("region_severity", 
                      label = h4("Severity of Mental Illness"),
                      choices = list("Any Mental Illness" = "any",
                                     "Serious Mental Illness" = "serious",
                                     "Any Mental Illness Excluding Serious" = "exclude_serious",
                                     "No Mental Illness" = "none"),
                                     selected = "any")
          ),           
        mainPanel(
          plotlyOutput("map"),
          br(),
          plotlyOutput("county_bar")
        )
    ),
    fluidRow(
      column(12,
             h3("Analysis")),
      column(12,
             p("Through the interactive geographical visualization, we can 
               observe the number of mental illnesses throughout the United 
               States. By contrasting the interactive map with the bar chart 
               visualizing the differences among county types we can make some 
               interesting connections. The first being that, as expected, large 
               metro makes up a large amount of the numbers, regardless of the 
               percentage of mental illness. However, the mental health ratio 
               (no mental illness divided by any mental illness) in the large 
               metro is 0.89, and the mental health ratio of nonmetro is 0.78. 
               A possible inference that could be made is that in a large 
               metropolitan county there are more resources available. Regions 
               that tend towards one trend, whether that be a tendency to more 
               mental illness or less, will in general follow that trend in the 
               other direction. In the same way, the Northeast region of the 
               United States has the most people with a mental illness, but also 
               one of the most people with no mental illness. A different 
               approach that could be made with this observation is that the 
               Northeast region utilizes mental health services the least. This 
               could explain why the region has both a great number of people 
               both with and without mental illness.")),
      column(12,
             p("Does a high mental health ratio infer an adequate or inadequate 
             utilization of mental health services? It's difficult to come to a 
             solid conclusion. Due to limitations of our dataset, our data is per 
             geographical region rather than by state. This greatly broadened 
             the scope of our data greater than we would have preferred. To 
             further this research, we would find more precise data concerning 
             each state. This would allow us to make deeper connections to other 
             aspects of society that factor into the number of mental illnesses 
             in the United States.")),
      )
    )

# ----------- Q2: SAMANTHA ---------------
question_2 <- tabPanel(
  "Types of Services Received",            
  titlePanel("What types of mental health services are received based on the level of mental illness?"),
  br(),
  sidebarLayout(             
    sidebarPanel( 
      selectInput("severity", label = h4("Severity of Mental Illness"),
                  choices = list("Any Mental Illness" = "any",
                                 "Serious Mental Illness" = "serious",
                                 "Any Mental Illness Excluding Serious" = "exclude_serious",
                                 "No Mental Illness" = "none"),
                  selected = "any"),
      radioButtons("insurance", label = h4("Insurance Type"),
                   choices = list("Private" = "private",
                                  "Medicaid/CHIP" = "medicaid",
                                  "Other" = "other",
                                  "No Coverage" = "none"),
                   selected = "private")
    ),           
    mainPanel(                
      plotlyOutput("bar_chart")
    )),
  fluidRow(
    column(12,
           h3("Analysis")),
    column(12,
           p("This interactive bar chart focuses on what types of mental health
             services are received, based on the severity of one's mental
             illness and the type of insurance they have. The three services from 
             the survey that people sought were inpatient, outpatient, and 
             prescription medications. These two factors
             (severity and insurance type) can shed some light on when a user
             decides to seek help, versus trying to figure out everything on 
             their own. Knowing what kind of insurance or whether a patient
             is even covered by insurance addresses some of the inequalities 
             seen from distribution of care. By understanding what factors are 
             indicative of requiring mental health care, this could pinpoint 
             what services one might seek based on what they are able to afford 
             or what kind of issues they are experiencing.")),
    column(12,
           p("As expected, those who face serious mental illness have a higher
             percentage of mental health service usage than those who face
             more mild or no mental illness. Those who are insured with Medicaid
             have the highest chance of seeking help, while those without
             coverage at all seek very minimal help, probably due to the high
             cost of out-of-network medical bills. Throughout all the categories
             and combinations of a patient's external factors, the service that 
             is sought the most is prescription medication, probably due to 
             the high time commitment that comes along with inpatient services.
             Outpatient services are a middle ground in terms of time 
             commitment, but it may be difficult to find a proper mental 
             health professional to see to the patients needs. More on types
             of mental health professionals seen are shown below.")),
    column(12, p ("")),
    column(12,
           plotlyOutput("professionals_chart")),
    column(12,
           h3("Analysis")),
    column(12,
           p("We also wanted to get a better grasp of the types of professionals
             commonly seen for outpatient care. With this knowledge, communities 
             could be better equipped with enough of the proper professionals to 
             provide services to those who need it.")),
    column(12,
           p("This pie chart shows that the top two types of professionals
             seen are general practitioners and psychiatrists/psychologists.
             Though there is a majority, one or two types of professionals 
             are not overwhelmingly taking over the mental health services 
             industry. With this data spread out in this way, it goes to 
             show how necessary it is to have an adequate amount of clinicians
             of every type, as many patients seek a variety of different 
             professionals. It also shows how care might be seen as fragmented
             due to the poor integration of general healthcare and mental
             healthcare."))
  ))

# ----------- Q3: SONALI ---------------
question_3 <- tabPanel(
  "Prevalence of Mental Health",             #title of the page, what will appear as the tab name
  titlePanel("What is the prevalence of mental health issues in certain communities?"),
  br(),
  sidebarLayout(             
    sidebarPanel( 
      # left side of the page 
      # insert widgets or text here -- their variable name(s), NOT the raw code
      selectInput("illness", label = h4("Severity of Mental Illness"),
                  choices = list("Any Mental Illness" = "any",
                                 "Serious Mental Illness" = "serious",
                                 "Any Mental Illness Excluding Serious" = "exclude_serious",
                                 "No Mental Illness" = "none"),
                  selected = "any")
    ),        
    mainPanel(                
      plotlyOutput("dem_bar_chart"),
      h6("Make sure to press the 'Autoscale' button to see the differences better!")
    )),
  fluidRow(
    column(12,
           h3("Analysis")),
    column(12,
           p("This interactive bar chart focuses on the levels of mental health illness compared across different ethnic groups. Respondents are 
             categorized into
             'Not Hispanic or Latino', 'White', 'American Indian or Alaskan Native (AIAN)', 
             'Native Hawaiian or Other Pacific Islander (NHOPI)',
              'Asian', 'Two or More Races', and 'Hispanic or Latino'. 
              You are able to filter by the levels of the respondents' reported mental illness,
              including 'Any mental illess', 'Serious mental illness', 
              'Any mental illness excluding serious', and 'No mental illness'.
              These comparisons can hopefully shed some light on which ethnic groups are comfortable enough
              to report the presence of mental illness in their lives, due to lack of stigma and many other
              factors.")),
    column(12,
           p("For every one of the conditions indicating a presence of mental illness,
              Asians have the least amount of respondents, while their percentage for the 'None'
              category is the highest. This could be due to a variety of factors, but perpetuating stigma
              surrounding mental health in different Asian cultures and placing importance on being the 
              'Model Minority' may have a lot to do with why this is the case.")),
    column(12,
           p("On the other hand, respondents in the AIAN or Two or More Races categories generally report
              the highest prevalence of mental health issues. In the case for AIAN, history of oppression, 
              discrimination, and removal from traditional lands experienced by Native people has contributed 
              to their significant representation among populations with high need for mental health care. They
              have also been reported to categorize mental health differently than the standard US definition.")),
    column(12,
           p("Overall, all of these populations have high needs for mental healthcare, but this notion is expressed 
              differently based on very personal factors. This graph can contribute to a visual understanding of 
              the differences while allowing viewers to think about why the respondents are more than just numbers in
              a data table.")
              )))
# ----------- CONCLUSION: SONALI ---------------
conclusion <- tabPanel(
  "Conclusion / Sources",             #title of the page, what will appear as the tab name
  titlePanel("Conclusion and Insights"),
  fluidRow(
    column(12, h3("Summary of Findings")),
    column(12, p("** @sonali ** blah blah")),
    column(12, h3("Limitations")),
    column(12, p("** @sonali ** add more here about the limitations of the data... along
                 the lines:
                 A challenge of data collection that is more specific to our 
                 project is a misrepresentation of the populations being 
                 surveyed. One factor of low healthcare access is often low 
                 connectivity and these groups might not be represented in the 
                 data that is presented on online sources.")),
    column(12, p ("")),
    column(12, h3("Further Research")),
    column(12, h5(a("Cohen Veteran’s Network’s America’s Mental Health 2018 Study",
                    href = "https://www.cohenveteransnetwork.org/americasmentalhealth/"))),
    column(12, p("Ketchum Analytics helped the Cohen Veteran’s Network and the 
                 National Council for Behavioral Health gain a better 
                 understanding of the current attitudes and access to mental 
                 health services for Americans. They took an online survey of 
                 5,000 Americans, representative of the US population based on 
                 age, gender, region, household income, and race/ethnicity. They 
                 found that low healthcare access, including high cost and 
                 insufficient insurance coverage, limited options and long 
                 waits, and lack of awareness all contribute to insufficient 
                 mental health services, which in turn affects mental health 
                 outcomes.")),
    column(12, h5(a("Common Mental Health Disorders: Identification and Pathways to Care",
                    href = "https://www.ncbi.nlm.nih.gov/books/NBK92265/"))),
    column(12, p("The British Psychological Society & The Royal College of 
                 Psychiatrists did a study on mental health disorders. Section 4
                 of this study focused on healthcare access, and the effects it 
                 had on mental health. The study found that in addition to the 
                 factors that affect healthcare access for all individuals, 
                 there should be considered additional factors regarding certain 
                 vulnerable groups (the study chose to focus on minority ethnic 
                 groups and elderly people). There were three listed factors of 
                 healthcare access: individual-level factors, practitioner-level 
                 factors, and resource-based/practical factors. Each factor 
                 being broken down by: general population, minority ethnic 
                 groups, and elderly people.")),
    column(12, h5(a("Impact of residential displacement on healthcare access and 
                     mental health among original residents of gentrifying 
                     neighborhoods in New York City",
                    href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0190139"))),
    column(12, p("The goal of this study was to make quantitative assessments of
                 the health impacts of socioeconomic displacement in New York 
                 City. The study found that adults who moved into 
                 non-gentrifying, poor neighborhoods had a higher number of ED 
                 visits, hospitalizations, and mental health-related visits for 
                 about 5 years after displacement. The study linked these 
                 findings with the negative impacts that residential 
                 displacement had on healthcare access and mental health."))
  ))


# ----------- DEFINE UI AND TABS ---------------
ui <- navbarPage(
  "Mental Health Care Accessibility",
  introduction,
  question_1,
  question_2,
  question_3,
  conclusion
)
