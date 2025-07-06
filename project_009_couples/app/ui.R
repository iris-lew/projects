# Packages and libraries

# Package names
library(tidyverse)
library(plotly)
library(shiny)
library(forcats)


# load dataset
load("data/HCMST 2017 to 2022 small public version 2.2.rdata")
d <- `HCMST small public version 2.2`
colnames(d) <- tolower(colnames(d))

q24_cols <- colnames(d)[grep("w1_q24_",colnames(d))][c(17:37)]
# year they met, couple status, how they met
cols <- c("w1_q21a_year","w1_partnership_status", q24_cols) 

d_q24 <- d[,cols]

year_q24 <- pivot_longer(
  data = d_q24,
  cols = c(q24_cols)
) %>% group_by(w1_q21a_year,w1_partnership_status,name) %>%  
  summarise(total = sum(value, na.rm = TRUE), .groups = "keep") %>%
  #### renamed columns
  mutate(name = recode(name,
                       w1_q24_bar_restaurant = 'Bar or Restaurant',
                       w1_q24_college = 'College',
                       w1_q24_internet_dating =  'Internet Dating',
                       w1_q24_internet_other = 'Other Internet',
                       w1_q24_met_online = 'Met Online',
                       w1_q24_party = 'Party',
                       w1_q24_school = 'School',
                       w1_q24_blind_date = "Blind Date",
                       w1_q24_business_trip = "Business Trip",
                       w1_q24_church = "Place of Worship",
                       w1_q24_customer = "Customer",
                       w1_q24_internet_chat = "Internet Chat",
                       w1_q24_internet_game = 'Internet Game',
                       w1_q24_internet_org = 'Internet Organization',
                       w1_q24_internet_soc_network = 'Internet Social Network',
                       w1_q24_mil = 'Military',
                       w1_q24_public = 'Public',
                       w1_q24_singles_serve_nonint = 'Single Service (Not Internet)',
                       w1_q24_vacation = 'Vacation',
                       w1_q24_vol_org = 'Volunteer Organization',
                       w1_q24_work_neighbors = 'Work Neighbors'),
         w1_partnership_status = recode(w1_partnership_status,
                                        '1' = 'Married',
                                        '2' = 'Unmarried Partners',
                                        '3' = 'Singles But Had Past Partners',
                                        '4' = 'Single And Never Had Partners'))

### Filter for those where more than 10 couples met
morethan10 <- year_q24 %>%
  filter(total>10) %>%
  distinct(name) %>% 
  pull() %>%
  unique() 



ui <- fluidPage(
  fluidRow(
    column(width = 3,
           selectInput("how_met",
                       "How They Met", 
                       choices = unique(year_q24$name), 
                       multiple = TRUE,
                       selected = morethan10)),
    column(width = 3,
           selectInput("couple_status",
                       "Couple Status", 
                       choices = factor(unique(year_q24$w1_partnership_status),
                                        levels = c("Married",
                                                   "Unmarried Partners",
                                                   "Singles But Had Past Partners",
                                                   "Single And Never Had Partners")), 
                       multiple = TRUE,
                       selected = unique(year_q24$w1_partnership_status)),)
  ),
  plotOutput("plot")
)

