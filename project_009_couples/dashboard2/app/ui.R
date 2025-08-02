# Packages and libraries

# Package names
library(tidyverse)
library(plotly)
library(data.table)

# Setup
user <- c("School","College", "Military","Church",
          "Volunteer Organization",
          "Customer", "Bar or Restaurant", "Party", "Internet (Other)", 
          "Internet Dating or Phone Apps",
          "Internet (Social Network)","Online Gaming","Internet (Chat)",
          "Internet Site Not Mainly Dedicated to Dating","Public","Blind Date",
          "Vacation","Non Internet Single Service","Business Trip",
          "Work Neighbors", "Online",
          "Family","Friend","Neighbors",
          "Coworkers","Online Excluding Phone Apps","Never Dated")


# Selection for how they met
# Selection for LGBTQ+ COUPLES
# Selection for whether at least one of the members are unhappy
# Selection for whether woman is unhappy

ui <- fluidPage(
  htmlOutput('UpdatedDate'),
  p(''),
  titlePanel("How US Couples Meet and Whether They Stay Together"),
  fluidRow(
    column(width = 3,
           selectInput("how_met",
                       "How They Met", 
                       choices = c("All",user)), 
                       multiple = FALSE,
                       selected = "All"),
    column(width = 3,
           selectInput("LGB_couple",
                       "LGB Couple?", 
                       choices = c("All Couples","LGB Couple", "Straight Couple")), 
           multiple = FALSE,
           selected = "All"),
    column(width = 3,
           selectInput("breakup",
                       "Who Wanted to Break Up?", 
                       choices = c("All Couples",
                                   "One party wanted to end it", 
                                   "A woman ended it",
                                   "A man ended it", 
                                   "Both wanted to end it")), 
           multiple = FALSE,
           selected = "All"),
    column(width = 3,
           sliderInput("YearMet",
                       "Year",
                       min = 1939,
                       max = 2021,
                       value = c(1939,2021),
                       timeFormat="%Y"),
           checkboxInput("NAyear", "Keep Year NA?", value = TRUE, width = NULL)
    ),
  ),
  htmlOutput('Multiple'),
  p(''),
  htmlOutput('ZeroText'),
  p(''),
  plotOutput('MetThroughTime'),
  p(''),
  plotlyOutput("sankey"),
  htmlOutput('MissedText')
)