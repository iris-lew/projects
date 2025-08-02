# Packages and libraries

# Package names
library(tidyverse)
library(plotly)
library(shiny)
library(forcats)


# load dataset
load("data/q24.rdata")

# Setup
morethan15 <- year_q24 %>%
  filter(total>=15) %>%
  distinct(how_met) %>% 
  pull() %>%
  unique() 

ui <- fluidPage(
  htmlOutput('UpdatedDate'),
  p(''),
  titlePanel("Most Common Methods for US Couples to Meet Throughout the Years"),
  fluidRow(
    column(width = 3,
           selectInput("how_met",
                       "How They Met", 
                       choices = unique(year_q24$how_met), 
                       multiple = TRUE,
                       selected = morethan15)),
    column(width = 3,
           selectInput("couple_status",
                       "Couple Status", 
                       choices = c("Married",
                                   "Unmarried Partners",
                                   "Singles But Had Past Partners"), 
                       multiple = TRUE,
                       selected = c("Married",
                                    "Unmarried Partners",
                                    "Singles But Had Past Partners")))
  ),
  plotOutput("plot"),
  htmlOutput('NAtext'),
  p(''),
  tableOutput('NAtable')
)