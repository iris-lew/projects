# Packages and libraries

# Package names
library(tidyverse)
library(plotly)
library(shiny)
library(forcats)

# Load dataset
load("data/q24.rdata")

# Prep
DateUpdated <- Sys.Date()

# actual server function
function(input, output, session){
  # Date
  output$UpdatedDate <- renderText(paste0('Last Updated: ',DateUpdated))
  
  #Setting up data tables to be reactive to inputs
  data_noNA <- reactive({
    year_q24 %>% 
      dplyr::filter(how_met %in% input$how_met & 
                      partnership_status  %in% input$couple_status &
                      !is.na(year))
  })
  
  data_NA <- reactive({
    year_q24 %>% 
      dplyr::filter(how_met %in% input$how_met & 
                      partnership_status  %in% input$couple_status &
                      is.na(year) &
                      total>0) %>%
      ungroup() %>%
      select(partnership_status,how_met,total)
  })
  
  # Plots and Tables
  output$plot <- renderPlot({
    ggplot(data = data_noNA(), aes(x=year, y=total)) + 
      geom_line(aes(colour=how_met)) +
      theme_bw(base_size = 16) +
      labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met") +
      scale_y_continuous(limits = c(0,40),
                         breaks = seq(0, 40, by=10)) +
      scale_x_continuous(limits = c(1935,2020),
                         breaks = seq(1935, 2020, by=10)) 
  })
  
  NAtext_conditional <- reactive({
    ifelse(nrow(data_NA())>0,
           paste0('The following ',
                  sum(data_NA()$total),
                  ' data points are not displayed due to missing years in the publicly available dataset.' ),
           '')
  })
  
  output$NAtext <- renderText(NAtext_conditional())
  output$NAtable <- renderUI({
    req(nrow(data_NA())>0)
    output$NAtable2 <- renderDataTable(data_NA())
    DT::DTOutput("NAtable2")
  })
}