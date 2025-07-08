load("~/HCMST 2017 to 2022 small public version 2.2.rdata")
d <- `HCMST small public version 2.2`
colnames(d) <- tolower(colnames(d))

# Packages and libraries

# Package names
packages <- c("tidyverse","plotly",'shiny','forcats',"DT","shinylive","httpuv")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Q24

q24_cols <- colnames(d)[grep("w1_q24_",colnames(d))][c(17:37, 40:44)]
cols <- c("w1_q21a_year","w1_partnership_status", q24_cols) # year they met, how they met

d_q24 <- d[,cols]
sum(d_q24$w1_q24_bar_restaurant,na.rm = T)

year_q24 <- pivot_longer(
  data = d_q24,
  cols = c(q24_cols)
) %>% group_by(w1_q21a_year,w1_partnership_status,name) %>%  
  summarise(total = sum(value, na.rm = TRUE), .groups = "keep")

# verifying the pivot is correct.
for (i in 1:length(q24_cols)){
  print(q24_cols[i])
  print(sum(year_q24[year_q24$name==q24_cols[i],]["total"],na.rm = T)==sum(d[[q24_cols[i]]],na.rm=T))
  print(sum(year_q24[year_q24$name==q24_cols[i],]["total"],na.rm = T))
}

# add in the renamed columns
year_q24 <- year_q24 %>%
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
                       w1_q24_work_neighbors = 'Work Neighbors',
                       w1_q24_met_through_family = 'Met Through Family',
                       w1_q24_met_through_friend = 'Met Through Friend',
                       w1_q24_met_through_as_nghbrs = 'Met As Neighbors',
                       w1_q24_met_as_through_cowork = 'Met As Coworkers',
                       w1_q24_metonline_no_phone_apps = 'Met Online (no phone apps)'),
         w1_partnership_status = recode(w1_partnership_status,
                                        '1' = 'Married',
                                        '2' = 'Unmarried Partners',
                                        '3' = 'Singles But Had Past Partners',
                                        '4' = 'Single And Never Had Partners'))


year_q24 %>%
  group_by(w1_partnership_status) %>%
  summarise(tot = sum(total, na.rm=T))

# Remove single and never had partners from visualization
year_q24 <- year_q24[year_q24$w1_partnership_status!="Single And Never Had Partners",]

lookup <- c(year = "w1_q21a_year", how_met = "name", partnership_status = "w1_partnership_status")
year_q24 <- rename(year_q24, all_of(lookup))
year_q24
# save(year_q24, file="./data/q24.rdata")
# load("data/q24.rdata")


year_q24[is.na(year_q24$total),]
unique(year_q24[["total"]])
max(year_q24[["total"]])

plot <- ggplot(data = year_q24, aes(x=year, y=total)) +
  geom_line(aes(colour=how_met)) +
  labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met") +
  ggtitle("Most Common Methods for Couples to Meet Throughout the Years") +
  scale_y_continuous(limits = c(0,50),
                     breaks = seq(0, 50, by=10))


### Too many lines.

### Filter for those where more than 10 couples met

morethan10 <- year_q24 %>%
  filter(total>=10) %>%
  distinct(how_met) %>% 
  pull() %>%
  unique() 

morethan15 <- year_q24 %>%
  filter(total>=15) %>%
  distinct(how_met) %>% 
  pull() %>%
  unique() 

ui <- fluidPage(
  htmlOutput('UpdatedDate'),
  p(''),
  titlePanel("Most Common Methods for Couples to Meet Throughout the Years"),
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
                       choices = factor(unique(year_q24$partnership_status),
                                        levels = c("Married",
                                                   "Unmarried Partners",
                                                   "Singles But Had Past Partners",
                                                   "Single And Never Had Partners")), 
                       multiple = TRUE,
                       selected = unique(year_q24$partnership_status)),)
  ),
  plotOutput("plot"),
  htmlOutput('NAtext'),
  p(''),
  tableOutput('NAtable')
)

server <- function(input, output, session){
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

shinyApp(ui = ui, server = server)

### testing what was going wrong with my app
year_q24 %>%
  dplyr::filter(w1_partnership_status=="Married" &
                  is.na(w1_q21a_year)) %>%
  ungroup() %>%
  dplyr::select(w1_partnership_status, name, total)




min(year_q24$w1_q21a_year, na.rm = TRUE)

test <- year_q24[year_q24$w1_partnership_status=="Married",]
View(test)

test <- year_q24 %>% filter(name == "Met Online")

ggplot(data = test, aes(x=w1_q21a_year, y=total)) +
  geom_line(aes(colour=name)) +
  labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met") +
  ggtitle("Most Common Methods for Couples to Meet Throughout the Years") +
  scale_y_continuous(limits = c(0,60),
                     breaks = seq(0, 60, by=10))


# Trying to deploy on github pages
shinylive::export(appdir = "app", destdir = "docs")
httpuv::runStaticServer("docs/", port=8008)


## past version

# cols_q24sub <- c("w1_q21a_year","w1_partnership_status", morethan10)
# 
# # Load data
# 
# # make plot 1
# #### married, partnered+not married, and unpartnered+past lover
# 
# dsub_top7 <- d[,cols_q24sub]
# 
# all <- pivot_longer(
#   data = dsub_top7,
#   cols = c(morethan10)
# ) %>% group_by(w1_q21a_year,name) %>%  
#   summarise(total = sum(value, na.rm = TRUE), .groups = "keep") %>%
#   mutate(name = recode(name,
#                        w1_q24_bar_restaurant = 'Bar or Restaurant',
#                        w1_q24_college = 'College',
#                        w1_q24_internet_dating =  'Internet Dating',
#                        w1_q24_internet_other = 'Other Internet',
#                        w1_q24_met_online = 'Met Online',
#                        w1_q24_party = 'Party',
#                        w1_q24_school = 'School',
#                        w1_q24_blind_date = "Blind Date",
#                        w1_q24_business_trip = "Business Trip",
#                        w1_q24_church = "Place of Worship",
#                        w1_q24_customer = "Customer",
#                        w1_q24_internet_chat = "Internet Chat",
#                        w1_q24_internet_game = 'Internet Game',
#                        w1_q24_internet_org = 'Internet Organization',
#                        w1_q24_internet_soc_network = 'Internet Social Network',
#                        w1_q24_mil = 'Military',
#                        w1_q24_public = 'Public',
#                        w1_q24_singles_serve_nonint = 'Single Service (Not Internet)',
#                        w1_q24_vacation = 'Vacation',
#                        w1_q24_vol_org = 'Volunteer Organization',
#                        w1_q24_work_neighbors = 'Work Neighbors'))
# 
# 
# q24_sub_all <- ggplot(data = all, aes(x=w1_q21a_year, y=total)) + 
#   geom_line(aes(colour=name)) +
#   labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met") +
#   ggtitle("Most Common Methods for Couples to Meet Throughout the Years") +
#   scale_y_continuous(limits = c(0,60),
#                      breaks = seq(0, 60, by=10))
# 
# plotly_all <- ggplotly(q24_sub_all)
# 
# 
# # make plot 2
# #### married only
# 
# dsub_married <- d[d$w1_partnership_status==1,cols]
# 
# married <- pivot_longer(
#   data = dsub_married,
#   cols = c(morethan10)
# ) %>% group_by(w1_q21a_year,name) %>%  
#   summarise(total = sum(value, na.rm = TRUE), .groups = "keep") %>%
#   mutate(name = recode(name,
#                        w1_q24_bar_restaurant = 'Bar or Restaurant',
#                        w1_q24_college = 'College',
#                        w1_q24_internet_dating =  'Internet Dating',
#                        w1_q24_internet_other = 'Other Internet',
#                        w1_q24_met_online = 'Met Online',
#                        w1_q24_party = 'Party',
#                        w1_q24_school = 'School',
#                        w1_q24_blind_date = "Blind Date",
#                        w1_q24_business_trip = "Business Trip",
#                        w1_q24_church = "Place of Worship",
#                        w1_q24_customer = "Customer",
#                        w1_q24_internet_chat = "Internet Chat",
#                        w1_q24_internet_game = 'Internet Game',
#                        w1_q24_internet_org = 'Internet Organization',
#                        w1_q24_internet_soc_network = 'Internet Social Network',
#                        w1_q24_mil = 'Military',
#                        w1_q24_public = 'Public',
#                        w1_q24_singles_serve_nonint = 'Single Service (Not Internet)',
#                        w1_q24_vacation = 'Vacation',
#                        w1_q24_vol_org = 'Volunteer Organization',
#                        w1_q24_work_neighbors = 'Work Neighbors'))
# 
# 
# q24_sub_married <- ggplot(data = married, aes(x=w1_q21a_year, y=total)) + 
#   geom_line(aes(colour=name)) +
#   labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met")+
#   ggtitle("Most Common Methods for Married US Couples to Meet Throughout the Years") +
#   scale_y_continuous(limits = c(0,60),
#                      breaks = seq(0, 60, by=10))
# plotly_married <- ggplotly(q24_sub_married)
# 
# # make plot 3
# #### partner
# 
# dsub_partner <- d[d$w1_partnership_status==2,cols]
# 
# partner <- pivot_longer(
#   data = dsub_partner,
#   cols = c(morethan10)
# ) %>% group_by(w1_q21a_year,name) %>%  
#   summarise(total = sum(value, na.rm = TRUE), .groups = "keep") %>%
#   mutate(name = recode(name,
#                        w1_q24_bar_restaurant = 'Bar or Restaurant',
#                        w1_q24_college = 'College',
#                        w1_q24_internet_dating =  'Internet Dating',
#                        w1_q24_internet_other = 'Other Internet',
#                        w1_q24_met_online = 'Met Online',
#                        w1_q24_party = 'Party',
#                        w1_q24_school = 'School',
#                        w1_q24_blind_date = "Blind Date",
#                        w1_q24_business_trip = "Business Trip",
#                        w1_q24_church = "Place of Worship",
#                        w1_q24_customer = "Customer",
#                        w1_q24_internet_chat = "Internet Chat",
#                        w1_q24_internet_game = 'Internet Game',
#                        w1_q24_internet_org = 'Internet Organization',
#                        w1_q24_internet_soc_network = 'Internet Social Network',
#                        w1_q24_mil = 'Military',
#                        w1_q24_public = 'Public',
#                        w1_q24_singles_serve_nonint = 'Single Service (Not Internet)',
#                        w1_q24_vacation = 'Vacation',
#                        w1_q24_vol_org = 'Volunteer Organization',
#                        w1_q24_work_neighbors = 'Work Neighbors'))
# 
# q24_sub_partner <- ggplot(data = partner, aes(x=w1_q21a_year, y=total)) + 
#   geom_line(aes(colour=name)) +
#   labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met")+
#   ggtitle("Most Common Methods for Unmarried Couples to Meet Throughout the Years") +
#   scale_y_continuous(limits = c(0,60),
#                      breaks = seq(0, 60, by=10))
# plotly_partner <- ggplotly(q24_sub_partner)
# 
# # make plot 4
# #### past partner
# 
# dsub_pastpartner <- d[d$w1_partnership_status==3,cols]
# 
# pastpartner <- pivot_longer(
#   data = dsub_pastpartner,
#   cols = c(morethan10)
# ) %>% group_by(w1_q21a_year,name) %>%  
#   summarise(total = sum(value, na.rm = TRUE), .groups = "keep") %>%
#   mutate(name = recode(name,
#                        w1_q24_bar_restaurant = 'Bar or Restaurant',
#                        w1_q24_college = 'College',
#                        w1_q24_internet_dating =  'Internet Dating',
#                        w1_q24_internet_other = 'Other Internet',
#                        w1_q24_met_online = 'Met Online',
#                        w1_q24_party = 'Party',
#                        w1_q24_school = 'School',
#                        w1_q24_blind_date = "Blind Date",
#                        w1_q24_business_trip = "Business Trip",
#                        w1_q24_church = "Place of Worship",
#                        w1_q24_customer = "Customer",
#                        w1_q24_internet_chat = "Internet Chat",
#                        w1_q24_internet_game = 'Internet Game',
#                        w1_q24_internet_org = 'Internet Organization',
#                        w1_q24_internet_soc_network = 'Internet Social Network',
#                        w1_q24_mil = 'Military',
#                        w1_q24_public = 'Public',
#                        w1_q24_singles_serve_nonint = 'Single Service (Not Internet)',
#                        w1_q24_vacation = 'Vacation',
#                        w1_q24_vol_org = 'Volunteer Organization',
#                        w1_q24_work_neighbors = 'Work Neighbors'))
# 
# 
# q24_sub_pastpartner <- ggplot(data = pastpartner, aes(x=w1_q21a_year, y=total)) + 
#   geom_line(aes(colour=name)) +
#   labs(x = "Year They Met", y = "Number of Couples", colour = "How They Met")+
#   ggtitle("Most Common Methods for Single People to Have Met Their Past Partners Throughout the Years") +
#   scale_y_continuous(limits = c(0,60),
#                      breaks = seq(0, 60, by=10))
# 
# plotly_pastpartner <- ggplotly(q24_sub_pastpartner)


# ui <- fluidPage(
#   sidebarLayout(
#     pickerInput(inputId = "w1_q21a_year",
#                   label = "Couple Status",
#                   choices = c("All", 
#                               "Married",
#                               "Unmarried Partners",
#                               "Single But Had Partner in Past",
#                               "Single and Never Had a Partner"),
#                   options = list(`style` = "btn-info"))
#     ,
#     mainPanel(
#       uiOutput("plot_hole")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   selectedData2 <- reactive({
#     year_q24
#   })
#   
#   output$plot_hole <- renderPlot({
#     plot(selectedData2())
#   })
#   
# }
# 
# shinyApp(ui = ui, server = server)

# for people born in X year, age difference with partner

# religion
# table(d$p17_pppa1648,d$w1_q24_church)
