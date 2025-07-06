# How Couples Meet

How Couples Meet and Stay Together 2017 (HCMST 2017) is a publicly available dataset that asks 3,510 survey respondents about their relationship. 

I decided to build an R Shiny app to display a graph of the ways couples meet throughout the years. 
The survey asked respondents how they met their partner in Q24 and their response was recorded as an 
open-ended response. The researchers then coded the response into categories of how they met. 
For example, if the response was, "A friend set me up on a blind date with my partner at a restaurant," 
the response would be coded as "Blind Date" and "Bar or Restaurant," double-counting this respondent.

The default is set to the most common methods of how all the couples meet. 
Interestingly, there is an exponential growth starting in the 2000s of couples meeting online and 
through internet dating, but when filtered for those who are married, the growth suddenly disappears. 
Sifting through the data, the trend is mostly explained by the "Unmarried partners." 

Language: R<br>
R Libraries: tidyverse, plotly, shiny, forcats<br>

## Files
* project_009_exploration.R: a "scratchpad" of sorts as I familiarize myself with the dataset
* project_009_q24graphs: a "scratchpad" of sorts as I try to build the graphs and app
* app/server.R: the server part of the app. It contains the code to build and display the plot.
* app/ui.R: the ui part of the app. It contains the code for the interface (selection and layout) of the app.