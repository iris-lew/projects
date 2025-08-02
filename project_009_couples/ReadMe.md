# How Couples Meet

How Couples Meet and Stay Together 2017 (HCMST 2017) is a publicly available dataset that asks 3,510 survey respondents about their relationship. 

When creating Dashboard 1 (How US Couples Meet Throughout the Years), 
I decided to build an R Shiny app to create in order 
to display a graph of the ways couples meet throughout the years. 
The survey asked respondents how they met their partner in Q24 and their response was recorded as an 
open-ended response. The researchers then coded the response into categories of how they met. 
For example, if the response was, "A friend set me up on a blind date with my partner at a restaurant," 
the response would be coded as "Blind Date" and "Bar or Restaurant," double-counting this respondent. 
The filters in the dashboard included how they met and the couple's status (married, unmarried partners, 
single (but had past relationships), and single (never dated)).

The default is set to the most common methods of how all the couples meet. 
Interestingly, there is an exponential growth starting in the 2000s of couples meeting online and 
through internet dating, but when filtered for those who are married, the growth suddenly disappears. 
Sifting through the data, the trend is mostly explained by the "Unmarried partners." 

Then, when creating Dashboard 2 (How Couples Meet and Whether They Stay Together), I extended the 
previous analysis to include a sankey graph to display whether the couples stayed together or 
changed their relationship status throughout the years. Most people who were married stayed married. 
While it seems that most people who met through internet dating or phone apps were unmarried couples 
in 2017, a fair proportion of them converted to being married in 2020 so they just didn't have the 
time to get married during their 2017 survey.


Language: R<br>
R Libraries: tidyverse, plotly, shiny, forcats<br>

## Files
* project_009_exploration.R: a "scratchpad" of sorts as I familiarize myself with the dataset
* dashboard1/archive/project_009_q24graphs.R: a "scratchpad" of sorts as I try to build the graphs and app for dashboard 1
* dashboard1/app/server.R: the server part of the dashboard 1 app. It contains the code to build and display the plot.
* dashboard1/app/ui.R: the ui part of the dasbhoard 1 app. It contains the code for the interface (selection and layout) of the app.
* dashboard2/archive/project_009_q24w2_exploration.R: a "scratchpad" of sorts as I try to build the graphs and app for dashboard 2
* dashboard2/archive/project_009_q24w2.R: a "scratchpad" of sorts as I try to build the graphs and app for dashboard 2. Slightly different from exploration.
* dashboard2/app/server.R: the server part of the dashboard 2 app. It contains the code to build and display the plot.
* dashboard2/app/ui.R: the ui part of the dashboard 2 app. It contains the code for the interface (selection and layout) of the app.