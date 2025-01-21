# Phishing for Significance
For our DATASCI W241 (Experiments and Causal Inference) class, 
my group and I were concerned about whether cybersecurity training could be improved so that 
less students would be victims of online recruitment fraud. 

At the time this project occurred, 
there was no cybersecurity training that was provided to the students, 
and the university only offered "Fight the Phish" posters which we had to search for ourselves. 
We created a multi-factorial experiment where we used a survey to test whether having interactive cybersecurity training, as compared to training where 
which only requires reading, would increase the accuracy rate of identification rate of phishing emails that are targeted to students.
In order to ensure that the respondents weren't just clicking through the training, we also included a timer so that the respondents 
had to stop and read through the training. For the measurement of the results, there were 10 emails, 
5 of which are real recruitment emails sent to the three of us but with the names changed and 5 of which are fake emails I wrote.

We find that there is no statistically significant difference in the identification rates between the students who 
received the interactive training vs. the passive training. The responses indicated that the emails looked "phishy"
in the first place, but a chi-square test revealed that there's no statistically significant difference between identifying
emails as phishing and whether they were actual phishing emails in the first place. The timer did not improve the respondent's 
accuracy rates of identifying phishing emails either.

While this was originally done in conjunction with Angelique Agho and Srila Maiti, most of the Qualtrics programming, R code, and 
writing is mine. 

Qualtrics link: https://berkeley.qualtrics.com/jfe/form/SV_0HZ8Urm7Wng0zzg<br>
Language: R<br>
R Libraries: data.table, lmtest, sandwich, reshape2, stargazer, diagram, tidyverse, ggpubr<br>
Statistics: Summary statistics, linear regression, A/B testing, chi-square test

## Files
* final_report.Rmd: contains the writeup, analysis, graphs, and code for the project
* final_report.pdf: contains the writeup, analysis, and graphs for the project in a report format for better reading
