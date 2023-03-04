# LinkedIn Recommendations in a Word Cloud
I've previously used Word Clouds through my work, so I was interested in exploring how they functioned for myself through R and Tableau. Additionally, I wanted to see if there were any common words that people used to describe in their recommendations on LinkedIn so I combined these two desires.

The coding and write up was done by me.

Language: R<br>
R Libraries: readxl, writexl, tm, tau, RColorBrewer, wordcloud, wordcloud2, tidyverse, reshape2<br>
Software: Tableau<br>
Statistics: Bag of Words 

## Files
### Reports
* Linked In Recommendations in a Wordcloud.Rmd: contains the writeup, code, and wordclouds for the project
* Linked In Recommendations in a Wordcloud.pdf: contains the writeup, code, and wordclouds for the project in a report form
* Tableau Story.pdf: pdf of the Tableau workbook's story
### Supporting Files
* Recs.xlsx: the original raw dataset that was used for this project
* BagOfWords.xlsx: list of words that were created in R for use in Tableau
* R_Wordcloud.png: the wordcloud image that was created by R
* Step1.png: wordcloud created in Tableau without any data cleaning
* Step2.png: wordcloud created in Tableau with automated data cleaning
* Wordcloud.pdf: the wordcloud created by R 
* traits.xlsx: dataset of traits after processing
* traits_1.png: image of the filters, columns, and marks that was used in the Tableau workbook
* .gitignore: files that were ignored when pushing up to Git repository
