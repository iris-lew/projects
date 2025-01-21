# LA Traffic Collisions
Vision Zero's goal is to eliminate traffic fatalities and severe injuries. Los Angeles (LA) is infamous for its traffic and high speed highway chases, and it adopted Vision Zero in 2015 in an attempt to reduce the
traffic deaths to 0. There is inconsistent messaging on whether it is aimed at LA as a whole or only on unincorporated county roadways.
The analysis is based on Vision Zero's impact on LA as a whole. 

Using LA City's Traffic Collision data, I did some exploratory data analysis and regression discontinuity analysis on the number of traffic collisions, the number of victims, and the number of victims
with severe or fatal injuries. For the most part, the collisions increased from 2015 onwards until COVID-19 was declared a pandemic.
Most of the collisions were in streets (95.4%) and parking lots (3.2%). There are certain intersections 
(e.g., the intersection of Sepulveda Blvd and Sherman Way) where there are more collisions. 
Most collisions were vehicle vs. vehicle collisions, but not all them resulted in a severe or fatal injury.
Out of 370,447 collisions which has a code associated with injury, only 13,794 (~3.7%) had a report of a severe or fatal injury.
The regression discontinuity analysis determined that there was a staistically significant reduction in the number of traffic collisions, 
likely due to the  COVID-19 pandemic, but not due to Vision Zero. In fact, after Vision Zero was signed, 
there was an increase in the number of collisions. However, there seemed to be little to no difference in the number of 
collisions with severe or fatal injuries regardless of whether Vision Zero was signed or Covid-19 declared as a pandemic. 

While this was originally a group project by Ben Meier, Rebecca Sun, and me for our DATASCI W200 class together where we only did data exploration, I redid the writing and the code to be mine.
On November 10, 2024, I updated it to add the regression discontinuity analysis.

Next steps: If it supposed to focus on the unincorporated county roadways, then I should analyze only the traffic collisions on unincorporated county roadways.

Language: Python<br>
Python Libraries: MatPlotLib, Seaborn, Numpy, Pandas, Folium, copy, datetime, rdrobust<br>
Statistics: Summary statistics, Regression Discontinuity

## Files
* LA_Traffic_Collisions_Writeup.ipynb: contains the writeup, analysis, code, and graphs for the project. To view all outputs (including Folium map), click "Open in Colab" button to be redirected to Google Colab.
* LA_Traffic_Collisions_Writeup_nofoliummap.ipynb: contains the writeup, analysis, code, and graphs for the project (Folium map commented out to aid in GitHub pages display, all other block outputs displayed)
* LAPD_Reporting_Districts.csv: a dataset that contains the reporting districts' information
* MO_CODES_used_in_file.csv: the dataset I used after converting the MO Codes from the PDF (link to download can be found within the writeup) to csv
* MO_CODES_utf8.csv: the MO codes in utf-8 format
* sepulveda_sherman.png: a picture of an intersection within LA that was used in the writeup
