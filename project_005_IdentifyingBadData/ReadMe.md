# Identifying Bad Data
Models are only as good as the datasets they are trained on. Some datasets contain better data than other datasets, 
and it's up to the researcher to identify which are good or bad datasets. 
As data scientists, we should be able to examine the dataset and identify whether the model is truly doing what it is stated to be 
doing. While there is debate over how much we responsibility we take over the models we create and it is often not a one-person job,
we should not build models "hoping that the AI can learn something" even when we don't know what exactly it's learning.

To demonstrate, I found two datasets on wildfires. The datasets I use as examples are found in the Kaggle links within the write-up.
If the objective is to build a model detect wildfires, then it seems like these two datasets are good candidates to build a model with.
The first dataset is a file of ~30,000 images from Canada that are "wildfire" and "nowildfire" images, 
which implies I should build an image classification model.
The neural net model that I built predicts the wildfire images with ~90% accuracy, but I find 
myself questioning whether the model built using the data from the image, file names, and the Kaggle description is truly 
identifying wildfires or whether it is identifying non-urban images. 
The second dataset is a US dataset of wildfires and contains a detailed 
list of wildfires throughout the US, but there is no stated objective to the dataset. I can use the dataset to 
pull images with timestamps associated with them to predict whether a wildfire will occur in the next 6 months, join it with 
weather data, or do something else with the information; it just depends on how creative I am with the data available and 
whether the logic in building the model is sound.

Some of the code was taken from a group project I participated in as part of my master's degree. 
I have labeled which code chunks that is not my original code and that I've adapted for this demonstration.

Language: Python<br>
Python Libraries: Numpy, Pandas, landsatxplore, MatPlotLib, Pillow, folium, cv2, Sci-kit Learn, tensorflow, pickle, json, tifffile<br>
Statistics: Neural Networks

## Files
* Identifying_bad_datasets.ipynb: contains the writeup, analysis, code, and graphs for the project. To view all outputs and interact with the interactive maps, click the "Open in Colab" button to be redirected to Google Colab.
* identifying_bad_datasets_supplementary.ipynb: contains the code to generate the "urban" column for one of the datasets in the writeup.
* Canada_oecd_methodology.pdf: a pdf explaining how the functional urban areas of Canada were defined by the OECD.
* Canada_Wildfires_gov_website.jpg: screenshot of the Canada Wildfires government website to show the language used.
* Canada_Wildfires_kaggle.jpg: screenshot of the Canada Wildfires Kaggle page to show the language used.