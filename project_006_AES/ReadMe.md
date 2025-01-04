# Automatic Essay Scoring
Grading is an aruduous task. Not only does it run the risk of bias when the grader is a human (they may score essays they read first differently from how the essays they read last),
but there may be inconsistencies depending on the grader themselves. Thus, it may seem like a task that's perfect for artificial intelligence (AI). I explore this idea 
using two datasets: the ASAP-AES and the ELLIPSE Corpus.

Language: Python<br>
Python Libraries: Numpy, Pandas, torch, sk-learn, time, datetime, transformers, MatPlotLib, Seaborn<br>
Statistics: Large Language Model

## Files
* AES_1_data exploration.ipynb: data exploration for both datasets
* AES_2_model_no_custom.ipynb: Results of classifying the scores based on a BERT-base-cased model. 
Model trained on ASAP-AES essays and domain1_score.
* AES_3_model_set_order.ipynb: Results of classifying the scores based on a BERT-base-cased model. 
Models trained on ASAP-AES essay sets. Includes 7 models where each set is added into the 
training with the previous set(s) (i.e., Model 1 is trained on Set 1, predicting on Sets 2-8; 
Model 2 is trained on Sets 1-2, predicting on Sets 3-8; and so on so forth.).