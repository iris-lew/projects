# Automatic Essay Scoring
Grading is an aruduous task. Not only does it run the risk of bias when the grader is a human (they may score essays they read first differently from how the essays they read last),
but there may be inconsistencies depending on the grader themselves. Thus, it may seem like a task that's perfect for artificial intelligence (AI). 
Automatic essay scoring (AES) is a machine learning problem where essays are parsed through a machine learning model and a score is assigned. Since a score is assigned by a machine, it must 
be unbiased...right? 

I explore this idea using two datasets: the ASAP-AES and the ELLIPSE Corpus. The ASAP-AES dataset is assumed to be essays from a general population 
whereas the ELLIPSE Corpus is a set of essays from English Language Learners (ELLs).
I decided to see if there were systematic differences in how AI models 
would score these two different essay datasets.  

I used a BERT-base-cased model that is fine-tuned on the ASAP-AES dataset. This is because the ASAP-AES dataset is larger (12,976) than the ELLIPSE Corpus (6,500)
and only has one score. The ASAP-AES dataset has is composed of 8 essay sets and only one of them has 2 scores, so it makes more sense to use the one 
score that all the essays have. The ELLIPSE Corpus has 6 separate scores (cohesion, syntax, vocabulary, phraseology, grammar, and conventions),
but there is an overall score.

There are problems with using two different datasets. The first is that both datasets use different scores. As the 
goal of this project is to see if there is any differences in the predicted scores, it should be sufficient to look at the 
distribution when comparing. The second is that the ASAP-AES dataset doesn't have consistent scoring criteria amongst its own
sets. For example, there is a different meaning in a score of 3 from Set 1 as compared to a score of 3 from Set 4. This will 
make interpreting the scores the model produces difficult.

I played around with the training and test data to see what happens when I add more and more ASAP-AES essay sets into the training data and using the other sets as a test set. 
This is to mimic what would happen if the structure of an essay is the same, but the prompt changes from year to year. 
The accuracy of the ASAP-AES test set increases with more essays, 
but only if the scoring criteria doesn't change drastically. 
Once the scoring criteria changes, especially with Sets 6 and 7, the model fails to predict any ASAP-AES scores. 
When all the sets were mixed together and 10% was reserved for the test set, I have an accuracy of about 57.7%. 

Generally, the ELLIPSE Corpus received a higher predicted score than the ASAP-AES essays so it doesn't seem to be biased against ELLs. 
Even when the ASAP-AES training data was varied, the predictions on the ELLIPSE 
test data was generally the same, but the only model which has a mixture of all 8 sets as part of the training data (only 90% of the ASAP-AES set) was able to predict 
scores higher than 20. This shows how much the model depends on the training data it is fine-tuned on and the failures of interpreting the scores outputted
(i.e., the same essay can receive a score of 4 or 10 and it's not clear why it's different other than the training data).

Practically speaking, it's not worth it to build a classroom or even school-level AES model. Students need to receive feedback on where their essays 
need improvement and having a clear scoring rubric is essential. The teachers will also need to read through and grade the essays themselves to 
see where students' weaknesses are and use it to inform their classroom instruction. As it's hard to generalize the model from one dataset to another,
especially with different scoring criteria, and challenging to gather enough examples for the model to be fine-tuned, it is not feasible to 
build an AES model. Even in the ASAP-AES Kaggle competition, the highest similarity between the machine produced scores and the human-produced scores was 81.7%.
If an AES model were implemented at a school- or classroom-level, the administrators will have to answer to the public on whether that is a reasonable 
threshold, or whether the model will need to be even more similar to a human-grader.

While building this, I followed a guide and adapted most of the code from the guide for use in AES_2_model_no_custom.ipynb and AES_3_model_set_order.ipynb.

Language: Python<br>
Python Libraries: Numpy, Pandas, PyTorch, PIL, wordcloud, transformers, MatPlotLib, Seaborn, scikit-learn, transformers, time, datetime,
SciPy<br>
Statistics: Large Language Model

## Files
* AES_1_data exploration.ipynb: data exploration for both datasets
* AES_2_model_no_custom.ipynb: Results of classifying the scores based on a BERT-base-cased model. 
Model trained on ASAP-AES essays and domain1_score.
* AES_3_model_set_order.ipynb: Results of classifying the scores based on a BERT-base-cased model. 
Models trained on ASAP-AES essay sets. Includes 7 models where each set is added into the 
training with the previous set(s) (i.e., Model 1 is trained on Set 1, predicting on Sets 2-8; 
Model 2 is trained on Sets 1-2, predicting on Sets 3-8; and so on so forth.).