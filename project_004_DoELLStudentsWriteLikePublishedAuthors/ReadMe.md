# Do English Language Learners Write Like Published Authors?
For our DATASCI W266 (Natural Language Processing with Deep Learning) class, Srila and I examined 
whether the performance of BERT models could be improved in scoring essays written by English Language Learners (ELLs). 
Prior studies suggest that the pre-trained data (Google BooksCorpus and Wikipedia) of BERT was unsuited for automatic essay scoring, 
thus we decided to experiment with unfreezing the pretrained layers of BERT-base-cased and BERTweet-base. 
We decided to also experiment with BERTweet because it is pretrained on Tweets, 
and we are hoping that its informal nature will be more reminiscent of student writing. 

We find that depending less on the pretraining weights (with more layers unfrozen) provided more accurate predictions 
but BERT-base-cased outperformed BERTweet-base suggesting that student essays belonged to a different population than Tweets.
Together, the results indicate that ELL student essays don't contain writing similar to published authors (Google BooksCorpus), Wikipedia editors, or Tweets; therefore, models that don't contain training specific to student essays are unlikely to produce
scores that reflect their actual grade.

Srila wrote the majority of the code while I did the background research, piecing the code together, and the writing. 

Language: Python<br>
Python Libraries: MatPlotLib, Seaborn, Numpy, Pandas, transformers, sklearn, nltk, wordcloud, scipy, yellowbrick, torch<br>
Architecture: Tensorflow and Keras<br>
Statistics: Summary statistics

## Files
* essay_evaluation_code.ipynb: contains the code for the project
* essay_evaluation_paper.docx: contains the original writeup for the project
* essay_evaluation_paper.pdf: contains the writeup for the project, derived from the Word document