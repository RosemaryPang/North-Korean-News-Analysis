# North-Korean-News-Analysis

Research assistant to Dr. Roseanne W. McManus (Spring 2019)

This project first aims to identify threats to other countries in North Korean online news. We first scrape North Korean online news from 1997 to 2018, and manually annotated 506 random news into "threat" and "nonthreat". We then use machine learning method to predict the characteristics of unannotated news.

# NKscrape.py

A python file that scrape North Korean online news (the webpage requires a Japanese server).

# NKanalysis.R

An R file that extract the title, date, and content of news. Then randomly select 23 news each year for annotation.

# NKClassification.R

An R file that analyze annotated news and predict unannotated news using machine learning method. This file includes wordcloud for threat and nonthreat news, and comparing machine learning results using Naive Bayes, SVM, and logistic regression. 
