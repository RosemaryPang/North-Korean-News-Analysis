##########################################
##### North Korea New Classification #####
#####         RA for Roseanne        #####
#####          Rosemary Pang         #####
#####         April. 18, 2019        #####
##########################################


##### Setting up the Environment #####
setwd("/Users/mpang/Dropbox/NKStatement/Training")
library(tm)
library(e1071)
library(dplyr)
library(caret)
library(doMC)
library(quanteda)
library(RColorBrewer)
library(randomForest)
##### Reading in Data #####
train1997 <- read.csv("train1997.csv",colClasses=c("NULL",NA,NA,NA,NA))
train1998 <- read.csv("train1998.csv",colClasses=c("NULL",NA,NA,NA,NA))
train1999 <- read.csv("train1999.csv",colClasses=c("NULL",NA,NA,NA,NA))
train2000 <- read.csv("train2000.csv",colClasses=c("NULL",NA,NA,NA,NA))
train2001 <- read.csv("train2001.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2002 <- read.csv("train2002.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2003 <- read.csv("train2003.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2004 <- read.csv("train2004.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2005 <- read.csv("train2005.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2006 <- read.csv("train2006.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2007 <- read.csv("train2007.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2008 <- read.csv("train2008.csv",colClasses=c("NULL",NA,NA,NA,"NULL",NA))
train2009 <- read.csv("train2009.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2010 <- read.csv("train2010.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2011 <- read.csv("train2011.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2012 <- read.csv("train2012.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2013 <- read.csv("train2013.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2014 <- read.csv("train2014.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2015 <- read.csv("train2015.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2016 <- read.csv("train2016.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2017 <- read.csv("train2017.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
train2018 <- read.csv("train2018.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL",NA))
library(data.table)
combine <- rbindlist(list(train1997,train1998,train1999,train2000,train2001,train2002,train2003,train2004,
                        train2005,train2006,train2007,train2008,train2009,train2010,train2011,train2012,
                        train2013,train2014,train2015,train2016,train2017,train2018))

### Code category 1 and 2 as threat, 3 as non-threat
combine$threat <- 1
combine$threat[combine$category==3] <- 0
table(combine$threat)
combine <- data.frame(lapply(combine, as.character), stringsAsFactors=FALSE)

write.csv(combine,"Annotated.csv") # Need to delete a strange output on 320 (Oct. 2, 2010, Anniversary of WPK Greeted)

combine <- read.csv("Annotated.csv")
combine <- data.frame(lapply(combine, as.character), stringsAsFactors=FALSE)
combine$threat <- as.factor(combine$threat)

##### Separate into train (400) and test (106)
### Randomize the dataset
set.seed(640714)
combine <- combine[sample(nrow(combine)),]
combine <- combine[sample(nrow(combine)),]


##### Remove punctuation, Stop Words, etc #####
corpus <- Corpus(VectorSource(combine$Content))
inspect(corpus[1:3])

corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%    # Lower case
  tm_map(removePunctuation) %>%               # Punctuation
  tm_map(removeNumbers) %>%                   # Numbers
  tm_map(removeWords, stopwords("english"))   # Stopwords


##### Document Term Matrix #####
dtm <- DocumentTermMatrix(corpus.clean)
inspect(dtm[1:10,1:10])

##### Devide into 400 training and 106 testing
train <- combine[1:400,]
test <- combine[401:506,]

dtm.train <- dtm[1:400,]
dtm.test <- dtm[401:506,]

corpus.clean.train <- corpus.clean[1:400]
corpus.clean.test <- corpus.clean[401:506]


##### Feature Selection #####
dim(dtm.train) #DTM contrains 9009 features but not all of them are useful for classification.
### Choose words appear at least 5 times
fivefreq <- findFreqTerms(dtm,5) #At least 5 times

dtm.train.five <- DocumentTermMatrix(corpus.clean.train,control = list(dictionary=fivefreq))  # Build new DTM
dim(dtm.train.five)     # Number of feature reduced to 2147

dtm.test.five <- DocumentTermMatrix(corpus.clean.test,control = list(dictionary=fivefreq))
dim(dtm.test.five) # Number of feature is also 2147


###### Wordcloud for threat and nonthreat ######
nonthreat <- combine[which(combine$threat==0),]
threat <- combine[which(combine$threat==1),]
nonthreat_dtm <- quanteda::dfm(nonthreat$Content,
                              tolower = TRUE,               # make all letters lower case
                              stem = FALSE,                 # do not stem
                              remove_punct = TRUE,          # remove punctuation
                              remove = stopwords("english"), # ignore common words on a "stop" list
                              ngrams = 1)   

threat_dtm <- quanteda::dfm(threat$Content,
                               tolower = TRUE,               # make all letters lower case
                               stem = FALSE,                 # do not stem
                               remove_punct = TRUE,          # remove punctuation
                               remove = stopwords("english"), # ignore common words on a "stop" list
                               ngrams = 1)   


set.seed(640714)
textplot_wordcloud(nonthreat_dtm, 
                   min_count=50, random_order=FALSE,
                   rotation = 0.25, color = RColorBrewer::brewer.pal(8, "Dark2"))


set.seed(640714)
textplot_wordcloud(threat_dtm, 
                   min_count=10, random_order=FALSE,
                   rotation = 0.25, color = RColorBrewer::brewer.pal(8, "Dark2"))



##### Naive Bayes Classification #####
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x){
  y <- ifelse(x>0,1,0)
  y <- factor(y,levels=c(0,1), labels=c("No","Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.five, 2, convert_count)
testNB <- apply(dtm.test.five, 2, convert_count)

### Training NB Model
# Train the classifier
system.time( classifier <- naiveBayes(trainNB, train$threat) )
# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )
# Confusion matrix
conf.mat <- confusionMatrix(pred,test$threat)
conf.mat

### Accuracy, precision, recall, f1
n <- sum(conf.mat$table)
nc <- nrow(conf.mat$table)
diag = diag(conf.mat$table)
rowsums <- apply(conf.mat$table,1,sum)
colsums <- apply(conf.mat$table,2,sum)
p <- rowsums/n
q <- colsums/n

# Accuracy
accuracy <- sum(diag)/n
recall <- diag/colsums
precision <- diag/rowsums
f1 <- 2*precision*recall/(precision+recall)


### NB model add laplace1 (this one is less accurate than first NB model, result not reported)
system.time( classifier <- naiveBayes(trainNB, train$threat,laplace=1) )
# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )
# Confusion matrix
conf.mat <- confusionMatrix(pred,test$threat)
conf.mat


##### SVM classification #####
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
}

# Apply the convert_count function to get final training and testing DTMs
trainSVM <- apply(dtm.train.five, 2, convert_counts)
testSVM <- apply(dtm.test.five, 2, convert_counts)
trainSVM <- as.data.frame(trainSVM)
testSVM <- as.data.frame(testSVM)

# Combine dataset
SVMdatatrain <- cbind(threat = factor(train$threat), trainSVM )
SVMdatatest <- cbind(threat = factor(test$threat), testSVM)

### Build SVM model
svm_model <- svm(threat ~., data = SVMdatatrain)

# Predict
fit.pred <- predict(svm_model, na.omit(testSVM))

conf.mat <- table(na.omit(testSVM$threat), fit.pred, dnn=c("Actual", "Predicted"))

conf.mat

### Accuracy, precision, recall, f1
n <- sum(conf.mat)
nc <- nrow(conf.mat)
diag = diag(conf.mat)

rowsums <- apply(conf.mat,1,sum)
colsums <- apply(conf.mat,2,sum)
p <- rowsums/n
q <- colsums/n

# Accuracy
accuracy <- sum(diag)/n
precision <- diag/colsums
recall <- diag/rowsums
f1 <- 2*precision*recall/(precision+recall)





##### Logistic regression classification #####
training_with_words <- as.data.frame(as.matrix(dtm.train.five))
training_words_final <- cbind(train$threat,training_with_words)
colnames(training_words_final)[1] <- "threat"
testing_with_words <- as.data.frame(as.matrix(dtm.test.five))
testing_words_final <- cbind(test$threat,testing_with_words)
colnames(testing_words_final)[1] <- "threat"


log_model <- glm(threat~., data=training_words_final,family = binomial,control=list(maxit=50))
summary(log_model)
log_pred_test <- predict(log_model,newdata=testing_words_final,type="response")
logtable <- table(log_pred_test>0.5,testing_words_final$threat)

# Accuracy is only 53%
### Accuracy, precision, recall, f1
n <- sum(logtable)
nc <- nrow(logtable)
diag = diag(logtable)
rowsums <- apply(logtable,1,sum)
colsums <- apply(logtable,2,sum)
p <- rowsums/n
q <- colsums/n

# Accuracy
accuracy <- sum(diag)/n
recall <- diag/colsums
precision <- diag/rowsums
f1 <- 2*precision*recall/(precision+recall)


##### Read in unannotated #####
##### Reading in Data #####
test1997 <- read.csv("test1997.csv",colClasses=c("NULL",NA,NA,NA))
test1998 <- read.csv("test1998.csv",colClasses=c("NULL",NA,NA,NA))
test1999 <- read.csv("test1999.csv",colClasses=c("NULL",NA,NA,NA))
test2000 <- read.csv("test2000.csv",colClasses=c("NULL",NA,NA,NA))
test2001 <- read.csv("test2001.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2002 <- read.csv("test2002.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2003 <- read.csv("test2003.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2004 <- read.csv("test2004.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2005 <- read.csv("test2005.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2006 <- read.csv("test2006.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2007 <- read.csv("test2007.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2008 <- read.csv("test2008.csv",colClasses=c("NULL",NA,NA,NA,"NULL"))
test2009 <- read.csv("test2009.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2010 <- read.csv("test2010.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2011 <- read.csv("test2011.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2012 <- read.csv("test2012.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2013 <- read.csv("test2013.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2014 <- read.csv("test2014.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2015 <- read.csv("test2015.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2016 <- read.csv("test2016.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2017 <- read.csv("test2017.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
test2018 <- read.csv("test2018.csv",colClasses=c("NULL","NULL",NA,NA,NA,"NULL"))
library(data.table)
unannotated <- rbindlist(list(test1997,test1998,test1999,test2000,test2001,test2002,test2003,test2004,
                          test2005,test2006,test2007,test2008,test2009,test2010,test2011,test2012,
                          test2013,test2014,test2015,test2016,test2017,test2018))
write.csv(unannotated,"Unannotated.csv")
# remove formular in 25442


library(readxl)
Unannotated2 <- read_excel("Unannotated2.xlsx")
text <- Unannotated2$Content
textclean <- gsub("_\x9c","",text)
textclean <- gsub("\xa8\xb2K","",textclean)
textclean <- gsub("_\xa8\xb9","",textclean)
textclean <- gsub("\x81@","",textclean)
textclean <- gsub("_\x99s","",textclean)
textclean <- gsub("_\x9aQ003","",textclean)
textclean <- gsub("_@","",textclean)
textclean <- gsub("_\x9ar","",textclean)
textclean <- gsub("(K_\x9ab_\x9am_\x9a`)","",textclean)
textclean <- gsub("(KCN\xef\xbf_)","",textclean)
textclean <- gsub("\x81","",textclean)
textclean <- gsub("_\x9d","",textclean)
textclean <- gsub("_\x9a_\x93","",textclean)
textclean <- gsub("_\x93","",textclean)
textclean <- gsub("_\xfc\xbe\x8e\x93\xa4\xbc","",textclean)
textclean <- gsub("\xfc\xbe\x98\xb3\xa4\xbc","",textclean)
textclean <- gsub("\xfc\xbe\x99\x83\xa4\xbc","",textclean)


##### Remove punctuation #####
corpus <- Corpus(VectorSource(textclean))
inspect(corpus[1:3])

corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%    # Lower case
  tm_map(removePunctuation) %>%               # Punctuation
  tm_map(removeNumbers) %>%                   # Numbers
  tm_map(removeWords, stopwords("english"))   # Stopwords

##### Document Term Matrix #####
dtm2 <- DocumentTermMatrix(corpus.clean)

##### Feature Selection #####
# still use feature in model building

dtm.unannotated.five <- DocumentTermMatrix(corpus.clean,control = list(dictionary=fivefreq))  # Build new DTM
unannotatedNB <- apply(dtm.unannotated.five, 2, convert_count)


### Predict threat
system.time( pred2 <- predict(classifier, newdata=unannotatedNB) )
Unannotated2$threat <- pred2
write.csv(Unannotated2,"PredUnannotated.csv")
