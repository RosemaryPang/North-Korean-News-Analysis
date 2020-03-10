####################################
##### North Korea News Article #####
#####     Rosemary Pang        #####
#####     Feb. 5, 2019         #####
####################################

#### Read into csv
setwd("/Users/mpang/Dropbox/NKStatement/2008")
pathTxt <- list.files(pattern = ".txt") # This creates a list of News Case files, in a bad order
library(gtools)
pathTxt <- mixedsort(sort(pathTxt)) # This function sorts dataframe in News Case order
GoodFormat <- pathTxt[c(1:1638)]
BadFormat <- pathTxt[c(1639:6240)]

### Use Regex to extract data of the news
library(purrr)
library(tidyr)
library(stringr)
dataTxt <- map_df(GoodFormat, function(txt){
     article <- readLines(txt, encoding='UTF-8') %>%
         paste0(collapse='')
     date <- str_extract(article,regex("[A-Za-z]{2,10}\\s\\d{0,2}.\\s\\d{4}"))
     input <- data.frame(date)
   })
sum(is.na(dataTxt$date))
#dataTxt[is.na(dataTxt$date),]
#id <- 1:1639
dataTxt$id<- c(1:1489,1491:1639)
  
  
### Extract title and content
data<- as.data.frame(matrix(NA,length(GoodFormat),3))
for (i in 1:length(GoodFormat))
{
  article<- readLines(GoodFormat[i])
  data[i,1]<- article[[1]]
  data[i,2]<- article[[2]]
  data[i,3]<- str_c(article[-c(1,2,length(article))],collapse ="")
}
sum(is.na(data$V3))
data<- subset(data,select=-c(V1))
names(data) <- c("title","content")
data$id<- c(1:1489,1491:1639)



##### Merge date, title and content
News <- merge(dataTxt,data,by="id")

##### Identify language
library(textcat)
News$language <- textcat(News$content)
EnglishNews <- News[!grepl("ACNC",News$content),]
EnglishNews <- EnglishNews[!grepl("ATCC",EnglishNews$content), ]
EnglishNews <- EnglishNews[EnglishNews$language!="spanish",]
table(EnglishNews$language)

##### randomly select 23 articles for training
set.seed(640714)
train_ind <- sample(seq_len(nrow(NK2006)), size=23)

train2006 <- NK2006[train_ind,]
test2006 <- NK2006[-train_ind,]
write.csv(train2006,"train2006.csv")
write.csv(test2006,"test2006.csv")


