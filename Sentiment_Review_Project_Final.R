# Load all Library
library("RSQLite") 
library("tm")
library("RCurl")
library(grid)
library("klaR")

library(plyr)
library(stringr)
library(caret)
library(rpart)
library(rpart.plot)
library(RTextTools)
library(e1071)
library("ggplot2")
library(ggplot)
library(grid)
library(latticeExtra)

#load all data
moviedata <- read.csv("C:/BigData/capstone Course/trainfile.csv") # has 156060 obs of 4 variables

print(summary(moviedata))

# make 70% data as training and 30% data as test
total.data <- sample(nrow(moviedata),floor(nrow(moviedata)*0.7))
train.data <- moviedata[total.data,] # has 109242 rows of 4 variables
test.data <- moviedata[-total.data,] # has 46818 rows of 4 variables

# extract Phrase from the Training DATA
# prepare train data start here
doc.collect.train <- subset(train.data, select=Phrase:Sentiment) # has 109242 rows of 2 variables

my_phrase_train <- doc.collect.train$Phrase
my_char_train <- grep("^[A-Z]", my_phrase_train, value = TRUE) # has 21224 elements

my_sentence_train <- grep("[.]", my_char_train, value = TRUE) # has 6382 elements

head(my_sentence_train)

df_train <- data.frame(id=1:6382, words=my_sentence_train) # has 6382 observations

my_sentence_train <- Corpus(VectorSource(df_train$words))

my_sentence_train <- tm_map(my_sentence_train, stripWhitespace)
my_sentence_train  <- tm_map(my_sentence_train , removePunctuation)
my_sentence_train  <- tm_map(my_sentence_train , removeWords, stopwords("english"))

# prepare test data start here
doc.collect.test <- subset(test.data, select=Phrase:Sentiment) # has 46818 rows
my_phrase_test <- doc.collect.test$Phrase # has 46818 elements
my_char_test <- grep("^[A-Z]", my_phrase_test, value = TRUE) # has 8960 elements
my_sentence_test <- grep("[.]", my_char_test, value = TRUE) #has final 2681 rows

head(my_sentence_test)

df_test <- data.frame(id=1:2681, words=my_sentence_test) # has 2681 observations

head(df_test)

my_sentence_test <- Corpus(VectorSource(df_test$words))

my_sentence_test <- tm_map(my_sentence_test, stripWhitespace)
my_sentence_test  <- tm_map(my_sentence_test , removePunctuation)
my_sentence_test <- tm_map(my_sentence_test , removeWords, stopwords("english"))

# prepare test data end here

require(tm)
require(SnowballC)

initial.tdm <- TermDocumentMatrix(my_sentence_train)
examine.tdm <- removeSparseTerms(initial.tdm, sparse = 0.96)
top.words <- Terms(examine.tdm)
print(top.words)

# convert elements of character vector to UTF-8
bytecode.convert <- function(x) {iconv(enc2utf8(x), sub = "byte")}

#load 2005 positive words
positive.data.frame <- read.table(file="C:/BigData/positive-words_ without header.txt",header=FALSE,colClasses=c("character"),row.names=NULL,col.names="positive.words")

#load 4783 negative words
negative.data.frame <- read.table(file="C:/BigData/negative-words_ without header.txt",header=FALSE,colClasses=c("character"),row.names=NULL,col.names="negative.words")

positive.data.frame$positive.words <- bytecode.convert(positive.data.frame$positive.words)
  
negative.data.frame$negative.words <- bytecode.convert(negative.data.frame$negative.words)

posword <- positive.data.frame$positive.words # 2005 words
negword <- negative.data.frame$negative.words # 4783 words
  
# evaluate text measures for each word 
total.words <- integer(length(names(my_sentence_train)))
positive.words <- integer(length(names(my_sentence_train)))
negative.words <- integer(length(names(my_sentence_train)))
other.words <- integer(length(names(my_sentence_train)))

reviews.tdm <- TermDocumentMatrix(my_sentence_train)

for(index.for.document in seq(along=names(my_sentence_train))) {
  positive.words[index.for.document] <- sum(termFreq(my_sentence_train[[index.for.document]],
               control = list(dictionary=positive.data.frame$positive.words)))
  
  negative.words[index.for.document] <- sum(termFreq(my_sentence_train[[index.for.document]],
                                                     control = list(dictionary=negative.data.frame$negative.words)))
  total.words[index.for.document] <- length(reviews.tdm[,index.for.document][["i"]])
  other.words[index.for.document] <- total.words[index.for.document] - 
  positive.words[index.for.document] - 
    negative.words[index.for.document]  
  
}
head(text.measures.data.frame)

document <- names(my_sentence_train)

text.measures.data.frame <- data.frame(document, total.words,positive.words,
                                       negative.words,other.words, stringsAsFactors = FALSE) 
rownames(text.measures.data.frame) <- paste("D",as.character(1:6382),sep="")

head(text.measures.data.frame) # has 5 variables -- document (1,2...), total.words, positive.words,negative.words, other.words


text.measures.data.frame["total.words",]
lngth <- length(text.measures.data.frame$total.words)

 
text.measures.data.frame$POSITIVE <-
  100 * text.measures.data.frame$positive.words /
  text.measures.data.frame$total.words

  for (i in 1:lngth)
  {if(text.measures.data.frame$total.words[i] == 0)text.measures.data.frame$POSITIVE[i] <- 0}

  text.measures.data.frame$NEGATIVE <-
  100 * text.measures.data.frame$negative.words /
  text.measures.data.frame$total.words

  for (i in 1:lngth)
  {if(text.measures.data.frame$total.words[i] == 0)text.measures.data.frame$NEGATIVE[i] <- 0}
  

head(text.measures.data.frame)


# processing of test data starts here
initial.tdm.test <- TermDocumentMatrix(my_sentence_test)
examine.tdm.test <- removeSparseTerms(initial.tdm.test, sparse = 0.96)
top.words.test <- Terms(examine.tdm.test)
print(top.words.test)

# evaluate text measures for each word for test data
total.words.test <- integer(length(names(my_sentence_test)))
positive.words.test <- integer(length(names(my_sentence_test)))
negative.words.test <- integer(length(names(my_sentence_test)))
other.words.test <- integer(length(names(my_sentence_test)))

reviews.tdm.test <- TermDocumentMatrix(my_sentence_test)

for(index.for.document in seq(along=names(my_sentence_test))) {
  positive.words.test[index.for.document] <- sum(termFreq(my_sentence_test[[index.for.document]],
                                                     control = list(dictionary=positive.data.frame$positive.words)))
  
  negative.words.test[index.for.document] <- sum(termFreq(my_sentence_test[[index.for.document]],
                                                     control = list(dictionary=negative.data.frame$negative.words)))
  total.words.test[index.for.document] <- length(reviews.tdm.test[,index.for.document][["i"]])
  other.words.test[index.for.document] <- total.words.test[index.for.document] - 
    positive.words.test[index.for.document] - 
    negative.words.test[index.for.document]  
  
}
document.test <- names(my_sentence_test) # has 2681 observations

text.measures.data.frametest <- data.frame(document.test, total.words.test,positive.words.test,
                                       negative.words.test,other.words.test, stringsAsFactors = FALSE) 

rownames(text.measures.data.frametest) <- paste("D",as.character(1:2681),sep="")
head(text.measures.data.frametest)

text.measures.data.frametest["total.words.test",]
lngthtest <- length(text.measures.data.frametest$total.words.test)

text.measures.data.frametest$POSITIVE <-
  100 * text.measures.data.frametest$positive.words.test /
  text.measures.data.frametest$total.words.test

for (i in 1:lngthtest)
{if(text.measures.data.frametest$total.words.test[i] == 0)text.measures.data.frametest$POSITIVE[i] <- 0}

text.measures.data.frametest$NEGATIVE <-
  100 * text.measures.data.frametest$negative.words.test /
  text.measures.data.frametest$total.words.test

for (i in 1:lngthtest)
{if(text.measures.data.frametest$total.words.test[i] == 0)text.measures.data.frametest$NEGATIVE[i] <- 0}

head(text.measures.data.frametest)

# processing of test data ends here


#routine for sentiment scoring alogorithm starts here 

score.sentiment = function(sentences, posword,negword, .progress='none')
{
  require(plyr)
  require(stringr)
  
  scores_a = laply(sentences, function(sentence, posword, negword) 
    {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, posword)
    neg.matches = match(words, negword)
  
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
  
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
      return(score)
  }, posword, negword, .progress=.progress)
  
  scores.df = data.frame(score=scores_a, text=sentences)
  return(scores.df)
}

## sentiment score routine ends here

## run train data score

result_train = score.sentiment(df_train$words,posword, negword) # has 6382 text with Scores

sentiment <- c("")
result_train <- data.frame(result_train, sentiment,stringsAsFactors=FALSE)

# run histogram of train data score
hist(result_train$score)

testscore <- length(result_train$score) # 6382 length
as.integer(testscore)


for(i in 1:testscore){
  if (result_train$score[i] == 0) result_train$sentiment[i] <- "neutral"
  else
    if (result_train$score[i] > 0) result_train$sentiment[i] <- "positive"
  else
    if (result_train$score[i] < 0) result_train$sentiment[i] <- "negative" 
  else    result_train$sentiment[i] <- "na"
}

stat <- result_train

library(ROAuth)
library(ggplot2)
library(dplyr)
by.sentiment <- group_by(stat, sentiment)

by.sentiment <- summarise(by.sentiment, number=n())

# run ggplot for sentiments
ggplot(by.sentiment, aes(x=sentiment,y=number)) + geom_line(aes(group=sentiment, color=sentiment), size=2) + geom_point(aes(group=sentiment, color=sentiment), size=4) +  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1))

# send result train to excel 

write.table(result_train, "C:/BigData/result_trainnew.csv", sep = ",", col.names = NA, qmethod = "double")

# run test data score starts here

result_test = score.sentiment(df_test$words,posword, negword) # has 2681 observation with 2 variable

sentiment_test <- c("")
result_test <- data.frame(result_test, sentiment_test,stringsAsFactors=FALSE) # now 3 variables

hist(result_test$score) # test score histogram

count_score <- length(result_test$score) # 2681
as.integer(count_score)

# convert scores to "neutral", "positive", "negative" 
for(i in 1:count_score ){
  if (result_test$score[i] == 0) result_test$sentiment_test[i] <- "neutral"
  else
    if (result_test$score[i] > 0) result_test$sentiment_test[i] <- "positive"
  else
    if (result_test$score[i] < 0) result_test$sentiment_test[i] <- "negative" 
  else    result_test$sentiment_test[i] <- "na"
}
# send result test to excel 

write.table(result_test, "C:/BigData/result_testnew.csv", sep = ",", col.names = NA, qmethod = "double")

# draw graph for sentiment score
stat2 <- result_test
by.sentiment.test <- group_by(stat2, sentiment_test)

by.sentiment.test <- summarise(by.sentiment.test, number=n())

ggplot(by.sentiment.test, aes(x=sentiment_test,y=number)) + geom_line(aes(group=sentiment_test, 
                                                                          color=sentiment_test), size=2) + geom_point(aes(group=sentiment_test, color=sentiment_test), size=4) +  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1))
# run test data score ends here

# draw qq plots
colSelection<- c("total.words.test","positive.words.test", "negative.words.test", "other.words.test")
#creating a feature plot 

featurePlot(x=test.final[,colSelection],y = test.final$sentiment_test,plot="pairs")

qplot(positive.words.test, negative.words.test, colour=sentiment_test, data=test.final)

qplot(positive.words, negative.words, colour=sentiment, data=train.final)

# In order to understand what is going with the strange groupings on I created a histogram 
# of positive and negative words

par(mfrow=c(1,2))
hist(positive.words.test, main = "positive.words.test")
hist(negative.words.test, main="negative.words.test")

## Support Vector Machine with RWeka

## run with train data

library("RWeka")

# x_all has 6 variables - total.words, positive.words, negative.words, other.words, POSITIVE, NEGATIVE
x_all_1 <- subset(text.measures.data.frame, select=-document) # has 6382 obs of 6 variables
# y_all has 1 variable -sentiment
y_all_1 <- subset(result_train, select=sentiment) # has 6382 obs of 1 variable
train.final_1 <- cbind(x_all, y_all) # has 6382 obs of 7 variables

system.time(smo.full <- SMO(sentiment_factor ~ positive.words + negative.words + other.words, data=train.final_1,
                            control = Weka_control(K = "weka.classifiers.functions.supportVector.RBFKernel")))

system.time(smo.full.eval <- evaluate_Weka_classifier(smo.full, numFolds=10,train.final_1, class=T)) 

smo.full.eval

# run with test data
x_test_svm <- subset(text.measures.data.frametest, select=-document.test) # has 2681 obs & 6 variables
x_test1_svm <- subset(x_test_svm, select =-POSITIVE)  # has 5 variables
x_test2_svm <- subset(x_test1_svm, select =-NEGATIVE) # has 4 variables

# y_all has 1 variable -sentiment
y_test <- subset(result_test, select=sentiment_test) # has 2681 obs & 1 variable

test.final_svm <- cbind(x_test2_svm, y_test) # has 2681 obs & 5 variables

colnames(test.final_svm) <- c('total.words', 'positive.words', 'negative.words', 'other.words','sentiment_factor')

system.time(smo.test.eval <- evaluate_Weka_classifier(smo.full, numFolds=10,test.final_svm, class=T)) 

smo.test.eval

## RANDOM FOREST with Rweka
library(randomForest)

rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")

# run with train data - tree size 1000
system.time(randf.full <- rf(sentiment_factor ~ positive.words + negative.words + other.words, data=train.final_1, control = Weka_control(I = 1000)))

summary(randf.full)

print(randf.full)

system.time(randf.full.eval <- evaluate_Weka_classifier(randf.full, numFolds=10,train.final_1, class=T) )

randf.full.eval

# run with train data - tree size 500
system.time(randf.full2 <- rf(sentiment_factor ~ positive.words + negative.words + other.words, data=train.final_1, control = Weka_control(I = 500)))

summary(randf.full2)

system.time(randf.full.eval2 <- evaluate_Weka_classifier(randf.full2, numFolds=10,train.final_1, class=T) )
randf.full.eval2


#run with test data - tree size 1000

test.final.rf <- test.final_svm

colnames(test.final.rf) <- c('total.words','positive.words', 'negative.words', 'other.words','sentiment_factor')
test.final.rf <- subset(test.final.rf, select=c("positive.words", "negative.words","other.words", "sentiment_factor"))

head(test.final.rf)

system.time(randf.test.eval <- evaluate_Weka_classifier(randf.full, numFolds=10,test.final.rf, class=T)) 

randf.test.eval

#run with test data - tree size 500
system.time(randf.test.eval2 <- evaluate_Weka_classifier(randf.full2, numFolds=10,test.final.rf, class=T)) 
randf.test.eval2

# naive bayes using weka

# use train data
  trainNB <- subset(train.final_1, select=c("positive.words", "negative.words","other.words", "sentiment"))   

system.time(weka_fit <- NB(as.factor(sentiment) ~ positive.words + negative.words + other.words, data = trainNB))
evaluate_Weka_classifier(weka_fit, numFolds = 10)

# use test data
test.final_NB <- test.final_svm

testNB <- subset(test.final_NB, select=c("positive.words", "negative.words","other.words", "sentiment_factor")) 

colnames(testNB) <- c('positive.words', 'negative.words', 'other.words','sentiment')

weka_fit <- NB(as.factor(sentiment) ~ positive.words + negative.words + other.words, data = testNB) 

weka_fit.testNB <- evaluate_Weka_classifier(weka_fit, numFolds = 10, data = testNB)  

weka_fit.testNB

