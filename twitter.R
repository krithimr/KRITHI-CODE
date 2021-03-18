getwd()
setwd("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/Text mining")
library(rvest)
library(xml2)
library(readxl)
library(XML)
library(magrittr)
library(dplyr)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(textstem)
library(sentimentr)
library(DT)
library(stringr)
library(dplyr)

#The below dataset shows the tweets of Barack Obama in excel sheet. We exract just the tweets from the dataset in csv and txt format.
tweets_OBAMA <- read_excel("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/Text mining/tweets OBAMA.xlsx")
View(tweets_OBAMA)
TW= tweets_OBAMA$Tweets
attach(tweets_OBAMA)
length(TW)
write.csv(TW,"tweets_obama.csv")
write.table(TW,"obama_tweets.txt",row.names = F)
tweets<-read.csv(file.choose())
attach(tweets)
tweets<-tweets$x
View(tweets)

#sentimental analysis for the tweest of Obama,
#For sentimental analysis,convert all words into lower, remove numbers, 
#punctuations, stopwords, removewords, white space after removing words etc..
#A term document matrix is used to see how many words are being repeated in the extracted text mining document
#and remove the most repeted words by taking out their frequency. Plot the first few words based on their frequency

tweets<-as.character(tweets)
tweets<-iconv(tweets,"UTF-8")
tweets<-Corpus(VectorSource(tweets))
inspect(tweets[1])
tweets<-tm_map(tweets,tolower)
inspect(tweets[1])
tweets<-tm_map(tweets,removeNumbers)
tweets<-tm_map(tweets,removePunctuation)
tweets<-tm_map(tweets,removeWords,stopwords('english'))
tweets<-tm_map(tweets,removeWords,c('i','michelle','obama','nation','government','have','happy'))
tweets<-tm_map(tweets,stripWhitespace)
tdm<-TermDocumentMatrix(tweets)
tdm<-as.matrix(tdm)
tdm
tweets<-tm_map(tweets,stemDocument)
sort_obama<-sort(rowSums(tdm),decreasing=TRUE)
df_tweet<-data.frame(word=names(sort_obama),freq=sort_obama)
head(df_tweet,10)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=10)
barplot(sub_plot,las=3,col=rainbow(10))

#repeat the same steps after understanding the repeated words unnecessarly which influence the analysis.
#consider only the words which has similar number of observations.
tweets<-tm_map(tweets,removeWords,c('american','peopl','help','doyourjob','actonclim','can','leader','get','chang','now'))
tweets<-tm_map(tweets,stripWhitespace)
tdm<-TermDocumentMatrix(tweets)
tdm<-as.matrix(tdm)
tdm
tweets<-tm_map(tweets,stemDocument)
sort_obama<-sort(rowSums(tdm),decreasing=TRUE)
df_tweet<-data.frame(word=names(sort_obama),freq=sort_obama)
head(df_tweet,10)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=10)
barplot(sub_plot,las=3,col=rainbow(10))

#wordcloud for a set of words in the extracted tweets after filtering out all other words.
wordcloud(words=names(plot_sum),freq=plot_sum,random.order=F,colors=rainbow(10),scale=c(2,0.5),rot.per=0.3)

obama_positive<-scan('positive-words.txt',what='character',comment.char=';')
obama_negative<-scan('negative-words.txt',what='character',comment.char=';')
obama_positive[10:30]
obama_negative[10:30]
positive_cloud<-match(names(plot_sum),c(obama_positive))
positive_cloud<-!is.na(positive_cloud)
positive_freq<- plot_sum[positive_cloud]
positive_names<-names(positive_freq)
wordcloud(words=names(positive_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)
#words like support, love, afford and thank are commonly used terms in the reviews extracted which are given Obama.

#load positive and negative words list 
negative_cloud<-match(names(plot_sum),c(obama_negative))
negative_cloud<-!is.na(negative_cloud)
negative_freq<- plot_sum[negative_cloud]
negative_names<-names(negative_freq)
wordcloud(words=names(negative_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)
#words like shatter, suffer, weak, harsh, wrong etc. are negative words which had been shown in the tweets frequently.

#association between words
tdm<-TermDocumentMatrix(tweets)
findAssocs(tdm,c("shake"),corlimit=0.5)
findAssocs(tdm,c("support"),corlimit=0.5)
