getwd()
library(rvest)
library(xml2)
library(XML)
library(magrittr)
library(dplyr)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(textstem)
library(sentimentr)
#extraction of IMDB movie review for SHAWSHANK REDEMPTION  
SR<-NULL
shawshank<-"https://www.imdb.com/title/tt0111161/reviews?spoiler=hide&sort=helpfulnessScore&dir=desc&ratingFilter=0"
for(i in 0:30){
  shawshank<-read_html(as.character(paste(shawshank,i,sep="")))
  shaw_red<-shawshank %>%
    html_nodes(".content") %>%
    html_text() 
  SR<-c(SR,shaw_red)
}
length(SR)
SR[1]
write.table(SR,"shawshank.txt")
write.csv(SR,file="shawred.csv")
getwd()
write.table(SR,"shawshank.txt")
shawshank<-read.table(file.choose())

#sentimental analysis for the movie reviews,
#For sentimental analysis,convert all words into lower, remove numbers, 
#punctuations, stopwords, removewords, white space after removing words etc..
#A term document matrix is used to see how many words are being repeated in the extracted text mining document
#and remove the most repeted words by taking out their frequency. Plot the first few words based on their frequency
attach(shawshank)
shawshank<-shawshank$x
imdb<-as.character(shawshank)
imdb_shwshank<-iconv(imdb,"UTF-8")
imdb_shwshank<-Corpus(VectorSource(imdb_shwshank))
inspect(imdb_shwshank[1])
imdb_shwshank<-tm_map(imdb_shwshank,tolower)
inspect(imdb_shwshank[1])
imdb_shwshank<-tm_map(imdb_shwshank,removeNumbers)
imdb_shwshank<-tm_map(imdb_shwshank,removePunctuation)
imdb_shwshank<-tm_map(imdb_shwshank,removeWords,stopwords('english'))
imdb_shwshank<-tm_map(imdb_shwshank,removeWords,c('shawshank','redemption','the','is','an','banker','a','to','for'))
imdb_shwshank<-tm_map(imdb_shwshank,stripWhitespace)
inspect(imdb_shwshank[1])
imdb_shwshank<-lemmatize_words(imdb_shwshank)
tdm<-TermDocumentMatrix(imdb_shwshank)
tdm<-as.matrix(tdm)
tdm
imdb_shwshank<-tm_map(imdb_shwshank,stemDocument)
sort_shwshank<-sort(rowSums(tdm),decreasing=TRUE)
df_shwshank<-data.frame(word=names(sort_shwshank),freq=sort_shwshank)
head(df_shwshank,20)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=100)
barplot(sub_plot,las=3,col=rainbow(20))

#repeat the same steps after understanding the repeated words unnecessarly which influence the analysis.
#conside only the words which has similar number of observations.
imdb_shwshank<-tm_map(imdb_shwshank,removeWords,c('movie','film','movies','movi','vote.','time','one','best','review','found','helpful','helpful?','permalink','sign'))
imdb_shwshank<-tm_map(imdb_shwshank,stripWhitespace)
inspect(imdb_shwshank[1])
tdm<-TermDocumentMatrix(imdb_shwshank)
tdm<-as.matrix(tdm)
tdm
imdb_shwshank<-tm_map(imdb_shwshank,stemDocument)
sort_shwshank<-sort(rowSums(tdm),decreasing=TRUE)
df_shwshank<-data.frame(word=names(sort_shwshank),freq=sort_shwshank)
head(df_shwshank,20)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=100)
barplot(sub_plot,las=3,col=rainbow(20))

#wordcloud for a set of words in the xtracted reviews after filtering out all other words.
wordcloud(words=names(plot_sum),freq=plot_sum,random.order=F,colors=rainbow(10),scale=c(2,0.5),rot.per=0.3)

#load positive and negative words list 
imdb_positive<-scan('positive-words.txt',what='character',comment.char=';')
imdb_negative<-scan('negative-words.txt',what='character',comment.char=';')
imdb_positive[10:30]
imdb_negative[10:30]
positive_cloud<-match(names(plot_sum),c(imdb_positive))
positive_cloud<-!is.na(positive_cloud)
positive_freq<- plot_sum[positive_cloud]
positive_names<-names(positive_freq)
wordcloud(words=names(positive_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)
#words like better, good, like and great are commonly used terms in the reviews extracted which are given by the users.

negative_cloud<-match(names(plot_sum),c(imdb_negative))
negative_cloud<-!is.na(negative_cloud)
negative_freq<- plot_sum[negative_cloud]
negative_names<-names(negative_freq)
wordcloud(words=names(negative_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)
#words like shatter, suffer, weak, harsh, wrong etc. are negative words which had been shown in the reviews frequently.

#association between words
tdm<-TermDocumentMatrix(imdb_shwshank)
findAssocs(tdm,c("shatter"),corlimit=0.5)
findAssocs(tdm,c("great"),corlimit=0.5)
#Association if the negative word shatter is taken into account the words like anger, astonish,etc. has 100% association with the word shatter.

