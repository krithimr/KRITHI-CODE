#Zomata reviews had been extracted.
#Loading the libraries.
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
library(DT)
library(stringr)
library(dplyr)
aur<-"https://www.trustpilot.com/review/www.zomato.com?page="
zomato_review<-NULL
for(i in 1:10){
  review<-read_html(as.character(paste(aur,i,sep="=")))
  rev<-review %>%
    html_nodes(".review-content__text") %>%
    html_text()
  zomato_review<-c(zomato_review,rev)
}
length(zomato_review)
write.table(zomato_review,"zomato.txt",row.names=F)
write.csv(zomato_review,file="zomato.csv")
getwd()
zomata<-read.csv(file.choose())

#sentimental analysis for the movie reviews,
#For sentimental analysis,convert all words into lower, remove numbers, 
#punctuations, stopwords, removewords, white space after removing words etc..
#A term document matrix is used to see how many words are being repeated in the extracted text mining document
#and remove the most repeted words by taking out their frequency. Plot the first few words based on their frequency
zomato_trust<-as.character(zomato_review)
zomato_trust<-iconv(zomato_trust,"UTF-8")
zomato_trust<-Corpus(VectorSource(zomato_trust))
inspect(zomato_trust[1])
zomato_trust<-tm_map(zomato_trust,tolower)
inspect(zomato_trust[1])
zomato_trust<-tm_map(zomato_trust,removeNumbers)
zomato_trust<-tm_map(zomato_trust,removePunctuation)
zomato_trust<-tm_map(zomato_trust,removeWords,stopwords('english'))
zomato_trust<-tm_map(zomato_trust,removeWords,c('zomato','is','bought','a','order'))
zomato_trust<-tm_map(zomato_trust,stripWhitespace)
inspect(zomato_trust[1])
zomato_trust<-lemmatize_words(zomato_trust)
tdm<-TermDocumentMatrix(zomato_trust)
tdm<-as.matrix(tdm)
tdm
zomato_trust<-tm_map(zomato_trust,stemDocument)
sort_zomato<-sort(rowSums(tdm),decreasing=TRUE)
df_zomato<-data.frame(word=names(sort_zomato),freq=sort_zomato)
head(df_zomato,10)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=20)
barplot(sub_plot,las=3,col=rainbow(10))


#repeat
#repeat the same steps after understanding the repeated words unnecessarly which influence the analysis.
#conside only the words which has similar number of observations.
zomato_trust<-tm_map(zomato_trust,removeWords,c('delivery','customer','delivered','money','time','refund'))
tdm<-TermDocumentMatrix(zomato_trust)
tdm<-as.matrix(tdm)
tdm
zomato_trust<-tm_map(zomato_trust,stemDocument)
sort_zomato<-sort(rowSums(tdm),decreasing=TRUE)
df_zomato<-data.frame(word=names(sort_zomato),freq=sort_zomato)
head(df_zomato,10)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=20)
barplot(sub_plot,las=3,col=rainbow(10))

#wordcloud
#wordcloud for the words after filtering can be plotted in such a way that most repeated comes on the center.
#rest of the words surrounds the other.
wordcloud(words=names(plot_sum),freq=plot_sum,random.order=F,colors=rainbow(50),scale=c(2,0.5),rot.per=0.3)

#words
getwd()
#words can be compared with a set of positive and negative words.There are 2006 positive words in the given text document.
# and 4783 negative words.
#Comparing those words with echo document reviews, most of the reviews holds a positive comment of "like".
#ie, the product is liked by the set of users.
zomato_positive<-scan('positive-words.txt',what='character',comment.char=';')
zomato_negative<-scan('negative-words.txt',what='character',comment.char=';')
zomato_positive[10:30]
zomato_negative[10:30]
positive_cloud<-match(names(plot_sum),c(zomato_positive))
positive_cloud<-!is.na(positive_cloud)
positive_freq<- plot_sum[positive_cloud]
positive_names<-names(positive_freq)
wordcloud(words=names(positive_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)


#similarly negative words are compared with the extracted words, where many users provided the review that "weak ,
#the words common to both are surrounded. It is necessary to find the association between the words in the word cloud.
negative_cloud<-match(names(plot_sum),c(zomato_negative))
negative_cloud<-!is.na(negative_cloud)
negative_freq<- plot_sum[negative_cloud]
negative_names<-names(negative_freq)
wordcloud(words=names(negative_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)

#association
tdm<-TermDocumentMatrix(zomato_trust)
findAssocs(tdm,c("like"),corlimit=0.3)
