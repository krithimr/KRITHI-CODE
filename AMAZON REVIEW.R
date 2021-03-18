setwd("C:\\Users\\DELL\\Desktop\\New folder\\DATA SCIENCE\\ASSIGNMENTS\\Text mining")
getwd()
install.packages("rvest")
install.packages("magrittr")
install.packages("XML")
install.packages("tm")
install.packages("xml2")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("textstem")
#loading libraries need for extraction and for sentimental analysis with the help of wordcloud
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
aur<-"https://www.amazon.com/product-reviews/B08F8RQ469/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&filterByStar=five_star&reviewerType=all_reviews&pageNumber="
amazone_reviews<-NULL
for(i in 1:10){
  mur<-read_html(as.character(paste(aur,i,sep="=")))
  rev<-mur %>%
    html_nodes(".review-text") %>%
    html_text()
  amazone_reviews<-c(amazone_reviews,rev)
}
length(amazone_reviews)
write.table(amazone_reviews,"echo.txt",row.names=F)
write.csv(amazone_reviews,file="alexaecho.csv")
getwd()
write.table(amazone_reviews,"echo.txt")
alexaecho<-read.csv(file.choose())


#sentimental analysis
#The process starts with converting the text document after text mining into character type.
#We encode the output into UTF-8 unicode statement to avoid errors in coding
alexaecho<-as.character(amazone_reviews)
alexaecho<-iconv(alexaecho,"UTF-8")
#The NLP text Vector source is for character vectors.
alexaecho<-Corpus(VectorSource(alexaecho))
inspect(alexaecho[1])

#For sentimental analysis,convert all words into lower, remove numbers, 
#punctuations, stopwords, removewords, white space after removing words etc..
#A term document matrix is used to see how many words are being repeated in the extracted text mining document
#and remove the most repeted words by taking out their frequency. Plot the first few words based on their frequency
echo<-tm_map(alexaecho,tolower)
inspect(echo[1])
echo<-tm_map(echo,removeNumbers)
echo<-tm_map(echo,removePunctuation)
echo<-tm_map(echo,removeWords,stopwords('english'))
echo<-tm_map(echo,removeWords,c('alexa','dot','the','and','your','browser','support','html','video'))
echo<-tm_map(echo,stripWhitespace)
inspect(echo[1])
echo<-lemmatize_words(echo)
tdm<-TermDocumentMatrix(echo)
tdm<-as.matrix(tdm)
tdm
echo<-tm_map(echo,stemDocument)
sort_echo<-sort(rowSums(tdm),decreasing=TRUE)
df_echo<-data.frame(word=names(sort_echo),freq=sort_echo)
head(df_echo,10)
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=50)
barplot(sub_plot,las=3,col=rainbow(20))


#repeat the same steps after understanding the repeated words unnecessarly which influence the analysis.
#conside only the words which has similar number of observations.
echo<-tm_map(echo,removeWords,c('echo','que','get','better','para','consid','m'))
echo<-tm_map(echo,stripWhitespace)
inspect(echo[1])
tdm<-TermDocumentMatrix(echo)
tdm<-as.matrix(tdm)
tdm
plot_sum<-rowSums(tdm)
sub_plot<-subset(plot_sum,plot_sum>=50)
barplot(sub_plot,las=3,col=rainbow(20))

#wordcloud for the words after filtering can be plotted in such a way that most repeated comes on the center.
#rest of the words surrounds the other.
wordcloud(words=names(plot_sum),freq=plot_sum,random.order=F,colors=rainbow(50),scale=c(2,0.5),rot.per=0.3)

#words can be compared with a set of positive and negative words.There are 2006 positive words in the given text document.
# and 4783 negative words.
#Comparing those words with echo document reviews, most of the reviews holds a positive comment of "like".
#ie, the product is liked by the set of users.
alexa_positive<-scan('positive-words.txt',what='character',comment.char=';')
alexa_negative<-scan('negative-words.txt',what='character',comment.char=';')
alexa_positive[10:30]
alexa_negative[10:30]
positive_cloud<-match(names(plot_sum),c(alexa_positive))
positive_cloud<-!is.na(positive_cloud)
positive_freq<- plot_sum[positive_cloud]
positive_names<-names(positive_freq)
wordcloud(words=names(positive_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)

#similarly negative words are compared with the extracted words, where many users provided the review that "weak ,
#the words common to both are surrounded. It is necessary to find the association between the words in the word cloud.
negative_cloud<-match(names(plot_sum),c(alexa_negative))
negative_cloud<-!is.na(negative_cloud)
negative_freq<- plot_sum[negative_cloud]
negative_names<-names(negative_freq)
wordcloud(words=names(negative_freq),freq=positive_freq,random.order=F,colors=rainbow(50),scale=c(3.5,0.5),rot.per=0.3)

#association of positive word "like" with rest of the words in wordcloud
tdm<-TermDocumentMatrix(echo)
findAssocs(tdm,c("like"),corlimit=0.3)
#words like home, still shows close association with like (0.99), others like think, want, network, differ etc. has association greater than 0.90.
#most of the positive words like appropriate, astonish,good, most,etc. are closely associated with the positive word given. 
