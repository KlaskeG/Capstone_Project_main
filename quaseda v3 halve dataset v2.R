# quanteda package

#halve sample
library(quanteda)
library(dplyr)
library(tictoc)
setwd("D:/Coursera Courses/Data Science Specialisation/Capstone Project/data/sample_half2/")
blogs <- textfile("blogs_half2.txt")
twitter <- textfile("twitter_half2.txt")
news <- textfile("news_half2.txt")

#add to corpus
corpusblogs <- corpus(blogs)
corpustwitter <- corpus(twitter)
corpusnews <- corpus(news)
corpustotaal<-corpusblogs+corpustwitter+corpusnews

#keep end of line and questionmark as token, important information for estimation next word
texts(corpustotaal) <-stringi::stri_replace_all_regex(texts(corpustotaal),"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpustotaal) <-stringi::stri_replace_all_regex(texts(corpustotaal),"\\. ", " EndOfSentence ",vectorize_all = FALSE)
texts(corpustotaal) <-stringi::stri_replace_all_regex(texts(corpustotaal),"\\? ", " QuestionMark ",vectorize_all = FALSE)
texts(corpustotaal) <-stringi::stri_replace_all_regex(texts(corpustotaal),"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
texts(corpustotaal) <-stringi::stri_replace_all_regex(texts(corpustotaal),"\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpusblogs) <-stringi::stri_replace_all_regex(texts(corpusblogs),"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpusblogs) <-stringi::stri_replace_all_regex(texts(corpusblogs),"\\. ", " EndOfSentence ",vectorize_all = FALSE)
texts(corpusblogs) <-stringi::stri_replace_all_regex(texts(corpusblogs),"\\? ", " QuestionMark ",vectorize_all = FALSE)
texts(corpusblogs) <-stringi::stri_replace_all_regex(texts(corpusblogs),"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
texts(corpusblogs) <-stringi::stri_replace_all_regex(texts(corpusblogs),"\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpustwitter) <-stringi::stri_replace_all_regex(texts(corpustwitter),"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpustwitter) <-stringi::stri_replace_all_regex(texts(corpustwitter),"\\. ", " EndOfSentence ",vectorize_all = FALSE)
texts(corpustwitter) <-stringi::stri_replace_all_regex(texts(corpustwitter),"\\? ", " QuestionMark ",vectorize_all = FALSE)
texts(corpustwitter) <-stringi::stri_replace_all_regex(texts(corpustwitter),"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
texts(corpustwitter) <-stringi::stri_replace_all_regex(texts(corpustwitter),"\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpusnews) <-stringi::stri_replace_all_regex(texts(corpusnews),"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
texts(corpusnews) <-stringi::stri_replace_all_regex(texts(corpusnews),"\\. ", " EndOfSentence ",vectorize_all = FALSE)
texts(corpusnews) <-stringi::stri_replace_all_regex(texts(corpusnews),"\\? ", " QuestionMark ",vectorize_all = FALSE)
texts(corpusnews) <-stringi::stri_replace_all_regex(texts(corpusnews),"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
texts(corpusnews) <-stringi::stri_replace_all_regex(texts(corpusnews),"\\n", " EndOfSentence ",vectorize_all = FALSE)

#tokenize cleaned corpus, removing all other punctuation and other characters, but keeping capitals
token_twitter_stripped<-tokenize(corpustwitter,removeNumbers=TRUE,removePunct=TRUE,
                                 removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)
token_blogs_stripped<-tokenize(corpusblogs,removeNumbers=TRUE,removePunct=TRUE,
                                 removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)
token_news_stripped<-tokenize(corpusnews,removeNumbers=TRUE,removePunct=TRUE,
                                 removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)

token_tot_stripped<-tokenize(corpustotaal,removeNumbers=TRUE,removePunct=TRUE,
                                 removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)

#check spelling doesn't work for vectors, so skip that
#sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
#correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }
#correct_token_tot_stripped<-correct(token_tot_stripped) #dit duurt zolang voor hele kleine set, ws niet te doen voor grotere set.
#gram2_tot<-ngrams(token_tot_stripped,n=2)
#head(gram2_tot)

#make featurematrix count, add percentage of complete text and cumulate sum
tic()
dfm_tot<-dfm(token_tot_stripped,toLower = FALSE)
topfeatures(dfm_tot, 10)
toc()

#Dfm_tot_Pct <- weight(dfm_tot, "relFreq") * 100
#topwords_tot<-data.frame(text=names(topfeatures(Dfm_tot_Pct,Dfm_tot_Pct@Dim[2])),perc=topfeatures(Dfm_tot_Pct,Dfm_tot_Pct@Dim[2]))
#ik krijg hem via dfm_tot_pct nog niet voor elkaar, maar kan ook handmatig.
topwords_tot<-data.frame(text=names(topfeatures(dfm_tot,dfm_tot@Dim[2])),Ncount=topfeatures(dfm_tot,dfm_tot@Dim[2]))
topwords_tot[1:10,]
sumtot<-summarise(topwords_tot,sum(Ncount)) #10521 (gelijk aan aantal features)
topwords_tot<-mutate(topwords_tot,perc=Ncount*100/as.numeric(sumtot))
topwords_tot$csum<-cumsum(topwords_tot$perc) 
tail(topwords_tot) #handmatig werkt het, bij gebruik weight in dfm krijg ik 300 als totaal= aantal docs?
plot(1:nrow(topwords_tot),topwords_tot$csum,main="Coverage by number of words",xlab="number of words", ylab="Percentage of total text covered")

#now without stopwords 
dfm_tot_stop <- removeFeatures(dfm_tot, stopwords("english"))
topwords_tot_stop<-data.frame(text=names(topfeatures(dfm_tot_stop,dfm_tot_stop@Dim[2])),Ncount=topfeatures(dfm_tot_stop,dfm_tot_stop@Dim[2]))
sumtot<-summarise(topwords_tot_stop,sum(Ncount))  #(gelijk aan aantal features)
topwords_tot_stop<-mutate(topwords_tot_stop,perc=Ncount*100/as.numeric(sumtot))
topwords_tot_stop$csum<-cumsum(topwords_tot_stop$perc) 

saveRDS(dfm_tot,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_tot.RDS"))
dfm_tot<-readRDS("20170206dfm_tot.RDS")
#evt save.image("text.RData") vs load("text.RData") workspace
rm(dfm_tot,dmf_tot_stop)
rm(blogs,news,twitter)

#featurematrix 2-gram and 3-gram
tic()
token_tot_2gram<-tokenize(corpustotaal,removeNumbers=TRUE,removePunct=TRUE,
                          removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE,ngrams=2)
dfm_tot_2gram<-dfm(token_tot_2gram,toLower = FALSE)
topwords_tot_2gram<-data.frame(text=names(topfeatures(dfm_tot_2gram,dfm_tot_2gram@Dim[2])),Ncount=topfeatures(dfm_tot_2gram,dfm_tot_2gram@Dim[2]))
topwords_tot_2gram[1:10,]
sumtot<-summarise(topwords_tot_2gram,sum(Ncount)) #(gelijk aan aantal features)
topwords_tot_2gram<-mutate(topwords_tot_2gram,perc=Ncount*100/as.numeric(sumtot))
topwords_tot_2gram$csum<-cumsum(topwords_tot_2gram$perc) 
tail(topwords_tot_2gram) #handmatig werkt het, bij gebruik weight in dfm krijg ik 300 als totaal= aantal docs?
toc()
plot(1:nrow(topwords_tot_2gram),topwords_tot_2gram$csum,main="Coverage by 2-grams",xlab="Number of 2-grams",ylab="Percentage of total text covered")
saveRDS(dfm_tot_2gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_tot_2gram.RDS"))
rm(dfm_tot_2gram)
dfm_tot_2gram<-readRDS("20170207dfm_tot_2gram.RDS")

tic()
token_tot_3gram<-tokenize(corpustotaal,removeNumbers=TRUE,removePunct=TRUE,
                          removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE,ngrams=3)
dfm_tot_3gram<-dfm(token_tot_3gram,toLower = FALSE)
topwords_tot_3gram<-data.frame(text=names(topfeatures(dfm_tot_3gram,dfm_tot_3gram@Dim[2])),Ncount=topfeatures(dfm_tot_3gram,dfm_tot_3gram@Dim[2]))
topwords_tot_3gram[1:10,]
sumtot<-summarise(topwords_tot_3gram,sum(Ncount)) #(gelijk aan aantal features)
topwords_tot_3gram<-mutate(topwords_tot_3gram,perc=Ncount*100/as.numeric(sumtot))
topwords_tot_3gram$csum<-cumsum(topwords_tot_3gram$perc) 
tail(topwords_tot_3gram)
toc()
plot(1:nrow(topwords_tot_3gram),topwords_tot_3gram$csum,main="Coverage by 3-grams",xlab="Number of 3-grams",ylab="Percentage of total text covered")
saveRDS(dfm_tot_3gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_tot_3gram.RDS"))
rm(dfm_tot_3gram)
dfm_tot_3gram<-readRDS("20170207dfm_tot_3gram.RDS")


tic()
token_tot_4gram<-tokenize(corpustotaal,removeNumbers=TRUE,removePunct=TRUE,
                          removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE,ngrams=4)
dfm_tot_4gram<-dfm(token_tot_4gram,toLower = FALSE)
toc()
topwords_tot_4gram<-data.frame(text=names(topfeatures(dfm_tot_4gram,dfm_tot_4gram@Dim[2])),Ncount=topfeatures(dfm_tot_4gram,dfm_tot_4gram@Dim[2]))
topwords_tot_4gram[1:10,]
sumtot<-summarise(topwords_tot_4gram,sum(Ncount)) #(gelijk aan aantal features)
topwords_tot_4gram<-mutate(topwords_tot_4gram,perc=Ncount*100/as.numeric(sumtot))
topwords_tot_4gram$csum<-cumsum(topwords_tot_4gram$perc) 
tail(topwords_tot_4gram)
plot(1:nrow(topwords_tot_4gram),topwords_tot_4gram$csum,main="Coverage by 4-grams",xlab="Number of 4-grams",ylab="Percentage of total text covered")
saveRDS(dfm_tot_4gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_tot_4gram.RDS"))
rm(dfm_tot_4gram)
dfm_tot_4gram<-readRDS("20170207dfm_tot_4gram.RDS")


#also for the subsets
dfm_twitter<-dfm(token_twitter_stripped,toLower=FALSE)
topwords_twitter<-data.frame(text=names(topfeatures(dfm_twitter,dfm_twitter@Dim[2])),Ncount=topfeatures(dfm_twitter,dfm_twitter@Dim[2]))
sumtot<-summarise(topwords_twitter,sum(Ncount))
topwords_twitter<-mutate(topwords_twitter,perc=Ncount*100/as.numeric(sumtot))
topwords_twitter$csum<-cumsum(topwords_twitter$perc)
dfm_twitter_stop <- removeFeatures(dfm_twitter, stopwords("english"))
topwords_twitter_stop<-data.frame(text=names(topfeatures(dfm_twitter_stop,dfm_twitter_stop@Dim[2])),Ncount=topfeatures(dfm_twitter_stop,dfm_twitter_stop@Dim[2]))
sumtot<-summarise(topwords_twitter_stop,sum(Ncount))  #(gelijk aan aantal features)
topwords_twitter_stop<-mutate(topwords_twitter_stop,perc=Ncount*100/as.numeric(sumtot))
topwords_twitter_stop$csum<-cumsum(topwords_twitter_stop$perc) 
saveRDS(dfm_twitter,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_twitter.RDS"))
rm(dfm_twitter,dfm_twitter_stop)

dfm_news<-dfm(token_news_stripped,toLower=FALSE)
topwords_news<-data.frame(text=names(topfeatures(dfm_news,dfm_news@Dim[2])),Ncount=topfeatures(dfm_news,dfm_news@Dim[2]))
sumtot<-summarise(topwords_news,sum(Ncount))
topwords_news<-mutate(topwords_news,perc=Ncount*100/as.numeric(sumtot))
topwords_news$csum<-cumsum(topwords_news$perc) 
dfm_news_stop <- removeFeatures(dfm_news, stopwords("english"))
topwords_news_stop<-data.frame(text=names(topfeatures(dfm_news_stop,dfm_news_stop@Dim[2])),Ncount=topfeatures(dfm_news_stop,dfm_news_stop@Dim[2]))
sumtot<-summarise(topwords_news_stop,sum(Ncount))  #(gelijk aan aantal features)
topwords_news_stop<-mutate(topwords_news_stop,perc=Ncount*100/as.numeric(sumtot))
topwords_news_stop$csum<-cumsum(topwords_news_stop$perc) 
saveRDS(dfm_news,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_news.RDS"))
rm(dfm_news,dfm_news_stop)

dfm_blogs<-dfm(token_blogs_stripped,toLower=FALSE)
topwords_blogs<-data.frame(text=names(topfeatures(dfm_blogs,dfm_blogs@Dim[2])),Ncount=topfeatures(dfm_blogs,dfm_blogs@Dim[2]))
sumtot<-summarise(topwords_blogs,sum(Ncount))
topwords_blogs<-mutate(topwords_blogs,perc=Ncount*100/as.numeric(sumtot))
topwords_blogs$csum<-cumsum(topwords_blogs$perc) 
dfm_blogs_stop <- removeFeatures(dfm_blogs, stopwords("english"))
topwords_blogs_stop<-data.frame(text=names(topfeatures(dfm_blogs_stop,dfm_blogs_stop@Dim[2])),Ncount=topfeatures(dfm_blogs_stop,dfm_blogs_stop@Dim[2]))
sumtot<-summarise(topwords_blogs_stop,sum(Ncount))  #(gelijk aan aantal features)
topwords_blogs_stop<-mutate(topwords_blogs_stop,perc=Ncount*100/as.numeric(sumtot))
topwords_blogs_stop$csum<-cumsum(topwords_blogs_stop$perc) 
saveRDS(dfm_blogs,paste0(gsub("[[:punct:]]","",Sys.Date()),"dfm_blogs.RDS"))
rm(dfm_blogs,dfm_blogs_stop)


#plots for the different corpus
plot(topwords_tot$perc[1:20], type="b",main="Top 20 features in total corpus",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_tot$text[1:20],las=2,cex.axis=0.7)

plot(topwords_twitter$perc[1:20], type="b",main="Top 20 features in twitter corpus",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_twitter$text[1:20],las=2,cex.axis=0.7)
plot(topwords_news$perc[1:20], type="b",main="Top 20 features in news corpus",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_news$text[1:20],las=2,cex.axis=0.7)
plot(topwords_blogs$perc[1:20], type="b",main="Top 20 features in blogs corpus",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_blogs$text[1:20],las=2,cex.axis=0.7)

#plots for the different corpus when stopwords are removed
plot(topwords_tot_stop$perc[1:20], type="b",main="Top 20 features in total corpus (no stopwords)",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_tot_stop$text[1:20],las=2,cex.axis=0.7)

plot(topwords_twitter_stop$perc[1:20], type="b",main="Top 20 features in twitter corpus (no stopwords)",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_twitter_stop$text[1:20],las=2,cex.axis=0.7)
plot(topwords_news_stop$perc[1:20], type="b",main="Features in news corpus (no stopwords)",
     xlab="Top Twenty Words", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_news_stop$text[1:20],las=2,cex.axis=0.7)
plot(topwords_blogs_stop$perc[1:20], type="b",main="Top 20 features in blogs corpus (no stopwords)",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_blogs_stop$text[1:20],las=2,cex.axis=0.7)

#plots for the 2-grams and 3-grams
par(mar=c(8.1,4.1,4.1,2.1))
plot(topwords_tot_2gram$perc[1:20], type="b",main="Top 20 2-grams in total corpus",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_tot_2gram$text[1:20],las=2,cex.axis=0.55)
plot(topwords_tot_3gram$perc[1:20], type="b",main="Top 20 3-grams in total corpus",
     xlab="", ylab="Percentage of Full Text", xaxt ="n",cex.lab=1, cex.axis=1)
axis(1, 1:20, labels = topwords_tot_3gram$text[1:20],las=2,cex.axis=0.55)


#check if words are english, remove words from feature list that are foreign
#leave EndofSentence and Questionmark in it!
#First I delete the words that occur less than 24 times, since they are not usefull and save a lot of time.
#For the 2-grams I want them to occur at least 10 times, for 3 and 4-grams 5 times.
tic()
wordlist_US<-read.csv("wordlist_US.txt",quote = "",row.names = NULL,stringsAsFactors = FALSE)

topwords_tot$text<-as.character(topwords_tot$text)
topwords_tot<-filter(topwords_tot,Ncount>24)
topwords_tot$lower<-tolower(topwords_tot$text)
topwords_tot$english<-NA
topwords_tot$include<-0
for (i in 1:nrow(topwords_tot)) {
    topwords_tot$english[i]<-topwords_tot$lower[i] %in% wordlist_US$X.c
    if (topwords_tot$english[i]==TRUE | topwords_tot$text[i] %in% c("EndOfSentence","QuestionMark")) {
        topwords_tot$include[i]<-1

    }
}

##same for the ngrams
#First I have to split the ngrams in separate words
topwords_tot_2gram$text<-as.character(topwords_tot_2gram$text)
topwords_tot_2gram<-filter(topwords_tot_2gram,Ncount>4)
for (i in 1:nrow(topwords_tot_2gram)) {
    topwords_tot_2gram$word1[i]<-unlist(strsplit(topwords_tot_2gram$text[i],"_"))[1]
    topwords_tot_2gram$word2[i]<-unlist(strsplit(topwords_tot_2gram$text[i],"_"))[2]
}
#then check if the words are english
topwords_tot_2gram$lower1<-tolower(topwords_tot_2gram$word1)
topwords_tot_2gram$lower2<-tolower(topwords_tot_2gram$word2)
topwords_tot_2gram$english1<-NA
topwords_tot_2gram$english2<-NA
topwords_tot_2gram$include<-0
for (i in 1:nrow(topwords_tot_2gram)) {
    topwords_tot_2gram$english1[i]<-topwords_tot_2gram$lower1[i] %in% wordlist_US$X.c
    topwords_tot_2gram$english2[i]<-topwords_tot_2gram$lower2[i] %in% wordlist_US$X.c
    if ((topwords_tot_2gram$english1[i]==TRUE & topwords_tot_2gram$english2[i]==TRUE) | 
        (topwords_tot_2gram$word1[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_2gram$english2[i]==TRUE ) |
        (topwords_tot_2gram$word2[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_2gram$english1[i]==TRUE ))
        {
        topwords_tot_2gram$include[i]<-1
        
    }
}

topwords_tot_3gram$text<-as.character(topwords_tot_3gram$text)
topwords_tot_3gram<-filter(topwords_tot_3gram,Ncount>3)
for (i in 1:nrow(topwords_tot_3gram)) {
    topwords_tot_3gram$word1[i]<-unlist(strsplit(topwords_tot_3gram$text[i],"_"))[1]
    topwords_tot_3gram$word2[i]<-unlist(strsplit(topwords_tot_3gram$text[i],"_"))[2]
    topwords_tot_3gram$word3[i]<-unlist(strsplit(topwords_tot_3gram$text[i],"_"))[3]
}
topwords_tot_3gram$lower1<-tolower(topwords_tot_3gram$word1)
topwords_tot_3gram$lower2<-tolower(topwords_tot_3gram$word2)
topwords_tot_3gram$lower3<-tolower(topwords_tot_3gram$word3)
topwords_tot_3gram$english1<-NA
topwords_tot_3gram$english2<-NA
topwords_tot_3gram$english3<-NA
topwords_tot_3gram$include<-0
for (i in 1:nrow(topwords_tot_3gram)) {
    topwords_tot_3gram$english1[i]<-topwords_tot_3gram$lower1[i] %in% wordlist_US$X.c
    topwords_tot_3gram$english2[i]<-topwords_tot_3gram$lower2[i] %in% wordlist_US$X.c
    topwords_tot_3gram$english3[i]<-topwords_tot_3gram$lower3[i] %in% wordlist_US$X.c
    if ((topwords_tot_3gram$english1[i]==TRUE & topwords_tot_3gram$english2[i]==TRUE & topwords_tot_3gram$english3[i]==TRUE) | 
        (topwords_tot_3gram$word1[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_3gram$english2[i]==TRUE & topwords_tot_3gram$english3[i]==TRUE) |
        (topwords_tot_3gram$word2[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_3gram$english1[i]==TRUE & topwords_tot_3gram$english3[i]==TRUE) |
        (topwords_tot_3gram$word3[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_3gram$english1[i]==TRUE & topwords_tot_3gram$english2[i]==TRUE) )
        
    {
        topwords_tot_3gram$include[i]<-1
        
    }
}
toc()
topwords_tot_4gram$text<-as.character(topwords_tot_4gram$text)
topwords_tot_4gram<-filter(topwords_tot_4gram,Ncount>2)
for (i in 1:nrow(topwords_tot_4gram)) {
    topwords_tot_4gram$word1[i]<-unlist(strsplit(topwords_tot_4gram$text[i],"_"))[1]
    topwords_tot_4gram$word2[i]<-unlist(strsplit(topwords_tot_4gram$text[i],"_"))[2]
    topwords_tot_4gram$word3[i]<-unlist(strsplit(topwords_tot_4gram$text[i],"_"))[3]
    topwords_tot_4gram$word4[i]<-unlist(strsplit(topwords_tot_4gram$text[i],"_"))[4]
    }
topwords_tot_4gram$lower1<-tolower(topwords_tot_4gram$word1)
topwords_tot_4gram$lower2<-tolower(topwords_tot_4gram$word2)
topwords_tot_4gram$lower3<-tolower(topwords_tot_4gram$word3)
topwords_tot_4gram$lower4<-tolower(topwords_tot_4gram$word4)
topwords_tot_4gram$english1<-NA
topwords_tot_4gram$english2<-NA
topwords_tot_4gram$english3<-NA
topwords_tot_4gram$english4<-NA
topwords_tot_4gram$include<-0

for (i in 1:nrow(topwords_tot_4gram)) {
    topwords_tot_4gram$english1[i]<-topwords_tot_4gram$lower1[i] %in% wordlist_US$X.c
    topwords_tot_4gram$english2[i]<-topwords_tot_4gram$lower2[i] %in% wordlist_US$X.c
    topwords_tot_4gram$english3[i]<-topwords_tot_4gram$lower3[i] %in% wordlist_US$X.c
    topwords_tot_4gram$english4[i]<-topwords_tot_4gram$lower4[i] %in% wordlist_US$X.c
    if ((topwords_tot_4gram$english1[i]==TRUE & topwords_tot_4gram$english2[i]==TRUE & topwords_tot_4gram$english3[i]==TRUE & topwords_tot_4gram$english4[i]==TRUE) | 
        (topwords_tot_4gram$word1[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_4gram$english2[i]==TRUE & topwords_tot_4gram$english3[i]==TRUE & topwords_tot_4gram$english4[i]==TRUE) |
        (topwords_tot_4gram$word2[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_4gram$english1[i]==TRUE & topwords_tot_4gram$english3[i]==TRUE & topwords_tot_4gram$english4[i]==TRUE) |
        (topwords_tot_4gram$word3[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_4gram$english1[i]==TRUE & topwords_tot_4gram$english2[i]==TRUE & topwords_tot_4gram$english4[i]==TRUE) |
        (topwords_tot_4gram$word4[i] %in% c("EndOfSentence","QuestionMark") & topwords_tot_4gram$english1[i]==TRUE & topwords_tot_4gram$english2[i]==TRUE & topwords_tot_4gram$english3[i]==TRUE))
        
    {
        topwords_tot_4gram$include[i]<-1
        
    }
}
save.image("topwords1234.RData")
load("topwords1234v2.Rdata")

#select only correct words
topwords_tot_sel<-filter(topwords_tot,include==1)
topwords_tot_2gram_sel<-filter(topwords_tot_2gram,include==1)
#topwords_tot_2gram_sel<-rename(topwords_tot_2gram_sel,Ncount=count)
topwords_tot_3gram_sel<-filter(topwords_tot_3gram,include==1)
#topwords_tot_3gram_sel<-rename(topwords_tot_3gram_sel,Ncount=count)
topwords_tot_4gram_sel<-filter(topwords_tot_4gram,include==1)
#topwords_tot_4gram_sel<-rename(topwords_tot_4gram_sel,Ncount=count)
topwords_tot_3gram_sel$X<-paste(topwords_tot_3gram_sel$word1,topwords_tot_3gram_sel$word2," ")
matrix_2gram<-select(topwords_tot_2gram_sel,word1,word2,Ncount)
matrix_2gram<-arrange(matrix_2gram,word1,desc(Ncount))
matrix_2gram<-matrix_2gram %>% group_by(word1) %>% mutate(id = seq_len(n()))
matrix_2gram<-filter(matrix_2gram,id<6)
matrix_2gram<-matrix_2gram %>% group_by(word1) %>% mutate(choice1=word2[id==1],choice2=if (max(id)>1) word2[id==2] else NA, choice3=if (max(id)>2) word2[id==3] else NA,
                                                          choice4=if (max(id)>3) word2[id==4] else NA,choice5=if (max(id)>4) word2[id==5] else NA)
matrix_2gram<-filter(matrix_2gram,id==1)
matrix_2gram<-select(matrix_2gram,-Ncount,-id,-word2)
matrix_2gram<-rename(matrix_2gram,feature=word1)
saveRDS(matrix_2gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"matrix_2gram.RDS"))

matrix_3gram<-select(topwords_tot_3gram_sel,word1,word2,word3,Ncount)
matrix_3gram<-mutate(matrix_3gram,feature=paste0(word1,"_",word2))
matrix_3gram<-arrange(matrix_3gram,feature,desc(Ncount))
matrix_3gram<-matrix_3gram %>% group_by(feature) %>% mutate(id = seq_len(n()))
matrix_3gram<-filter(matrix_3gram,id<6)
matrix_3gram<-matrix_3gram %>% group_by(feature) %>% mutate(choice1=word3[id==1],choice2=if (max(id)>1) word3[id==2] else NA, choice3=if (max(id)>2) word3[id==3] else NA,
                                                            choice4=if (max(id)>3) word3[id==4] else NA,choice5=if (max(id)>4) word3[id==5] else NA)
matrix_3gram<-filter(matrix_3gram,id==1)
matrix_3gram<-select(matrix_3gram,feature,choice1,choice2,choice3,choice4,choice5)
saveRDS(matrix_3gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"matrix_3gram.RDS"))

matrix_4gram<-select(topwords_tot_4gram_sel,word1,word2,word3,word4,Ncount)
matrix_4gram<-mutate(matrix_4gram,feature=paste0(word1,"_",word2,"_",word3))
matrix_4gram<-arrange(matrix_4gram,feature,desc(Ncount))
matrix_4gram<-matrix_4gram %>% group_by(feature) %>% mutate(id = seq_len(n()))
matrix_4gram<-filter(matrix_4gram,id<4)
matrix_4gram<-matrix_4gram %>% group_by(feature) %>% mutate(choice1=word4[id==1],choice2=if (max(id)>1) word4[id==2] else NA, choice3=if (max(id)>2) word4[id==3] else NA,
                                                            choice4=if (max(id)>3) word4[id==4] else NA,choice5=if (max(id)>4) word4[id==5] else NA)
matrix_4gram<-filter(matrix_4gram,id==1)
matrix_4gram<-select(matrix_4gram,feature,choice1,choice2,choice3,choice4,choice5)
saveRDS(matrix_4gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"matrix_4gram.RDS"))

matrix_1gram<-data.frame(choice1=topwords_tot_sel$text[1],choice2=topwords_tot_sel$text[2],choice3=topwords_tot_sel$text[3],choice4=topwords_tot_sel$text[4],choice5=topwords_tot_sel$text[5])
saveRDS(matrix_1gram,paste0(gsub("[[:punct:]]","",Sys.Date()),"matrix_1gram.RDS"))


save.image("topwords1234v2.RData")
load("topwords1234v2.Rdata")

