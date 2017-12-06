# quanteda package

#halve sample
library(quanteda)
library(dplyr)
library(tictoc)
setwd("D:/Coursera Courses/Data Science Specialisation/Capstone Project/data/sample_half2/")
blogs <- textfile("blogs_half2_test.txt")
twitter <- textfile("twitter_half2_test.txt")
news <- textfile("news_half2_test.txt")

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


saveRDS(dfm_tot,"dfm_tot_test.RDS")
#dfm_tot<-readRDS("dfm_tot_test.RDS")
#evt save.image("text.RData") vs load("text.RData") workspace
rm(dfm_tot)
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
saveRDS(dfm_tot_2gram,"dfm_tot_2gram_test.RDS")
#rm(dfm_tot_2gram)
#dfm_tot_2gram<-readRDS("dfm_tot_2gram_test.RDS")

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
saveRDS(dfm_tot_3gram,"dfm_tot_3gram_test.RDS")
#rm(dfm_tot_3gram)
#dfm_tot_3gram<-readRDS("dfm_tot_3gram_test.RDS")


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
saveRDS(dfm_tot_4gram,"dfm_tot_4gram_test.RDS")
#rm(dfm_tot_4gram)
#dfm_tot_4gram<-readRDS("dfm_tot_4gram_test.RDS")




#check if words are english, remove words from feature list that are foreign
#leave EndofSentence and Questionmark in it!
#First I delete the words that occur less than 24 times, since they are not usefull and save a lot of time.
#For the 2-grams I want them to occur at least 10 times, for 3 and 4-grams 5 times.
tic()
wordlist_US<-read.csv("wordlist_US.txt",quote = "",row.names = NULL,stringsAsFactors = FALSE)

topwords_tot$text<-as.character(topwords_tot$text)
#topwords_tot<-filter(topwords_tot,Ncount>24)
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
#topwords_tot_2gram<-filter(topwords_tot_2gram,Ncount>4)
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
#topwords_tot_3gram<-filter(topwords_tot_3gram,Ncount>3)
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
#topwords_tot_4gram<-filter(topwords_tot_4gram,Ncount>2)
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
save.image("topwords1234_test.RData")
#load("topwords1234v2.Rdata")

#select only correct words
topwords_tot_sel<-filter(topwords_tot,include==1)
topwords_tot_2gram_sel<-filter(topwords_tot_2gram,include==1)
topwords_tot_3gram_sel<-filter(topwords_tot_3gram,include==1)
topwords_tot_4gram_sel<-filter(topwords_tot_4gram,include==1)
#topwords_tot_3gram_sel$X<-paste(topwords_tot_3gram_sel$word1,topwords_tot_3gram_sel$word2," ")


matrix_2gram_test<-select(topwords_tot_2gram_sel,word1,word2,Ncount)
saveRDS(matrix_2gram_test,"matrix_2gram_test.RDS")

matrix_3gram_test<-select(topwords_tot_3gram_sel,word1,word2,word3,Ncount)
matrix_3gram_test<-mutate(matrix_3gram_test,feature=paste0(word1,"_",word2))
matrix_3gram_test<-select(matrix_3gram_test,feature,word3,Ncount)

saveRDS(matrix_3gram_test,"matrix_3gram_test.RDS")

matrix_4gram_test<-select(topwords_tot_4gram_sel,word1,word2,word3,word4,Ncount)
matrix_4gram_test<-mutate(matrix_4gram_test,feature=paste0(word1,"_",word2,"_",word3))
matrix_4gram_test<-select(matrix_4gram_test,feature,word4,Ncount)
saveRDS(matrix_4gram_test,"matrix_4gram_test.RDS")

save.image("preparation testset.RData")
#load("preparation testset.Rdata")
rm(dfm_tot,dfm_tot_2gram,dfm_tot_3gram,dfm_tot_4gram)
rm(token_tot_2gram,token_tot_3gram,token_tot_4gram)
rm(topwords_tot,topwords_tot_4gram,topwords_tot_3gram,topwords_tot_2gram)
source("predict function testset.R")