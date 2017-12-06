#platte tekst inladen en bekijken, kleinere testbestanden maken
setwd("D:/Coursera Courses/Data Science Specialisation/Capstone Project/data/final/en_US/")

con <- file("en_US.twitter.txt", "r") 
#line1<-readLines(con, 1) ## Read the first line of text 
#line2<-readLines(con, 1) ## Read the next line of text 
#line3_7<-readLines(con, 5) ## Read in the next 5 lines of text 
twitter<-readLines(con)
close(con)

con <- file("en_US.blogs.txt", "r") 
blogs<-readLines(con)
close(con)

con <- file("en_US.news.txt", "r") 
news<-readLines(con)
close(con)

length(twitter)
length(blogs)
length(news)
twitter[1:10]

max(nchar(twitter))
max(nchar(blogs))
max(nchar(news))

test<-grep("â",twitter,value=TRUE) #niet te herstellen
love<-grep("love",twitter,value=TRUE)
hate<-grep("hate",twitter,value=TRUE)
length(love)/length(hate) #4,1 (alle matches, dus ook "loves", "hates", etc)
love2<-grep("[^a-zA-Z]love[^a-zA-Z]",twitter,value=TRUE)
hate2<-grep("[^a-zA-Z]hate[^a-zA-Z]",twitter,value=TRUE)
length(love2)/length(hate2) #4,8 (love en hate met ervoor en erachter geen letters)

biostats<-grep("biostats",twitter,value=TRUE)
#biostats
#[1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

match<-grep("A computer once beat me at chess, but it was no match for me at kickboxing",twitter,value=TRUE)
match #3x.

#kleinere subsets maken.
setwd("D:/Coursera Courses/Data Science Specialisation/Capstone Project/data/sample100/")

set.seed(1000)
id_twitter<-sample(1:length(twitter),size=100,replace=FALSE)
twitter_sample<-twitter[id_twitter]
fileConn<-file("twitter_100.txt")
writeLines(twitter_sample, fileConn)
close(fileConn)

set.seed(2000)
id_blogs<-sample(1:length(blogs),size=100,replace=FALSE)
blogs_sample<-blogs[id_blogs]
fileConn<-file("blogs_100.txt")
writeLines(blogs_sample, fileConn)
close(fileConn)

set.seed(3000)
id_news<-sample(1:length(news),size=100,replace=FALSE)
news_sample<-blogs[id_news]
fileConn<-file("news_100.txt")
writeLines(news_sample, fileConn)
close(fileConn)

#helft van data selecteren in trainingset.
setwd("D:/Coursera Courses/Data Science Specialisation/Capstone Project/data/sample_half2/")
#half: /200.
#half2: /50.
#half3: /10.

set.seed(1000)
id_twitter<-sample(1:length(twitter),size=length(twitter)/50,replace=FALSE)
twitter_sample<-twitter[id_twitter]
twitter_test<-twitter[-id_twitter]
#te groot om compleet mee te nemen, hier weer selectie van maken.
id_twitter_test<-sample(1:length(twitter_test),size=length(twitter)/200,replace=FALSE)
twitter_test<-twitter_test[id_twitter_test]
fileConn<-file("twitter_half2.txt")
writeLines(twitter_sample, fileConn)
close(fileConn)
fileConn<-file("twitter_half2_test.txt")
writeLines(twitter_test, fileConn)
close(fileConn)

set.seed(2000)
id_blogs<-sample(1:length(blogs),size=length(blogs)/50,replace=FALSE)
blogs_sample<-blogs[id_blogs]
blogs_test<-blogs[-id_blogs]
id_blogs_test<-sample(1:length(blogs_test),size=length(blogs)/200,replace=FALSE)
blogs_test<-blogs_test[id_blogs_test]
fileConn<-file("blogs_half2.txt")
writeLines(blogs_sample, fileConn)
close(fileConn)
fileConn<-file("blogs_half2_test.txt")
writeLines(blogs_test, fileConn)
close(fileConn)

set.seed(3000)
id_news<-sample(1:length(news),size=length(news)/50,replace=FALSE)
news_sample<-news[id_news]
news_test<-news[-id_news]
id_news_test<-sample(1:length(news_test),size=length(news)/200,replace=FALSE)
news_test<-news_test[id_news_test]
fileConn<-file("news_half2.txt")
writeLines(news_sample, fileConn)
close(fileConn)
fileConn<-file("news_half2_test.txt")
writeLines(news_test, fileConn)
close(fileConn)
length(blogs_sample) #17985 (half2), 89928 (half3)
length(twitter_sample) #47202 (half2),236014 (half3)
length(news_sample) #1545 (half2), 7725 (half3)

length(blogs_test) #4496 (half2)
length(twitter_test) #47202 (half2)
length(news_test) #386 (half2)

#test of de latere trainingset inderdaad gelijk is aan de oorspronkelijke, zodat testset (later pas gemaakt) echt niet overlapt met trainingset
con <- file("news_half2.txt", "r") 
news_training<-readLines(con)
close(con)
identical(news_training,news_sample)
#dit is het geval, behalve voor de news set, hier is per ongeluk de trainingset geselecteerd uit de blogs set.
#hier dus per definitie een andere testset.