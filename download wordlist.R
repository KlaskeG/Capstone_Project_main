download.file("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt","wordlist_US.txt")
wordlist_US<-read.csv("wordlist_US.txt")

setwd("D:/Coursera Courses/Data Science Specialisation/Capstone Project/afscheid Kirsten/txts/")
download.file("http://www.justpain.com/ut_maps/wordlists/words.dutch.txt","wordlist_NL.txt")
wordlist_NL<-read.csv("wordlist_NL.txt")
