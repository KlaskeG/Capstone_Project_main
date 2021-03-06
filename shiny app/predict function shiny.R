#function n-grams predictor
matrix_1gram<-readRDS("20170208matrix_1gram.RDS")
matrix_2gram<-readRDS("20170208matrix_2gram.RDS")
matrix_3gram<-readRDS('20171202matrix_3gram.RDS')
matrix_4gram<-readRDS("20171202matrix_4gram.RDS")

search4gram<-function(text) {
    text <-stringi::stri_replace_all_regex(text,"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\.", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\n", " EndOfSentence ",vectorize_all = FALSE)
    words<-tokenize(text,removeNumbers=TRUE,removePunct=TRUE,
                    removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)
    word1<-words[[1]][length(words[[1]])-2]
    word2<-words[[1]][length(words[[1]])-1]
    word3<-words[[1]][length(words[[1]])]
    feature<-paste0(word1,"_",word2,"_",word3)
    choice4gram<-data.frame(choice1=NA,choice2=NA,choice3=NA,choice4=NA,choice5=NA)
    choice4gram<-choice4gram %>% mutate(choice1=if(length(matrix_4gram$choice1[matrix_4gram$feature==feature])>0) matrix_4gram$choice1[matrix_4gram$feature==feature] else NA,
                                        choice2=if(length(matrix_4gram$choice2[matrix_4gram$feature==feature])>0) matrix_4gram$choice2[matrix_4gram$feature==feature] else NA,
                                        choice3=if(length(matrix_4gram$choice3[matrix_4gram$feature==feature])>0) matrix_4gram$choice3[matrix_4gram$feature==feature] else NA,
                                        choice4=if(length(matrix_4gram$choice4[matrix_4gram$feature==feature])>0) matrix_4gram$choice4[matrix_4gram$feature==feature] else NA,
                                        choice5=if(length(matrix_4gram$choice5[matrix_4gram$feature==feature])>0) matrix_4gram$choice5[matrix_4gram$feature==feature] else NA)
    
    choice4gram
}

search3gram<-function(text) {
    text <-stringi::stri_replace_all_regex(text,"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\.", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\n", " EndOfSentence ",vectorize_all = FALSE)
    words<-tokenize(text,removeNumbers=TRUE,removePunct=TRUE,
                    removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)
    word1<-words[[1]][length(words[[1]])-1]
    word2<-words[[1]][length(words[[1]])]
    feature<-paste0(word1,"_",word2)
    choice3gram<-data.frame(choice1=NA,choice2=NA,choice3=NA,choice4=NA,choice5=NA)
    choice3gram<-choice3gram %>% mutate(choice1=if(length(matrix_3gram$choice1[matrix_3gram$feature==feature])>0) matrix_3gram$choice1[matrix_3gram$feature==feature] else NA,
                                        choice2=if(length(matrix_3gram$choice2[matrix_3gram$feature==feature])>0) matrix_3gram$choice2[matrix_3gram$feature==feature] else NA,
                                        choice3=if(length(matrix_3gram$choice3[matrix_3gram$feature==feature])>0) matrix_3gram$choice3[matrix_3gram$feature==feature] else NA,
                                        choice4=if(length(matrix_3gram$choice4[matrix_3gram$feature==feature])>0) matrix_3gram$choice4[matrix_3gram$feature==feature] else NA,
                                        choice5=if(length(matrix_3gram$choice5[matrix_3gram$feature==feature])>0) matrix_3gram$choice5[matrix_3gram$feature==feature] else NA)
    choice3gram
}

search2gram<-function(text) {
    text <-stringi::stri_replace_all_regex(text,"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\.", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\n", " EndOfSentence ",vectorize_all = FALSE)
    words<-tokenize(text,removeNumbers=TRUE,removePunct=TRUE,
                    removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)
    feature<-words[[1]][length(words[[1]])]
    choice2gram<-data.frame(choice1=NA,choice2=NA,choice3=NA,choice4=NA,choice5=NA)
    choice2gram<-choice2gram %>% mutate(choice1=if(length(matrix_2gram$choice1[matrix_2gram$feature==feature])>0) matrix_2gram$choice1[matrix_2gram$feature==feature] else NA,
                                        choice2=if(length(matrix_2gram$choice2[matrix_2gram$feature==feature])>0) matrix_2gram$choice2[matrix_2gram$feature==feature] else NA,
                                        choice3=if(length(matrix_2gram$choice3[matrix_2gram$feature==feature])>0) matrix_2gram$choice3[matrix_2gram$feature==feature] else NA,
                                        choice4=if(length(matrix_2gram$choice3[matrix_2gram$feature==feature])>0) matrix_2gram$choice4[matrix_2gram$feature==feature] else NA,
                                        choice5=if(length(matrix_2gram$choice3[matrix_2gram$feature==feature])>0) matrix_2gram$choice5[matrix_2gram$feature==feature] else NA)
    choice2gram
}


predict_word<-function(text,N) {
    text <-stringi::stri_replace_all_regex(text,"\\.\\n", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\.", " EndOfSentence ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\?\\n", " QuestionMark ",vectorize_all = FALSE)
    text <-stringi::stri_replace_all_regex(text,"\\n", " EndOfSentence ",vectorize_all = FALSE)
    words<-tokenize(text,removeNumbers=TRUE,removePunct=TRUE,
                    removeSymbols=TRUE,removeTwitter=TRUE,removeURL=TRUE)
    choice<-data.frame(choice1=NA,choice2=NA,choice3=NA,choice4=NA,choice5=NA)
    if (length(words[[1]])>2) {
        output_4gram<-search4gram(text)
        output_3gram<-search3gram(text)
        output_2gram<-search2gram(text)
        combined<-data.frame(c1=output_4gram$choice1,c2=output_4gram$choice2,c3=output_4gram$choice3,d1=output_4gram$choice4,d2=output_4gram$choice5,c4=output_3gram$choice1,c5=output_3gram$choice2,c6=output_3gram$choice3,e1=output_3gram$choice4,e2=output_3gram$choice5,c7=output_2gram$choice1,c8=output_2gram$choice2,c9=output_2gram$choice3,f1=output_2gram$choice4,f2=output_2gram$choice5,c10=matrix_1gram$choice1,c11=matrix_1gram$choice2,c12=matrix_1gram$choice3,g1=matrix_1gram$choice4,g2=matrix_1gram$choice5)
        combined <- combined[,colSums(is.na(combined))<nrow(combined)]
        duplicated.columns<-duplicated(t(combined))
        combined<-combined[,!duplicated.columns]
        choice<-choice %>% mutate(choice1=as.character(combined[1,1]),choice2=as.character(combined[1,2]),choice3=as.character(combined[1,3]),choice4=as.character(combined[1,4]),choice5=as.character(combined[1,5]))
        
            }
    else if (length(words[[1]])>1) {
        output_3gram<-search3gram(text)
        output_2gram<-search2gram(text)
        combined<-data.frame(c4=output_3gram$choice1,c5=output_3gram$choice2,c6=output_3gram$choice3,e1=output_3gram$choice4,e2=output_3gram$choice5,c7=output_2gram$choice1,c8=output_2gram$choice2,c9=output_2gram$choice3,f1=output_2gram$choice4,f2=output_2gram$choice5,c10=matrix_1gram$choice1,c11=matrix_1gram$choice2,c12=matrix_1gram$choice3,g1=matrix_1gram$choice4,g2=matrix_1gram$choice5)
        combined <- combined[,colSums(is.na(combined))<nrow(combined)]
        duplicated.columns<-duplicated(t(combined))
        combined<-combined[,!duplicated.columns]
        choice<-choice %>% mutate(choice1=as.character(combined[1,1]),choice2=as.character(combined[1,2]),choice3=as.character(combined[1,3]),choice4=as.character(combined[1,4]),choice5=as.character(combined[1,5]))
        
    }
    else if (length(words[[1]])==1) {
        output_2gram<-search2gram(text)
        combined<-data.frame(c7=output_2gram$choice1,c8=output_2gram$choice2,c9=output_2gram$choice3,f1=output_2gram$choice4,f2=output_2gram$choice5,c10=matrix_1gram$choice1,c11=matrix_1gram$choice2,c12=matrix_1gram$choice3,g1=matrix_1gram$choice4,g2=matrix_1gram$choice5)
        combined <- combined[,colSums(is.na(combined))<nrow(combined)]
        duplicated.columns<-duplicated(t(combined))
        combined<-combined[,!duplicated.columns]
        choice<-choice %>% mutate(choice1=as.character(combined[1,1]),choice2=as.character(combined[1,2]),choice3=as.character(combined[1,3]),choice4=as.character(combined[1,4]),choice5=as.character(combined[1,5]))
    }
    else {
        choice$choice1<-"I"
        choice$choice2<-""
        choice$choice3<-""
        choice$choice4<-""
        choice$choice5<-""
    }
    choiceN<-as.character(choice[N])
    choiceN <-stringi::stri_replace_all_regex(choiceN, "EndOfSentence","\\.",vectorize_all = FALSE)
    choiceN <-stringi::stri_replace_all_regex(choiceN, "QuestionMark","\\?",vectorize_all = FALSE)

    
}



