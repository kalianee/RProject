setwd("~/Desktop/Research paper")
library(readr)
formspring_data <- read_delim("formspring_data.csv", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
View(formspring_data)
unique(formspring_data$severity1)
formspring_data$asker <- NULL
sapply(formspring_data,function(x) sum(is.na(x)))
sapply(formspring_data, class)

# replacing and labelling irrelevant data within the 'severity' attributes
formspring_data$severity1[is.na(formspring_data$severity1)] <- 0
formspring_data$severity1[formspring_data$severity1 %in% "None"] <- "0"
formspring_data$severity1[formspring_data$severity1 %in% "n/a"] <- "0"
formspring_data$severity1[formspring_data$severity1 %in% "no"] <- "0"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"Q: how many times have you been fucked?"] <- "8"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"\"\"\"\"ugly nasty ass bitch\"\"\"\"\"\"\""] <- "7"
formspring_data$severity1[formspring_data$severity1 %in% "horny"] <- "5"
formspring_data$severity1[formspring_data$severity1 %in% "fatherfucker"] <- "4"
formspring_data$severity1[formspring_data$severity1 %in% "u know if u wernt such a whore/bitch people wouldnt be haten on u okay fatass bitchh"] <- "10"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"Porch Monkey"] <- "3"
formspring_data$severity1[formspring_data$severity1 %in% "yo sexy! how asian are you? you tight girl?"] <- "10"
formspring_data$severity1[formspring_data$severity1 %in% "n/a"] <- "0"
formspring_data$severity1[formspring_data$severity1 %in% "suckin on ma tities like you wunntin me callinme fuck the pain away"] <- "10"
formspring_data$severity1[formspring_data$severity1 %in% "shes a fuking PUSSY"] <- "8"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"Bitchhh, yo momma, tranny, yall is annoyin.!\"\"\""] <- "10"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"how does it feel to have a hoe for a sister ?" ] <- "9"
formspring_data$severity1[formspring_data$severity1 %in% "omar be ugly"] <- "10"
formspring_data$severity1[formspring_data$severity1 %in% "wellll you can your a lil stiiick bitch"] <- "8"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"LAME BITCH, whyy are you still on my dickk wtffff\"\"\""] <- "10"
formspring_data$severity1[formspring_data$severity1 %in% "\"\"\"Q: DO YOU LIKE TIGHT PUSSSSSAYYYYS" ] <- "9"
unique(formspring_data$severity1)
formspring_data$severity1 <- as.numeric(formspring_data$severity1)
sapply(formspring_data,function(x) sum(is.na(x)))
sapply(formspring_data, class)
table(formspring_data$severity1)

unique(formspring_data$severity2)
formspring_data$severity2[is.na(formspring_data$severity2)] <- 0
formspring_data$severity2[formspring_data$severity2 %in% "None"] <- "0"
formspring_data$severity2[formspring_data$severity2 %in% "o"] <- "0"
formspring_data$severity2[formspring_data$severity2 %in% "n/a0"] <- "0"
formspring_data$severity2[formspring_data$severity2 %in% "0`"] <- "0"
unique(formspring_data$severity2)
formspring_data$severity2 <- as.numeric(formspring_data$severity2)
sapply(formspring_data,function(x) sum(is.na(x)))
sapply(formspring_data, class)
table(formspring_data$severity2)

unique(formspring_data$severity3)
formspring_data$severity3[is.na(formspring_data$severity3)] <- 0
formspring_data$severity3[formspring_data$severity3 %in% "None"] <- "0"
formspring_data$severity3[formspring_data$severity3 %in% "o"] <- "0"
formspring_data$severity3[formspring_data$severity3 %in% "N/a"] <- "0"
formspring_data$severity3[formspring_data$severity3 %in% "`0"] <- "0"
unique(formspring_data$severity3)
formspring_data$severity3 <- as.numeric(formspring_data$severity3)
sapply(formspring_data,function(x) sum(is.na(x)))
sapply(formspring_data, class)
table(formspring_data$severity3)

# Calculating average severity of 3 workers
formspring_data$Average_Severity <- as.integer((formspring_data$severity1 + formspring_data$severity2+ formspring_data$severity3)/3)
unique(formspring_data$Average_Severity)
table(formspring_data$Average_Severity)

# removing special characters and spaces
formspring_data<-data.frame(lapply(formspring_data, gsub, pattern = "[//]", replace=""))
formspring_data<-data.frame(lapply(formspring_data, gsub, pattern = "[^0-9A-Za-z/'\r\n ]", replace=""))
formspring_data$post <- as.character(formspring_data$post)
formspring_data$ques <- as.character(formspring_data$ques)
formspring_data$ans <- as.character(formspring_data$ans)

# Removing numbers and punctuation
library(tm)
formspring_data$post<-removePunctuation(formspring_data$post)
formspring_data$ques<-removePunctuation(formspring_data$ques)
formspring_data$ans<-removePunctuation(formspring_data$ans)
formspring_data$post <- removeNumbers(formspring_data$post)
formspring_data$ques <- removeNumbers(formspring_data$ques)
formspring_data$ans <- removeNumbers(formspring_data$ans)

formspring_data$Average_Severity<-as.integer(as.character(formspring_data$Average_Severity))
barplot(table(formspring_data$Average_Severity)) # as distribution of the average severity is highly imbalanced, the values other than '0' are replaced by 1. 0 representing no bullying and 1 representing bullying
formspring_data$Average_Severity[formspring_data$Average_Severity != 0] <- 1
table(formspring_data$Average_Severity)
unique(formspring_data$Average_Severity)

#Saving input file for pre-processing phase
write.csv(formspring_data, file = "formspring_data2.csv")

# using only relevant variables, these include Post Question, Post Answer and Average Severity.
formspring_data2 <- read.csv("formspring_data2.csv", row.names = NULL,header=T, sep=",")
data_QA <- formspring_data2[, -c(1:2,5:13)]
data_POST <- formspring_data2[, -c(1,3:13)]
data_POST<-data.frame(lapply(data_POST, gsub, pattern = "[Q]", replace=""))

# Converting to lower case characters
data_POST$post<-tolower(data_POST$post)
data_QA$ques<-tolower(data_QA$ques)
data_QA$ans<-tolower(data_QA$ans)

# renaming variables
library(data.table)
setnames(data_QA, old=c("ques", "ans"), new=c("Post_Ques", "Post_Ans"))
setnames(data_POST, old=c("post"), new=c("Post"))
sapply(data_QA,function(x) sum(is.na(x)))
data_QA$Average_Severity<-as.integer(as.character(data_QA$Average_Severity))
sapply(data_QA, class)
sapply(data_POST,function(x) sum(is.na(x)))
data_POST$Average_Severity<-as.integer(as.character(data_POST$Average_Severity))
sapply(data_POST, class)

#Saving Question & Answer to csv file
write.csv(data_QA, file = "data_QA.csv")
#Saving Posts to csv file
write.csv(data_POST, file = "data_POST.csv")

########### Preparing data for wordcloud
## Tokenization
library(hunspell)
tokenQues <- hunspell_parse(data_QA$Post_Ques, format = "latex")
tokenAns <- hunspell_parse(data_QA$Post_Ans, format = "latex")

########### Using the hunspell package
## Post Question
stemsQues <- unlist(hunspell_stem(unlist(tokenQues)))
wordsQues <- sort(table(stemsQues), decreasing = TRUE)
print(head(wordsQues, 30))
df <- as.data.frame(wordsQues)
df$stemsQues <- as.character(df$stemsQues)
## Stopwords
stopwords <- hunspell_parse(readLines('https://jeroen.github.io/files/stopwords.txt'))
stopsQues <- df$stemsQues %in% unlist(stopwords)
cleandata <- df[!stopsQues,]
print(cleandata, max = 30)
library(wordcloud2)
names(cleandata) <- c("Word", "Freq")
wordcloud2(cleandata)

## Post Answers
stemsAns <- unlist(hunspell_stem(unlist(tokenAns)))
wordsAns <- sort(table(stemsAns), decreasing = TRUE)
df1 <- as.data.frame(wordsAns)
df1$stemsAns <- as.character(df1$stemsAns)
## Stopwords
stopsAns <- df1$stemsAns %in% unlist(stopwords)
cleandata1 <- df1[!stopsAns,]
names(cleandata1) <- c("Word", "Freq")
wordcloud2(cleandata1)



################################# TF Term Frequencty using tm library######################
############# Building corpus for Post Question ###########################
library(tm)
corpusQues <- VCorpus(VectorSource(data_QA$Post_Ques))
corpusQues <- tm_map(corpusQues, removeWords, stopwords("english"))
#corpusQues <- tm_map(corpusQues, stemDocument)
corpusQues <- tm_map(corpusQues, stripWhitespace)
library(stringr)
removeExtraSpaces <- function(x) {gsub("\\s+", " ", str_trim(x))}
corpusQues <- tm_map(corpusQues, content_transformer(removeExtraSpaces))
corpusQues <- tm_map(corpusQues, removePunctuation)
removeHashTags <- function(x) gsub("#\\S+", "", x)
corpusQues <- tm_map(corpusQues, content_transformer(removeHashTags))
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)
corpusQues <- tm_map(corpusQues,content_transformer(removeTwitterHandles))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpusQues <- tm_map(corpusQues, content_transformer(removeURL))

###########creating term frequency for post question
dtmQues <- DocumentTermMatrix(corpusQues)
inspect(dtmQues)
dtmQues #12896 document and 13231 terms

########### Removing sparse terms from post question DTM
dtmQues_Sparse<-removeSparseTerms(dtmQues, 0.995) #12896 docs and 159 terms
inspect(dtmQues_Sparse)
dtmQues_Sparse
dtmQues.matrix<-as.matrix(dtmQues_Sparse)
dtmQues.matrix
freqQuesTF=sort(colSums(dtmQues.matrix),decreasing=TRUE)
freqQuesTF.df<-data.frame(Word=names(freqQuesTF),Freq=freqQuesTF)

########### creating TF-IDF weighting for Post Question
tfidf_Ques<-DocumentTermMatrix(corpusQues,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
inspect(tfidf_Ques)
tfidf_Ques

########### Removing sparse terms from post question TFIDF
tfidfQues_Sparse<-removeSparseTerms(tfidf_Ques, 0.995) #12896 docs and 159 terms
inspect(tfidfQues_Sparse)
tfidfQues_Sparse
tfidfQues.matrix<-as.matrix(tfidfQues_Sparse)
tfidfQues.matrix
freqQuesTFIDF=sort(colSums(tfidfQues.matrix),decreasing=TRUE)
freqQuesTFIDF.df<-data.frame(Word=names(freqQuesTFIDF),Freq=freqQuesTFIDF)
## variable to use for model are dtmQues.matrix & tfidfQues.matrix





############# Building corpus for Post Ans ###########################
corpusAns <- VCorpus(VectorSource(data_QA$Post_Ans))
corpusAns <- tm_map(corpusAns, removeWords, stopwords("english"))
#corpusAns <- tm_map(corpusAns, stemDocument)
corpusAns <- tm_map(corpusAns, stripWhitespace)
corpusAns <- tm_map(corpusAns, content_transformer(removeExtraSpaces))
corpusAns <- tm_map(corpusAns, removePunctuation)
corpusAns <- tm_map(corpusAns, content_transformer(removeHashTags))
corpusAns <- tm_map(corpusAns,content_transformer(removeTwitterHandles))
corpusAns <- tm_map(corpusAns, content_transformer(removeURL))

###########creating term frequency for post answer
dtmAns <- DocumentTermMatrix(corpusAns)
inspect(dtmAns)
dtmAns #12896 document and 12033 terms

########### Removing sparse terms from post answer DTM
dtmAns_Sparse<-removeSparseTerms(dtmAns, 0.995) 
inspect(dtmAns_Sparse)
dtmAns_Sparse #12896 docs, 176 terms
dtmAns.matrix<-as.matrix(dtmAns_Sparse) 
dtmAns.matrix
freqAnsTF=sort(colSums(dtmAns.matrix),decreasing=TRUE)
freqAnsTF.df<-data.frame(Word=names(freqAnsTF),Freq=freqAnsTF)

########### creating TF-IDF weighting for Post answer
tfidf_Ans<-DocumentTermMatrix(corpusAns,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
inspect(tfidf_Ans)
tfidf_Ans

########### Removing sparse terms from post answer TFIDF
tfidfAns_Sparse<-removeSparseTerms(tfidf_Ans, 0.995) #12896 docs and 176 terms
inspect(tfidfAns_Sparse)
tfidfAns_Sparse
tfidfAns.matrix<-as.matrix(tfidfAns_Sparse)
tfidfAns.matrix
freqAnsTFIDF=sort(colSums(tfidfAns.matrix),decreasing=TRUE)
freqAnsTFIDF.df<-data.frame(Word=names(freqAnsTFIDF),Freq=freqAnsTFIDF)
## variable to use for model are dtmAns.matrix & tfidfAns.matrix






############# Building corpus for Post dataset ###########################
corpusPost <- VCorpus(VectorSource(data_POST$Post))
corpusPost <- tm_map(corpusPost, removeWords, stopwords("english"))
##corpusPost <- tm_map(corpusPost, stemDocument)
corpusPost <- tm_map(corpusPost, stripWhitespace)
corpusPost <- tm_map(corpusPost, content_transformer(removeExtraSpaces))
corpusPost <- tm_map(corpusPost, removePunctuation)
corpusPost <- tm_map(corpusPost, content_transformer(removeHashTags))
corpusPost <- tm_map(corpusPost,content_transformer(removeTwitterHandles))
corpusPost <- tm_map(corpusPost, content_transformer(removeURL))

###########creating term frequency for posts
dtmPost <- DocumentTermMatrix(corpusPost)
inspect(dtmPost)
dtmPost #12896 document and 22354 terms

########### Removing sparse terms from posts DTM
dtmPost_Sparse<-removeSparseTerms(dtmPost, 0.995) 
inspect(dtmPost_Sparse)
dtmPost_Sparse #12896 docs, 329 terms
dtmPost.matrix<-as.matrix(dtmPost_Sparse) 
dtmPost.matrix
freqPostTF=sort(colSums(dtmPost.matrix),decreasing=TRUE)
freqPostTF.df<-data.frame(Word=names(freqPostTF),Freq=freqPostTF)

########### creating TF-IDF weighting for Posts
tfidf_Post<-DocumentTermMatrix(corpusPost,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
inspect(tfidf_Post)
tfidf_Post

########### Removing sparse terms from posts TFIDF
tfidfPost_Sparse<-removeSparseTerms(tfidf_Post, 0.995) #12896 docs and 329 terms
inspect(tfidfPost_Sparse)
tfidfPost_Sparse
tfidfPost.matrix<-as.matrix(tfidfPost_Sparse)
tfidfPost.matrix
freqPostTFIDF=sort(colSums(tfidfPost.matrix),decreasing=TRUE)
freqPostTFIDF.df<-data.frame(Word=names(freqPostTFIDF),Freq=freqPostTFIDF)
## variable to use for model are dtmPost.matrix & tfidfPost.matrix























