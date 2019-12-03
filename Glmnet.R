library(caret)
set.seed(1234)
sapply(data_QA, class)
sapply(data_POST, class)
######################### data_QA dataset #########################################
### training and test data for Questions
index.QA <- createDataPartition(data_QA$Average_Severity, p = .75, list = FALSE)
QA_train <- data_QA[index.QA, ]
QA_test <- data_QA[-index.QA, ]
train_Q.dtm <- dtmQues_Sparse[index.QA, ]
test_Q.dtm <- dtmQues_Sparse[-index.QA, ]
train_Q_dtm.matrix <- as.matrix(train_Q.dtm)
test_Q_dtm.matrix <- as.matrix(test_Q.dtm)
train_Q.tfidf <- tfidfQues_Sparse[index.QA, ]
test_Q.tfidf <- tfidfQues_Sparse[-index.QA, ]
train_Q_tfidf.matrix <- as.matrix(train_Q.tfidf)
test_Q_tfidf.matrix <- as.matrix(test_Q.tfidf)

### model for Questions DTM 
library(glmnet)
library(glmnetUtils)
glmnet.Q.dtm = cva.glmnet(x = train_Q_dtm.matrix, y = QA_train[['Average_Severity']], family = 'binomial', parallel = FALSE)
plot(glmnet.Q.dtm) #the curve flatten out for lower values of λ, that's why we use higher value for alpha
plot(glmnet.Q.dtm$modlist[[10]]) # alpha = 0.729
glmnet.Q.dtm$alpha
preds.Q.dtm = predict(glmnet.Q.dtm, test_Q_dtm.matrix, alpha = 0.729)
preds.Q.dtm <- ifelse(preds.Q.dtm>.5, 1, 0)
glmnet:::auc(QA_test$Average_Severity, preds.Q.dtm) #glmnet has inbuilt function for AUC
cm.Q.dtm<-caret::confusionMatrix(QA_test$Average_Severity, preds.Q.dtm, positive="1")
cm.Q.dtm 
table(QA_test$Average_Severity)
table(preds.Q.dtm) 

### model for Questions TFIDF
glmnet.Q.tfidf = cva.glmnet(x = train_Q_tfidf.matrix, y = QA_train[['Average_Severity']], family = 'binomial', parallel = FALSE)
plot(glmnet.Q.tfidf)
preds.Q.tfidf = predict(glmnet.Q.tfidf, test_Q_tfidf.matrix, alpha = 0.729)
preds.Q.tfidf <- ifelse(preds.Q.tfidf>.5, 1, 0)
glmnet:::auc(QA_test$Average_Severity, preds.Q.tfidf)
cm.Q.tfidf <- confusionMatrix(QA_test$Average_Severity, factor(as.integer(format(round(preds.Q.tfidf)))), positive="1")
cm.Q.tfidf 
table(preds.Q.tfidf)


#######################################################################################
### training and test data for Answers
train_A.dtm <- dtmAns_Sparse[index.QA, ]
test_A.dtm <- dtmAns_Sparse[-index.QA, ]
train_A_dtm.matrix <- as.matrix(train_A.dtm)
test_A_dtm.matrix <- as.matrix(test_A.dtm)
train_A.tfidf <- tfidfAns_Sparse[index.QA, ] 
test_A.tfidf <- tfidfAns_Sparse[-index.QA, ]
train_A_tfidf.matrix <- as.matrix(train_A.tfidf)
test_A_tfidf.matrix <- as.matrix(test_A.tfidf)

### model for Answers DTM 
glmnet.A.dtm = cva.glmnet(x = train_A_dtm.matrix, y = QA_train[['Average_Severity']], family = 'binomial', parallel = FALSE)
plot(glmnet.A.dtm, ylab='')
plot(glmnet.A.dtm$modlist[[10]])
glmnet.A.dtm$alpha
preds.A.dtm = predict(glmnet.A.dtm, test_A_dtm.matrix, alpha = 0.729)
preds.A.dtm <- ifelse(preds.A.dtm>.5, 1, 0)
glmnet:::auc(QA_test$Average_Severity, preds.A.dtm) 
cm.A.dtm <- confusionMatrix(QA_test$Average_Severity, factor(as.integer(format(round(preds.A.dtm)))), positive="1")
cm.A.dtm 
table(round(preds.A.dtm))

### model for Answers TFIDF
glmnet.A.tfidf = cva.glmnet(x = train_A_tfidf.matrix, y = QA_train[['Average_Severity']], family = 'binomial', parallel = FALSE)
plot(glmnet.A.tfidf)
preds.A.tfidf = predict(glmnet.A.tfidf, test_A_tfidf.matrix, alpha = 0.729)
preds.A.tfidf <- ifelse(preds.A.tfidf>.5, 1, 0)
glmnet:::auc(QA_test$Average_Severity, preds.A.tfidf) 
cm.A.tfidf <- confusionMatrix(QA_test$Average_Severity, factor(as.integer(format(round(preds.A.tfidf)))), positive="1")
cm.A.tfidf 
table(round(preds.A.tfidf)) 


#######################################################################################
######################### data_POST dataset #########################################
index.POST <- createDataPartition(data_POST$Average_Severity, p = .75, list = FALSE)
Post_train <- data_POST[index.POST, ]
Post_test <- data_POST[-index.POST, ]
train_P.dtm <- dtmPost_Sparse[index.POST, ]
test_P.dtm <- dtmPost_Sparse[-index.POST, ]
train_P_dtm.matrix <- as.matrix(train_P.dtm)
test_P_dtm.matrix <- as.matrix(test_P.dtm)
train_P.tfidf <- tfidfPost_Sparse[index.POST, ] 
test_P.tfidf <- tfidfPost_Sparse[-index.POST, ]
train_P_tfidf.matrix <- as.matrix(train_P.tfidf)
test_P_tfidf.matrix <- as.matrix(test_P.tfidf)

### model for POST DTM 
glmnet.P.dtm = cva.glmnet(x = train_P_dtm.matrix, y = Post_train[['Average_Severity']], family = 'binomial', parallel = FALSE)
plot(glmnet.P.dtm)
plot(glmnet.P.dtm$modlist[[10]])
preds.P.dtm = predict(glmnet.P.dtm, test_P_dtm.matrix, alpha = 0.729)
preds.P.dtm <- ifelse(preds.P.dtm>.5, 1, 0)
glmnet:::auc(Post_test$Average_Severity, preds.P.dtm)
cm.P.dtm <- confusionMatrix(Post_test$Average_Severity, factor(as.integer(format(round(preds.P.dtm)))), positive="1")
cm.P.dtm 
table(round(preds.P.dtm)) 

### model for POST TFIDF
glmnet.P.tfidf = cva.glmnet(x = train_P_tfidf.matrix, y = Post_train[['Average_Severity']], family = 'binomial', parallel = FALSE)
plot(glmnet.P.tfidf)
preds.P.tfidf = predict(glmnet.P.tfidf, test_P_tfidf.matrix, alpha = 0.729)
preds.P.tfidf <- ifelse(preds.P.tfidf>.5, 1, 0)
glmnet:::auc(Post_test$Average_Severity, preds.P.tfidf) 
cm.P.tfidf <- confusionMatrix(Post_test$Average_Severity, factor(as.integer(format(round(preds.P.tfidf)))), positive="1")
cm.P.tfidf 
table(Post_test$Average_Severity) 
table(round(preds.P.tfidf)) 















