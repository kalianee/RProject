library(randomForest)
library(pROC)
set.seed(1234)
#############QUESTION DTM
train_Q_dtm.df <- as.data.frame(train_Q_dtm.matrix)
test_Q_dtm.df<- as.data.frame(test_Q_dtm.matrix)
colnames(train_Q_dtm.df) <- make.names(colnames(train_Q_dtm.df))
colnames(test_Q_dtm.df) <- make.names(colnames(test_Q_dtm.df))
train_Q_dtm.df$Average_Severity = factor(QA_train$Average_Severity)
test_Q_dtm.df$Average_Severity = factor(QA_test$Average_Severity)

#defining default values of mtry
x1 <- train_Q_dtm.df[,1:159]
sqrt(ncol(x1)) #12.6

x2 <- train_A_dtm.df[,1:176]
sqrt(ncol(x2)) #13.2

x3 <- train_P_dtm.df[,1:329]
sqrt(ncol(x3)) #18.1

## 1. number of trees = 500
rf.Q.dtm.1 <- randomForest(Average_Severity ~ ., data=train_Q_dtm.df, ntree=500, mtry = 13, importance=TRUE)
rf.pred.Q.dtm.1 <- predict(rf.Q.dtm.1, newdata = test_Q_dtm.df)
table(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.1)
roc.Q.dtm.1<- roc(as.numeric(as.character(rf.pred.Q.dtm.1)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
roc.Q.dtm.1
plot(roc.Q.dtm.1, main="ROC Curve - Random Forest - Question DTM :: Trees = 500", col = "red", lwd=2)
caret::confusionMatrix(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.1, positive="1")
table(rf.pred.Q.dtm.1)

## 2. number of trees = 100
rf.Q.dtm.2 <- randomForest(Average_Severity ~ ., data=train_Q_dtm.df, ntree=100, mtry = 13, importance=TRUE)
rf.pred.Q.dtm.2 <- predict(rf.Q.dtm.2, newdata = test_Q_dtm.df)
table(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.2)
roc.Q.dtm.2<- roc(as.numeric(as.character(rf.pred.Q.dtm.2)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
roc.Q.dtm.2
caret::confusionMatrix(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.2, positive="1")

## 3. number of trees = 1000
rf.Q.dtm.3 <- randomForest(Average_Severity ~ ., data=train_Q_dtm.df, ntree=1000, mtry = 13, importance=TRUE)
rf.pred.Q.dtm.3 <- predict(rf.Q.dtm.3, newdata = test_Q_dtm.df)
table(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.3)
roc.Q.dtm.3<- roc(as.numeric(as.character(rf.pred.Q.dtm.3)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
roc.Q.dtm.3
caret::confusionMatrix(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.3, positive="1")
table(rf.pred.Q.dtm.3) 

## 4. number of trees = 10
rf.Q.dtm.4 <- randomForest(Average_Severity ~ ., data=train_Q_dtm.df, ntree=10, mtry = 13, importance=TRUE)
rf.pred.Q.dtm.4 <- predict(rf.Q.dtm.4, newdata = test_Q_dtm.df)
table(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.4)
roc.Q.dtm.4<- roc(as.numeric(as.character(rf.pred.Q.dtm.4)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
roc.Q.dtm.4
caret::confusionMatrix(test_Q_dtm.df$Average_Severity,rf.pred.Q.dtm.4, positive="1")
table(rf.pred.Q.dtm.4) 

#############QUESTION TFIDF
train_Q_tfidf.df <- as.data.frame(train_Q_tfidf.matrix)
test_Q_tfidf.df<- as.data.frame(test_Q_tfidf.matrix)
colnames(train_Q_tfidf.df) <- make.names(colnames(train_Q_tfidf.df))
colnames(test_Q_tfidf.df) <- make.names(colnames(test_Q_tfidf.df))
train_Q_tfidf.df$Average_Severity = factor(QA_train$Average_Severity)
test_Q_tfidf.df$Average_Severity = factor(QA_test$Average_Severity)

## 1. number of trees = 500
rf.Q.tfidf.1 <- randomForest(Average_Severity ~ ., data=train_Q_tfidf.df, ntree=500, mtry = 13, importance=TRUE)
rf.pred.Q.tfidf.1 <- predict(rf.Q.tfidf.1, newdata = test_Q_tfidf.df)
table(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.1)
roc.Q.tfidf.1<- roc(as.numeric(as.character(rf.pred.Q.tfidf.1)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
roc.Q.tfidf.1
plot(roc.Q.tfidf.1, main="ROC Curve - Random Forest - Question TFIDF :: Trees = 500", col = "red", lwd=2)
caret::confusionMatrix(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.1, positive="1")
table(rf.pred.Q.tfidf.1)

## 2. number of trees = 100
rf.Q.tfidf.2 <- randomForest(Average_Severity ~ ., data=train_Q_tfidf.df, ntree=100, mtry = 13, importance=TRUE)
rf.pred.Q.tfidf.2 <- predict(rf.Q.tfidf.2, newdata = test_Q_tfidf.df)
table(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.2)
roc.Q.tfidf.2<- roc(as.numeric(as.character(rf.pred.Q.tfidf.2)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
roc.Q.tfidf.2
caret::confusionMatrix(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.2, positive="1")
table(rf.pred.Q.tfidf.2)

## 3. number of trees = 1000
rf.Q.tfidf.3 <- randomForest(Average_Severity ~ ., data=train_Q_tfidf.df, ntree=1000, mtry = 13, importance=TRUE)
rf.pred.Q.tfidf.3 <- predict(rf.Q.tfidf.3, newdata = test_Q_tfidf.df)
table(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.3)
roc.Q.tfidf.3<- roc(as.numeric(as.character(rf.pred.Q.tfidf.3)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
roc.Q.tfidf.3
caret::confusionMatrix(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.3, positive="1")

## 4. number of trees = 10
rf.Q.tfidf.4 <- randomForest(Average_Severity ~ ., data=train_Q_tfidf.df, ntree=10, mtry = 13, importance=TRUE)
rf.pred.Q.tfidf.4 <- predict(rf.Q.tfidf.4, newdata = test_Q_tfidf.df)
table(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.4)
roc.Q.tfidf.4<- roc(as.numeric(as.character(rf.pred.Q.tfidf.4)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
roc.Q.tfidf.4
caret::confusionMatrix(test_Q_tfidf.df$Average_Severity,rf.pred.Q.tfidf.4, positive="1")


#############ANSWER DTM
train_A_dtm.df <- as.data.frame(train_A_dtm.matrix)
test_A_dtm.df<- as.data.frame(test_A_dtm.matrix)
colnames(train_A_dtm.df) <- make.names(colnames(train_A_dtm.df))
colnames(test_A_dtm.df) <- make.names(colnames(test_A_dtm.df))
train_A_dtm.df$Average_Severity = factor(QA_train$Average_Severity)
test_A_dtm.df$Average_Severity = factor(QA_test$Average_Severity)

## 1. number of trees = 500
rf.A.dtm.1 <- randomForest(Average_Severity ~ ., data=train_A_dtm.df, ntree=500, mtry = 13, importance=TRUE)
rf.pred.A.dtm.1 <- predict(rf.A.dtm.1, newdata = test_A_dtm.df)
table(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.1)
roc.A.dtm.1<- roc(as.numeric(as.character(rf.pred.A.dtm.1)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
roc.A.dtm.1
plot(roc.A.dtm.1, main="ROC Curve - Random Forest - Answer DTM :: Trees = 500", col = "red", lwd=2)
caret::confusionMatrix(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.1, positive="1")

## 2. number of trees = 100
rf.A.dtm.2 <- randomForest(Average_Severity ~ ., data=train_A_dtm.df, ntree=100, mtry = 13, importance=TRUE)
rf.pred.A.dtm.2 <- predict(rf.A.dtm.2, newdata = test_A_dtm.df)
table(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.2)
roc.A.dtm.2<- roc(as.numeric(as.character(rf.pred.A.dtm.2)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
roc.A.dtm.2
caret::confusionMatrix(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.2, positive="1")

## 3. number of trees = 1000
rf.A.dtm.3 <- randomForest(Average_Severity ~ ., data=train_A_dtm.df, ntree=1000, mtry = 13, importance=TRUE)
rf.pred.A.dtm.3 <- predict(rf.A.dtm.3, newdata = test_A_dtm.df)
table(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.3)
roc.A.dtm.3<- roc(as.numeric(as.character(rf.pred.A.dtm.3)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
roc.A.dtm.3
caret::confusionMatrix(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.3, positive="1")

## 4. number of trees = 10
rf.A.dtm.4 <- randomForest(Average_Severity ~ ., data=train_A_dtm.df, ntree=10, mtry = 13, importance=TRUE)
rf.pred.A.dtm.4 <- predict(rf.A.dtm.4, newdata = test_A_dtm.df)
table(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.4)
roc.A.dtm.4<- roc(as.numeric(as.character(rf.pred.A.dtm.4)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
roc.A.dtm.4
caret::confusionMatrix(test_A_dtm.df$Average_Severity,rf.pred.A.dtm.4, positive="1")

#############ANSWER TFIDF
train_A_tfidf.df <- as.data.frame(train_A_tfidf.matrix)
test_A_tfidf.df<- as.data.frame(test_A_tfidf.matrix)
colnames(train_A_tfidf.df) <- make.names(colnames(train_A_tfidf.df))
colnames(test_A_tfidf.df) <- make.names(colnames(test_A_tfidf.df))
train_A_tfidf.df$Average_Severity = factor(QA_train$Average_Severity)
test_A_tfidf.df$Average_Severity = factor(QA_test$Average_Severity)

## 1. number of trees = 500
rf.A.tfidf.1 <- randomForest(Average_Severity ~ ., data=train_A_tfidf.df, ntree=500, mtry = 13, importance=TRUE)
rf.pred.A.tfidf.1 <- predict(rf.A.tfidf.1, newdata = test_A_tfidf.df)
table(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.1)
roc.A.tfidf.1<- roc(as.numeric(as.character(rf.pred.A.tfidf.1)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
roc.A.tfidf.1
plot(roc.A.tfidf.1, main="ROC Curve - Random Forest - Answer TFIDF :: Trees = 500", col = "red", lwd=2)
caret::confusionMatrix(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.1, positive="1")

## 2. number of trees = 100
rf.A.tfidf.2 <- randomForest(Average_Severity ~ ., data=train_A_tfidf.df, ntree=100, mtry = 13, importance=TRUE)
rf.pred.A.tfidf.2 <- predict(rf.A.tfidf.2, newdata = test_A_tfidf.df)
table(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.2)
roc.A.tfidf.2<- roc(as.numeric(as.character(rf.pred.A.tfidf.2)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
roc.A.tfidf.2
caret::confusionMatrix(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.2, positive="1")

## 3. number of trees = 1000
rf.A.tfidf.3 <- randomForest(Average_Severity ~ ., data=train_A_tfidf.df, ntree=1000, mtry = 13, importance=TRUE)
rf.pred.A.tfidf.3 <- predict(rf.A.tfidf.3, newdata = test_A_tfidf.df)
table(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.3)
roc.A.tfidf.3<- roc(as.numeric(as.character(rf.pred.A.tfidf.3)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
roc.A.tfidf.3
caret::confusionMatrix(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.3, positive="1")

## 4. number of trees = 10
rf.A.tfidf.4 <- randomForest(Average_Severity ~ ., data=train_A_tfidf.df, ntree=10, mtry = 13, importance=TRUE)
rf.pred.A.tfidf.4 <- predict(rf.A.tfidf.4, newdata = test_A_tfidf.df)
table(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.4)
roc.A.tfidf.4<- roc(as.numeric(as.character(rf.pred.A.tfidf.4)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
roc.A.tfidf.4
caret::confusionMatrix(test_A_tfidf.df$Average_Severity,rf.pred.A.tfidf.4, positive="1")


#############POST DTM
train_P_dtm.df <- as.data.frame(train_P_dtm.matrix)
test_P_dtm.df<- as.data.frame(test_P_dtm.matrix)
colnames(train_P_dtm.df) <- make.names(colnames(train_P_dtm.df))
colnames(test_P_dtm.df) <- make.names(colnames(test_P_dtm.df))
train_P_dtm.df$Average_Severity = factor(Post_train$Average_Severity)
test_P_dtm.df$Average_Severity = factor(Post_test$Average_Severity)

## 1. number of trees = 500
rf.P.dtm.1 <- randomForest(Average_Severity ~ ., data=train_P_dtm.df, ntree=500, mtry = 18, importance=TRUE)
rf.pred.P.dtm.1 <- predict(rf.P.dtm.1, newdata = test_P_dtm.df)
table(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.1)
roc.P.dtm.1<- roc(as.numeric(as.character(rf.pred.P.dtm.1)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
roc.P.dtm.1
plot(roc.P.dtm.1, main="ROC Curve - Random Forest - Post DTM :: Trees = 500", col = "red", lwd=2)
caret::confusionMatrix(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.1, positive="1")

## 2. number of trees = 100
rf.P.dtm.2 <- randomForest(Average_Severity ~ ., data=train_P_dtm.df, ntree=100, mtry = 18, importance=TRUE)
rf.pred.P.dtm.2 <- predict(rf.P.dtm.2, newdata = test_P_dtm.df)
table(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.2)
roc.P.dtm.2<- roc(as.numeric(as.character(rf.pred.P.dtm.2)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
roc.P.dtm.2
caret::confusionMatrix(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.2, positive="1")

## 3. number of trees = 1000
rf.P.dtm.3 <- randomForest(Average_Severity ~ ., data=train_P_dtm.df, ntree=1000, mtry = 18, importance=TRUE)
rf.pred.P.dtm.3 <- predict(rf.P.dtm.3, newdata = test_P_dtm.df)
table(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.3)
roc.P.dtm.3<- roc(as.numeric(as.character(rf.pred.P.dtm.3)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
roc.P.dtm.3
caret::confusionMatrix(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.3, positive="1")

## 4. number of trees = 10
rf.P.dtm.4 <- randomForest(Average_Severity ~ ., data=train_P_dtm.df, ntree=10, mtry = 18, importance=TRUE)
rf.pred.P.dtm.4 <- predict(rf.P.dtm.4, newdata = test_P_dtm.df)
table(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.4)
roc.P.dtm.4 <- roc(as.numeric(as.character(rf.pred.P.dtm.4)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
roc.P.dtm.4
caret::confusionMatrix(test_P_dtm.df$Average_Severity,rf.pred.P.dtm.4, positive="1")

#############POST TFIDF
train_P_tfidf.df <- as.data.frame(train_P_tfidf.matrix) 
test_P_tfidf.df<- as.data.frame(test_P_tfidf.matrix)
colnames(train_P_tfidf.df) <- make.names(colnames(train_P_tfidf.df))
colnames(test_P_tfidf.df) <- make.names(colnames(test_P_tfidf.df))
train_P_tfidf.df$Average_Severity = factor(Post_train$Average_Severity)
test_P_tfidf.df$Average_Severity = factor(Post_test$Average_Severity)

## 1. number of trees = 500
rf.P.tfidf.1 <- randomForest(Average_Severity ~ ., data=train_P_tfidf.df, ntree=500, mtry = 18, importance=TRUE)
rf.pred.P.tfidf.1 <- predict(rf.P.tfidf.1, newdata = test_P_tfidf.df)
table(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.1)
roc.P.tfidf.1<- roc(as.numeric(as.character(rf.pred.P.tfidf.1)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
roc.P.tfidf.1
plot(roc.P.tfidf.1, main="ROC Curve - Random Forest - Post TFIDF :: Trees = 500", col = "red", lwd=2)
caret::confusionMatrix(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.1, positive="1")

## 2. number of trees = 100
rf.P.tfidf.2 <- randomForest(Average_Severity ~ ., data=train_P_tfidf.df, ntree=100, mtry = 18, importance=TRUE)
rf.pred.P.tfidf.2 <- predict(rf.P.tfidf.2, newdata = test_P_tfidf.df)
table(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.2)
roc.P.tfidf.2<- roc(as.numeric(as.character(rf.pred.P.tfidf.2)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
roc.P.tfidf.2
caret::confusionMatrix(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.2, positive="1")

## 3. number of trees = 1000
rf.P.tfidf.3 <- randomForest(Average_Severity ~ ., data=train_P_tfidf.df, ntree=1000, mtry = 18, importance=TRUE)
rf.pred.P.tfidf.3 <- predict(rf.P.tfidf.3, newdata = test_P_tfidf.df)
table(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.3)
roc.P.tfidf.3<- roc(as.numeric(as.character(rf.pred.P.tfidf.3)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
roc.P.tfidf.3
caret::confusionMatrix(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.3, positive="1")

## 4. number of trees = 10
rf.P.tfidf.4 <- randomForest(Average_Severity ~ ., data=train_P_tfidf.df, ntree=10, mtry = 18, importance=TRUE)
rf.pred.P.tfidf.4 <- predict(rf.P.tfidf.4, newdata = test_P_tfidf.df)
table(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.4)
roc.P.tfidf.4<- roc(as.numeric(as.character(rf.pred.P.tfidf.4)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
roc.P.tfidf.4
caret::confusionMatrix(test_P_tfidf.df$Average_Severity,rf.pred.P.tfidf.4, positive="1")


plot(roc.Q.dtm.1, main="ROC Curve - Random Forest ::: Trees = 100", col = "red", lwd=3)



