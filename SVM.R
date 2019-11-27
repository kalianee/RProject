#######SV Regression
library(e1071)
library(dplyr)
tc <- tune.control(cross = 10)

#linear = cost
#radial = cost, gamma
#sigmoid = cost, gamma, coef0
#polynomial = cost, gamma, coef0, degree

# Calculating parameters
tune.svm.1<- tune.svm(Average_Severity ~ ., data=train_Q_dtm.df, kernet = "radial", cost=10^(-1:2), gamma=c(.5,1,2), tunecontrol=tc)
tune.svm.2<- tune.svm(Average_Severity ~ ., data=train_Q_dtm.df, kernel = "polynomial", degree=c(2,3,4), coef0=c(0.1,0.5,1,2,3,4), tunecontrol=tc)
print(tune.svm.1) # cost = 0.1, gamma = 0.5
print(tune.svm.2) # degree = 2, coef0 = 3

#############QUESTION DTM
## 1. Kernel = linear
svm.Q.dtm.1 <- svm(Average_Severity ~ ., data=train_Q_dtm.df, kernel="linear", cost=tune.svm.1$best.parameters$cost, scale = FALSE)
svm.pred.Q.dtm.1<- predict(svm.Q.dtm.1, newdata=test_Q_dtm.df)
svmroc.Q.dtm.1<- roc(as.numeric(as.character(svm.pred.Q.dtm.1)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
svmroc.Q.dtm.1
plot(svmroc.Q.dtm.1, main="ROC Curve - SVM - Question DTM :: Kernel = linear", col = "red", lwd=2)
caret::confusionMatrix(svm.pred.Q.dtm.1, test_Q_dtm.df$Average_Severity, positive="1")

## 2. Kernel = radial
svm.Q.dtm.2 <- svm(Average_Severity ~ ., data=train_Q_dtm.df, kernel="radial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, scale = FALSE)
svm.pred.Q.dtm.2<- predict(svm.Q.dtm.2, newdata=test_Q_dtm.df)
svmroc.Q.dtm.2<- roc(as.numeric(as.character(svm.pred.Q.dtm.2)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
svmroc.Q.dtm.2
caret::confusionMatrix(svm.pred.Q.dtm.2, test_Q_dtm.df$Average_Severity, positive="1")

## 3. Kernel = polynomial
svm.Q.dtm.3 <- svm(Average_Severity ~ ., data=train_Q_dtm.df, kernel="polynomial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, degree=tune.svm.2$best.parameters$degree, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.Q.dtm.3<- predict(svm.Q.dtm.3, newdata=test_Q_dtm.df)
svmroc.Q.dtm.3<- roc(as.numeric(as.character(svm.pred.Q.dtm.3)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
svmroc.Q.dtm.3
caret::confusionMatrix(svm.pred.Q.dtm.3, test_Q_dtm.df$Average_Severity, positive="1")

## 4. Kernel = sidmoid
svm.Q.dtm.4 <- svm(Average_Severity ~ ., data=train_Q_dtm.df, kernel="sigmoid", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.Q.dtm.4<- predict(svm.Q.dtm.4, newdata=test_Q_dtm.df)
svmroc.Q.dtm.4<- roc(as.numeric(as.character(svm.pred.Q.dtm.4)), as.numeric(as.character(test_Q_dtm.df$Average_Severity)))
svmroc.Q.dtm.4
caret::confusionMatrix(svm.pred.Q.dtm.4, test_Q_dtm.df$Average_Severity, positive="1")

#############QUESTION TFIDf
## 1. Kernel = linear
svm.Q.tfidf.1 <- svm(Average_Severity ~ ., data=train_Q_tfidf.df, kernel="linear", cost=tune.svm.1$best.parameters$cost, scale = FALSE)
svm.pred.Q.tfidf.1<- predict(svm.Q.tfidf.1, newdata=test_Q_tfidf.df)
svmroc.Q.tfidf.1<- roc(as.numeric(as.character(svm.pred.Q.tfidf.1)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
svmroc.Q.tfidf.1
plot(svmroc.Q.tfidf.1, main="ROC Curve - SVM - Question TFIDF :: Kernel = linear", col = "red", lwd=2)
caret::confusionMatrix(svm.pred.Q.tfidf.1, test_Q_tfidf.df$Average_Severity, positive="1")

## 2. Kernel = radial
svm.Q.tfidf.2 <- svm(Average_Severity ~ ., data=train_Q_tfidf.df, kernel="radial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, scale = FALSE)
svm.pred.Q.tfidf.2<- predict(svm.Q.tfidf.2, newdata=test_Q_tfidf.df)
svmroc.Q.tfidf.2<- roc(as.numeric(as.character(svm.pred.Q.tfidf.2)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
svmroc.Q.tfidf.2
caret::confusionMatrix(svm.pred.Q.tfidf.2, test_Q_tfidf.df$Average_Severity, positive="1")

## 3. Kernel = polynomial
svm.Q.tfidf.3 <- svm(Average_Severity ~ ., data=train_Q_tfidf.df, kernel="polynomial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, degree=tune.svm.2$best.parameters$degree, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.Q.tfidf.3<- predict(svm.Q.tfidf.3, newdata=test_Q_tfidf.df)
svmroc.Q.tfidf.3<- roc(as.numeric(as.character(svm.pred.Q.tfidf.3)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
svmroc.Q.tfidf.3
caret::confusionMatrix(svm.pred.Q.tfidf.3, test_Q_tfidf.df$Average_Severity, positive="1")

## 4. Kernel = sigmoid
svm.Q.tfidf.4 <- svm(Average_Severity ~ ., data=train_Q_tfidf.df, kernel="sigmoid", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.Q.tfidf.4<- predict(svm.Q.tfidf.4, newdata=test_Q_tfidf.df)
svmroc.Q.tfidf.4<- roc(as.numeric(as.character(svm.pred.Q.tfidf.4)), as.numeric(as.character(test_Q_tfidf.df$Average_Severity)))
svmroc.Q.tfidf.4
caret::confusionMatrix(svm.pred.Q.tfidf.4, test_Q_tfidf.df$Average_Severity, positive="1")


#############ANSWER DTM
## 1. Kernel = linear
svm.A.dtm.1 <- svm(Average_Severity ~ ., data=train_A_dtm.df, kernel="linear", cost=tune.svm.1$best.parameters$cost, scale = FALSE)
svm.pred.A.dtm.1<- predict(svm.A.dtm.1, newdata=test_A_dtm.df)
svmroc.A.dtm.1<- roc(as.numeric(as.character(svm.pred.A.dtm.1)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
svmroc.A.dtm.1
plot(svmroc.A.dtm.1, main="ROC Curve - SVM - Answer DTM :: Kernel = linear", col = "red", lwd=2)
caret::confusionMatrix(svm.pred.A.dtm.1, test_A_dtm.df$Average_Severity, positive="1")

## 2. Kernel = radial
svm.A.dtm.2 <- svm(Average_Severity ~ ., data=train_A_dtm.df, kernel="radial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, scale = FALSE)
svm.pred.A.dtm.2<- predict(svm.A.dtm.2, newdata=test_A_dtm.df)
svmroc.A.dtm.2<- roc(as.numeric(as.character(svm.pred.A.dtm.2)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
svmroc.A.dtm.2
caret::confusionMatrix(svm.pred.A.dtm.2, test_A_dtm.df$Average_Severity, positive="1")

## 3. Kernel = polynomial
svm.A.dtm.3 <- svm(Average_Severity ~ ., data=train_A_dtm.df, kernel="polynomial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, degree=tune.svm.2$best.parameters$degree, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.A.dtm.3<- predict(svm.A.dtm.3, newdata=test_A_dtm.df)
svmroc.A.dtm.3<- roc(as.numeric(as.character(svm.pred.A.dtm.3)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
svmroc.A.dtm.3
caret::confusionMatrix(svm.pred.A.dtm.3, test_A_dtm.df$Average_Severity, positive="1")

## 4. Kernel = sidmoid
svm.A.dtm.4 <- svm(Average_Severity ~ ., data=train_A_dtm.df, kernel="sigmoid", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.A.dtm.4<- predict(svm.A.dtm.4, newdata=test_A_dtm.df)
svmroc.A.dtm.4<- roc(as.numeric(as.character(svm.pred.A.dtm.4)), as.numeric(as.character(test_A_dtm.df$Average_Severity)))
svmroc.A.dtm.4
caret::confusionMatrix(svm.pred.A.dtm.4, test_A_dtm.df$Average_Severity, positive="1")

#############ANSWER TFIDF
## 1. Kernel = linear
svm.A.tfidf.1 <- svm(Average_Severity ~ ., data=train_A_tfidf.df, kernel="linear", cost=tune.svm.1$best.parameters$cost, scale = FALSE)
svm.pred.A.tfidf.1<- predict(svm.A.tfidf.1, newdata=test_A_tfidf.df)
svmroc.A.tfidf.1<- roc(as.numeric(as.character(svm.pred.A.tfidf.1)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
svmroc.A.tfidf.1
plot(svmroc.A.tfidf.1, main="ROC Curve - SVM - Answer TFIDF :: Kernel = linear", col = "red", lwd=2)
caret::confusionMatrix(svm.pred.A.tfidf.1, test_A_tfidf.df$Average_Severity, positive="1")

## 2. Kernel = radial
svm.A.tfidf.2 <- svm(Average_Severity ~ ., data=train_A_tfidf.df, kernel="radial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, scale = FALSE)
svm.pred.A.tfidf.2<- predict(svm.A.tfidf.2, newdata=test_A_tfidf.df)
svmroc.A.tfidf.2<- roc(as.numeric(as.character(svm.pred.A.tfidf.2)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
svmroc.A.tfidf.2
caret::confusionMatrix(svm.pred.A.tfidf.2, test_A_tfidf.df$Average_Severity, positive="1")

## 3. Kernel = polynomial
svm.A.tfidf.3 <- svm(Average_Severity ~ ., data=train_A_tfidf.df, kernel="polynomial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, degree=tune.svm.2$best.parameters$degree, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.A.tfidf.3<- predict(svm.A.tfidf.3, newdata=test_A_tfidf.df)
svmroc.A.tfidf.3<- roc(as.numeric(as.character(svm.pred.A.tfidf.3)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
svmroc.A.tfidf.3
caret::confusionMatrix(svm.pred.A.tfidf.3, test_A_tfidf.df$Average_Severity, positive="1")

## 4. Kernel = sidmoid
svm.A.tfidf.4 <- svm(Average_Severity ~ ., data=train_A_tfidf.df, kernel="sigmoid", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.A.tfidf.4<- predict(svm.A.tfidf.4, newdata=test_A_tfidf.df)
svmroc.A.tfidf.4<- roc(as.numeric(as.character(svm.pred.A.tfidf.4)), as.numeric(as.character(test_A_tfidf.df$Average_Severity)))
svmroc.A.tfidf.4
caret::confusionMatrix(svm.pred.A.tfidf.4, test_A_tfidf.df$Average_Severity, positive="1")


#############POST DTM
## 1. Kernel = linear
svm.P.dtm.1 <- svm(Average_Severity ~ ., data=train_P_dtm.df, kernel="linear", cost=tune.svm.1$best.parameters$cost, scale = FALSE)
svm.pred.P.dtm.1<- predict(svm.P.dtm.1, newdata=test_P_dtm.df)
svmroc.P.dtm.1<- roc(as.numeric(as.character(svm.pred.P.dtm.1)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
svmroc.P.dtm.1
plot(svmroc.P.dtm.1, main="ROC Curve - SVM - Post DTM :: Kernel = linear", col = "red", lwd=2)
caret::confusionMatrix(svm.pred.P.dtm.1, test_P_dtm.df$Average_Severity, positive="1")

## 2. Kernel = radial
svm.P.dtm.2 <- svm(Average_Severity ~ ., data=train_P_dtm.df, kernel="radial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, scale = FALSE)
svm.pred.P.dtm.2<- predict(svm.P.dtm.2, newdata=test_P_dtm.df)
svmroc.P.dtm.2<- roc(as.numeric(as.character(svm.pred.P.dtm.2)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
svmroc.P.dtm.2
caret::confusionMatrix(svm.pred.P.dtm.2, test_P_dtm.df$Average_Severity, positive="1")

## 3. Kernel = polynomial
svm.P.dtm.3 <- svm(Average_Severity ~ ., data=train_P_dtm.df, kernel="polynomial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, degree=tune.svm.2$best.parameters$degree, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.P.dtm.3<- predict(svm.P.dtm.3, newdata=test_P_dtm.df)
svmroc.P.dtm.3<- roc(as.numeric(as.character(svm.pred.P.dtm.3)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
svmroc.P.dtm.3
caret::confusionMatrix(svm.pred.P.dtm.3, test_P_dtm.df$Average_Severity, positive="1")

## 4. Kernel = sidmoid
svm.P.dtm.4 <- svm(Average_Severity ~ ., data=train_P_dtm.df, kernel="sigmoid", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.P.dtm.4<- predict(svm.P.dtm.4, newdata=test_P_dtm.df)
svmroc.P.dtm.4<- roc(as.numeric(as.character(svm.pred.P.dtm.4)), as.numeric(as.character(test_P_dtm.df$Average_Severity)))
svmroc.P.dtm.4
caret::confusionMatrix(svm.pred.P.dtm.4, test_P_dtm.df$Average_Severity, positive="1")

#############POST TFIDF
## 1. Kernel = linear
svm.P.tfidf.1 <- svm(Average_Severity ~ ., data=train_P_tfidf.df, kernel="linear", cost=tune.svm.1$best.parameters$cost, scale = FALSE)
svm.pred.P.tfidf.1<- predict(svm.P.tfidf.1, newdata=test_P_tfidf.df)
svmroc.P.tfidf.1<- roc(as.numeric(as.character(svm.pred.P.tfidf.1)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
svmroc.P.tfidf.1
plot(svmroc.P.tfidf.1, main="ROC Curve - SVM - Post TFIDF :: Kernel = linear", col = "red", lwd=2)
caret::confusionMatrix(svm.pred.P.tfidf.1, test_P_tfidf.df$Average_Severity, positive="1")

## 2. Kernel = radial
svm.P.tfidf.2 <- svm(Average_Severity ~ ., data=train_P_tfidf.df, kernel="radial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, scale = FALSE)
svm.pred.P.tfidf.2<- predict(svm.P.tfidf.2, newdata=test_P_tfidf.df)
svmroc.P.tfidf.2<- roc(as.numeric(as.character(svm.pred.P.tfidf.2)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
svmroc.P.tfidf.2
caret::confusionMatrix(svm.pred.P.tfidf.2, test_P_tfidf.df$Average_Severity, positive="1")

## 3. Kernel = polynomial
svm.P.tfidf.3 <- svm(Average_Severity ~ ., data=train_P_tfidf.df, kernel="polynomial", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, degree=tune.svm.2$best.parameters$degree, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.P.tfidf.3<- predict(svm.P.tfidf.3, newdata=test_P_tfidf.df)
svmroc.P.tfidf.3<- roc(as.numeric(as.character(svm.pred.P.tfidf.3)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
svmroc.P.tfidf.3
caret::confusionMatrix(svm.pred.P.tfidf.3, test_P_tfidf.df$Average_Severity, positive="1")

## 4. Kernel = sidmoid
svm.P.tfidf.4 <- svm(Average_Severity ~ ., data=train_P_tfidf.df, kernel="sigmoid", cost=tune.svm.1$best.parameters$cost, gamma=tune.svm.1$best.parameters$gamma, coef0=tune.svm.2$best.parameters$coef0, scale = FALSE)
svm.pred.P.tfidf.4<- predict(svm.P.tfidf.4, newdata=test_P_tfidf.df)
svmroc.P.tfidf.4<- roc(as.numeric(as.character(svm.pred.P.tfidf.4)), as.numeric(as.character(test_P_tfidf.df$Average_Severity)))
svmroc.P.tfidf.4
caret::confusionMatrix(svm.pred.P.tfidf.4, test_P_tfidf.df$Average_Severity, positive="1")

