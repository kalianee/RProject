
x <- train_Q_dtm.df[,1:159]
y <- train_Q_dtm.df[,160]
mtry <- sqrt(ncol(x)) #12.61

control <- trainControl(method='repeatedcv', number=10, repeats=3, search = 'random')
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
#Number randomely variable selected is mtry
rf.Q.dtm.1 <- train(Average_Severity ~., 
                    data=train_Q_dtm.df, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneLength  = 15,
                    ntree=500,
                    trControl=control)

stopCluster(cl)



