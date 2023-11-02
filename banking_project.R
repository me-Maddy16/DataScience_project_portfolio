library(rpart)
library(rpart.plot)
library(caret)
bank<- read.csv("bank-full.csv",header=TRUE)
bank_dt<-bank
for (i in 1:nrow(bank_dt)){
  if(bank_dt$job[i]!='retired' & bank_dt$job[i]!='unemployed'){
    bank_dt$job[i]='employed'
  }
  else{
    bank_dt$job[i]=bank_dt$job[i]
  }
}
bank_dt<-bank_dt[,c(-10,-11,-12)]
#use bank_dt for analysis here

# partition
set.seed(16)  
train.index <- sample(c(1:dim(bank_dt)[1]), dim(bank_dt)[1]*0.6)  
train.df <- bank_dt[train.index, ]
valid.df <- bank_dt[-train.index, ]

# classification tree
default.ct <- rpart(y ~ ., data = train.df ,method = "class",cp=0.001)

# plot tree
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = 10)
# count number of leaves
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])

# classification tree split using entropy
default.info.ct <- rpart(y ~ ., data = train.df, parms = list(split = 'information'), method = "class",cp=0.001)
prp(default.info.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(default.info.ct$frame$var[default.info.ct$frame$var == "<leaf>"])


# minsplit parameter is the smallest number of observations in the parent node that could be split further, default is 20
# maxdepth parameter prevents the tree from growing past a certain depth / height, default is 30
# complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node, default value of 0.01

# classify records in the training data.
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(default.info.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$y))

default.ct.point.pred.trainG <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.trainG, as.factor(train.df$y))


### repeat the code for the validation set, and the deeper tree

default.ct.point.pred.validG <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.validG, as.factor(valid.df$y))

default.ct.point.pred.valid <- predict(default.info.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$y))



# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.


cv.ct <- rpart(y ~ ., data = train.df, method = "class", minsplit = 1, xval = 5)  # xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.

cv.ct <- rpart(y ~ ., data = train.df, method = "class", cp = 0.0001, minsplit = 1, xval = 5)  # xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.
pruned.ct <- prune(cv.ct, cp = 0.00126703)

printcp(pruned.ct)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 
default.ct.point.pred.valid_pruned<- predict(pruned.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid_pruned, as.factor(valid.df$y))

####Figure 9.15

library(randomForest)
## random forest
rf <- randomForest(as.factor(y) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)


## confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$y))




#### Table 9.5

library(adabag)

train.df$y <- as.factor(train.df$y)

boost <- boosting(y ~ ., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$y))

