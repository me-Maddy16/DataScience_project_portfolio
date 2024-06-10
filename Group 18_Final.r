#Uploading dataset
obesity<-read.csv('ObesityDataSet.csv',header=TRUE)
ob_num<-obesity

#Exploratory analysis
library(ggplot2)
gg<-ggplot(ob_num) 
#gender
gg + geom_bar(aes(x=Gender,fill=Gender)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#family_history
gg + geom_bar(aes(x=family_history_with_overweight,fill=family_history_with_overweight)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#FAVC
gg + geom_bar(aes(x=FAVC,fill=FAVC)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#NCP
#rounding each value in the column to nearest whole number
ob_num$NCP<-round(ob_num$NCP,0)
ob_num$NCP<-factor(ob_num$NCP)
gg + geom_bar(aes(x=ob_num$NCP,fill=ob_num$NCP),show.legend = FALSE) +ggtitle("Number of main meals a day") + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#smoke
gg + geom_bar(aes(x=SMOKE,fill=SMOKE)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#SCC
gg + geom_bar(aes(x=SCC,fill=SCC)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#CAEC
gg + geom_bar(aes(x=CAEC,fill=CAEC)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#MTRANS
gg + geom_bar(aes(x=MTRANS,fill=MTRANS)) + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#FCVC
ob_num$FCVC<-round(ob_num$FCVC,0)
ob_num$FCVC<-factor(ob_num$FCVC)
gg + geom_bar(aes(x=ob_num$FCVC,fill=ob_num$FCVC),show.legend = FALSE) +ggtitle("Number of meals where one eats vegetables") + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#CH20
str(ob_num$CH2O)
for(i in 1:length(ob_num$CH2O)){
  if(ob_num$CH2O[i]>2){
    ob_num$C2O[i]<-'More than 2'
  } else if(ob_num$CH2O[i]<1){
    ob_num$C2O[i]<-'Less than 1'
  } else{
    ob_num$C2O[i]<-'Between 1 and 2'
  }
}
gg + geom_bar(aes(x=ob_num$C2O,fill=ob_num$C2O),show.legend = FALSE) +ggtitle("Liters of water per day") + theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#CALC
gg + geom_bar(aes(x=CALC,fill=CALC),show.legend=FALSE) + ggtitle("Frequency of alcohol intake")+theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#FAF
gg + geom_bar(aes(x=factor(round(ob_num$FAF,0)),fill=factor(round(ob_num$FAF,0))),show.legend=FALSE) + ggtitle("Frequency of days of physical activity per week")+theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))
#TUE
gg + geom_bar(aes(x=factor(round(ob_num$TUE,0)),fill=factor(round(ob_num$TUE,0))),show.legend=FALSE) + ggtitle("Hours of use of technology devices on daily basis")+theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))


#height,weight,age
hist(ob_num$Age,xlab='Age',main = 'Histogram of age distribution')
hist(ob_num$Height,xlab='Height in {unit}',main = 'Histogram of height distribution')
hist(ob_num$Weight,xlab='Weight in {unit}',main = 'Histogram of weight distribution')
#Body_mass_index
gg + geom_bar(aes(x=ob_num$NObeyesdad,fill=ob_num$NObeyesdad),show.legend=FALSE) + ggtitle("BMI readings")+theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))

#correlation matrix to determine variables for analysis
ob_trial = ob_num

ob_trial$Gender <- ifelse(ob_trial$Gender == "Male", 0, 1)
ob_trial$family_history_with_overweight <- ifelse(ob_trial$family_history_with_overweight == "no", 0, 1)
ob_trial$FAVC <- ifelse(ob_trial$FAVC == "no", 0, 1)
ob_trial$CAEC <- ifelse(ob_trial$CAEC == "no", 0,ifelse(ob_trial$CAEC == "Sometimes", 1,ifelse(ob_trial$CAEC == "Frequently", 2, 3)))
ob_trial$SMOKE <- ifelse(ob_trial$SMOKE == "no", 0, 1)
ob_trial$SCC <- ifelse(ob_trial$SCC == "no", 0, 1)
ob_trial$CALC <- ifelse(ob_trial$CALC == "no", 0, ifelse(ob_trial$CALC == "Sometimes", 1, ifelse(ob_trial$CALC == "Frequently", 2, 3)))
ob_trial$NObeyesdad <- ifelse(ob_trial$NObeyesdad == "Insufficient_Weight", 1,
                                        ifelse(ob_trial$NObeyesdad == "Normal_Weight", 2,
                                               ifelse(ob_trial$NObeyesdad == "Overweight_Level_I", 3,
                                                      ifelse(ob_trial$NObeyesdad == "Overweight_Level_II", 4,
                                                             ifelse(ob_trial$NObeyesdad == "Obesity_Type_I", 5,
                                                                    ifelse(ob_trial$NObeyesdad == "Obesity_Type_II", 6,7
                                                                    ))))))
str(ob_trial)

ob_trial$FCVC = as.numeric(ob_trial$FCVC)
ob_trial$NCP = as.numeric(ob_trial$NCP)
ob_trial = ob_trial[,c(-16,-18)]
str(ob_trial)

ob_norm<-scale(ob_trial)  
str(ob_norm)
cor_obnorm <- cor(ob_norm, method="spearman")    

library(corrplot)
cor_w = corrplot(cor_obnorm, type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.5)
str(cor_w)

ob_norm<-scale(ob_trial[,c(-1,-2,-3,-4)])  
str(ob_norm)
cor_obnorm <- cor(ob_norm, method="spearman")    

library(corrplot)
cor_w1 = corrplot(cor_obnorm, type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.8)
str(cor_w1)

library(rpart)
library(rpart.plot)
library(caret)

ob_dt = ob_trial[,c(-1,-2,-3,-4)]
ob_dt$NObeyesdad <- ifelse(ob_dt$NObeyesdad == 1, 0,(ifelse(ob_dt$NObeyesdad == 2,0,1)))
str(ob_dt)
gg2<-ggplot(ob_dt)
gg2 + geom_bar(aes(x=NObeyesdad,fill=NObeyesdad),show.legend=FALSE) + ggtitle("BMI readings")+theme(axis.title.x =element_blank(),axis.title.y =element_blank(),axis.text =element_text(size=10),axis.title = element_text(size=14))

#Decision Tree
train.index <- sample(c(1:dim(ob_dt)[1]), dim(ob_dt)[1]*0.6)  
train.df <- ob_dt[train.index, ]

library(dplyr)

set.seed(1)
#Scaling training dataset

# Identify indices for each class in the training data
train_class_0_indices <- which(train.df$NObeyesdad == '0')
train_class_1_indices <- which(train.df$NObeyesdad == '1')

# Determine the number of samples in each class
count_class_0 <- length(train_class_0_indices)
count_class_1 <- length(train_class_1_indices)

# Find the minimum count between classes
min_count <- min(count_class_0, count_class_1)

# Randomly undersample the over-represented class ('1's) to match the size of the under-represented class ('0's)
sampled_indices_class_0 <- sample(train_class_0_indices, min_count)
sampled_indices_class_1 <- sample(train_class_1_indices, min_count)

# Combine the sampled indices of both classes
balanced_train_indices <- c(sampled_indices_class_0, sampled_indices_class_1)

# Create the balanced training dataset
train.df <- train.df[balanced_train_indices, ]
valid.df = ob_dt[-balanced_train_indices, ]

default.ct <- rpart(NObeyesdad ~ ., data = train.df ,method = "class",cp = -1,minsplit = 1)

# plot tree
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = 10)
# count number of leaves
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])


printcp(default.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.

pruned.ct <- prune(default.ct, cp = 0.00350140)

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

default.ct.point.pred.trainP <- predict(pruned.ct,train.df,type = "class")
# generate confusion matrix for Training data
confusionMatrix(default.ct.point.pred.trainP, as.factor(train.df$NObeyesdad))

default.ct.point.pred.validP <- predict(pruned.ct,valid.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred.validP, as.factor(valid.df$NObeyesdad))


## Random Forest
library(randomForest)
rf <- randomForest(as.factor(NObeyesdad) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

# confusion matrix for train data
rf.pred.train <- predict(rf, train.df)
confusionMatrix(rf.pred.train, as.factor(train.df$NObeyesdad))


# confusion matrix for validation data
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$NObeyesdad))

#Boosted Tree
library(adabag)

train.df$NObeyesdad <- as.factor(train.df$NObeyesdad)
boost <- boosting(NObeyesdad ~ ., data = train.df)

#Confusion matrix for train data
pred.train <- predict(boost, train.df)
confusionMatrix(as.factor(pred.train$class), as.factor(train.df$NObeyesdad))

#Confusion matrix for validation data
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$NObeyesdad))

#Logit Model
logit.reg <- glm(NObeyesdad ~ . , data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)


#Backward step model
backwards = step(logit.reg)
summary(backwards)

#Confusion matrix for Training data
logit.reg.pred_bk.train <- predict(backwards, train.df, type = "response")
logit.reg.pred.classes_bk.train <- ifelse(logit.reg.pred_bk.train > 0.5, 1, 0)
confusionMatrix(as.factor(logit.reg.pred.classes_bk.train), as.factor(train.df$NObeyesdad))

#Confusion matrix for Validation data
logit.reg.pred_bk <- predict(backwards, valid.df, type = "response")
logit.reg.pred.classes_bk <- ifelse(logit.reg.pred_bk > 0.5, 1, 0)
confusionMatrix(as.factor(logit.reg.pred.classes_bk), as.factor(valid.df$NObeyesdad))

#Neural Network
install.packages(c('neuralnet'), dependencies = T)
library(neuralnet)
library(caret)

train.df$NObeyesdad <- as.numeric(train.df$NObeyesdad) - 1

nn <- neuralnet(NObeyesdad ~ ., data = train.df, linear.output = FALSE, hidden = 2)
plot(nn, rep="best")

#Confusion matrix for Training data
nn.pred.train <- predict(nn, train.df, type = "response")
nn.pred.classes.train <- ifelse(nn.pred.train > 0.5, 1, 0)
confusionMatrix(as.factor(nn.pred.classes.train), as.factor(train.df$NObeyesdad))

#Confusion matrix for validation data
nn.pred <- predict(nn, valid.df, type = "response")
nn.pred.classes <- ifelse(nn.pred > 0.5, 1, 0)
confusionMatrix(as.factor(nn.pred.classes), as.factor(valid.df$NObeyesdad))

#ROC curve
library(pROC)

#Decision Tree
default.ct.point.pred.validP = as.numeric(default.ct.point.pred.validP)-1
r1 <- roc(valid.df$NObeyesdad, default.ct.point.pred.validP)
plot.roc(r1, main = "ROC curve for Decision Tree")

auc(r1)
#Random Forest
rf.pred = as.numeric(rf.pred)-1
r4 <- roc(valid.df$NObeyesdad, rf.pred)
plot.roc(r4, main = "ROC curve for Random Forest")
auc(r4)

#Boosted Tree
pred$class = as.numeric(pred$class)-1
r5 <- roc(valid.df$NObeyesdad, pred$class)
plot.roc(r5, main = "ROC curve for Boosted Tree")
auc(r5)

#Logit Regression
r2 <- roc(valid.df$NObeyesdad, logit.reg.pred.classes_bk)
plot.roc(r2, main = "ROC curve for logit Regression" )
auc(r2)

#Neural Network
r3 <- roc(valid.df$NObeyesdad, nn.pred.classes)
plot.roc(r3, main = "ROC curve for Neural Network")
auc(r3)

#Based on ROC index and AUC curve, Boosted Tree is showing best results.

#ROC curve comparison
plot.roc(r1, main = "ROC curve",col = "skyblue")
plot.roc(r4, main = "ROC curve for Random Forest", add = TRUE, col = "blue")
plot.roc(r5, main = "ROC curve for Boosted Tree", add = TRUE, col = "red")
plot.roc(r2, main = "ROC curve for logit Regression", add = TRUE, col = "green")
plot.roc(r3, main = "ROC curve for Neural Network", add = TRUE, col = "Orange")

legend("bottomright", legend = c("Decision Tree", "Random Forest", "Boosted Tree", "Logit Regression", "Neural Network"),
       col = c("skyblue", "blue", "red", "green", "Orange"), lwd = 2)
