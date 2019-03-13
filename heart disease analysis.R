getwd()
setwd("E:sadir")

heartdata<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data")
View(heartdata)
names(heartdata) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
str(heartdata)
is.na(heartdata)
anyNA(heartdata)
is.finite(c(heartdata))
complete.cases(heartdata)
#barplot(table(heartdata$num)x)
plot(heartdata$num)

is.special <- function(heartdata){
  if (is.numeric(heartdata)) !is.finite(heartdata) else is.na(heartdata)
}
zx<-ifelse(heartdata$ca=="?","wegfasg","f")
zx
length(zx=="wegfasg")
which(myData$ca %in% c("?"))

heartdata <- heartdata[-c(166, 192, 287,302,87,266), ]

#percentage of people having disease
163/(163+139)
139/(163+139)

heartdata$num<-ifelse(heartdata$num> 0,"disease","no disease")
length(heartdata$num[heartdata$num==0])
#ggplot(heartdata,aes(x=num))+geom_boxplot()
table(history)
barplot(table(history))
mosaicplot(heartdata$sex~history)
boxplot(heartdata$sex~history)
heartdata$sex<-ifelse(heartdata$sex==0,"female","male")
table(heartdata$sex)
ab<-table(gender=heartdata$sex,disease=heartdata$num)
ab
ggplot(heartdata,aes(x=sex))+geom_bar(fill="blue")+facet_wrap(~num)
ggplot(heartdata,aes(x=num,y=age))+geom_boxplot()
heartdata$num
heartdata$age
by(heartdata$age,heartdata$num,summary)
cor(heartdata)
stepAIC(fit123,direction = "backward")

str(heartdata)
heartdata$num<-ifelse(heartdata$num=="disease",0,1)
heartdata$sex<-ifelse(heartdata$sex=="female",0,1)
fit123<-lm(num~.,heartdata)
logistic_model <- glm(num~., family=binomial(link="logit"), data=heartdata)
summary(logistic_model)


#train data
set.seed(123)
str(heartdata)
row.number <- sample(x=1:nrow(heartdata), size=0.8*nrow(heartdata))
train = heartdata[row.number,]
test = heartdata[-row.number,]
head(train)

fit<-glm(num~.,data=train, family="binomial")
stepAIC(fit,direction = "backward")

fit1<-glm(formula = num ~ sex + cp + trestbps + restecg + thalach + 
            exang + oldpeak + slope + ca + thal, family = "binomial", 
          data = train)
pred_ex<-predict(fit1,newdata = test,type = "response")

pred_ex
pred_num <- ifelse(pred_ex > 0.5, 1, 0)
predictedf <- factor(pred_num, levels=c(0, 1))
test$num1<-factor(test$num,levels=c(0,1))
confmatrix<-confusionMatrix(predictedf,test[,"num1"])
str(predictedf)
install.packages('e1071', dependencies=TRUE)
confmatrix
plot.roc(as.numeric(test$num1),as.numeric(as.matrix(pred_ex)))
mean(test$num1==predictedf)
confmatrix$overall['Accuracy']

#tree
################################################################################3
View(heartdata)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# grow tree 
fit2 <- rpart(formula = num ~ sex + cp + trestbps + restecg + thalach + 
                         exang + oldpeak + slope + ca + thal, method = "class", 
                       data = train)

printcp(fit2) # display the results 
plotcp(fit2) # visualize cross-validation results 
summary(fit2) # detailed summary of splits

# plot tree 
rpart.plot(fit2,type=1)


plot(fit2, uniform=TRUE, 
     main="prediction of heart attack")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(fit2, cp=   fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#######################################################################################3

#tree

set.seed(123)
row.number1 <- sample(x=1:nrow(heartdata), size=0.8*nrow(heartdata))
train1 = heartdata[row.number,]
test1 = heartdata[-row.number,]
head(train1)

install.packages("tree")
library(tree)



fit2 <- tree(num ~ sex + cp + trestbps + restecg + thalach + 
                     exang + oldpeak + slope + ca + thal, data=train)


fit2
summary(fit2)
plot(fit2,uniform=TRUE)
text(fit2,cex=.8)

#plot(fit2,main="prediction of heart attack")
#text(fit2, cex=.8)





#Get predicted values of y
#Probability prediction
preds <- predict(fit2, test) # gives the probability for each class
head(preds)


########################################################
# Point prediction
# Let's translate the probability output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(preds, c(1), maxidx)
prediction <- c('setosa', 'versicolor', 'virginica')[idx]
table(prediction, test.set$Species)
#################################################################



#We can try pruning our tree to reduce overfitting and try to improve test fit:
pruned.tree <- prune.tree(fit2, best=6)
plot(pruned.tree)
text(pruned.tree)
pruned.prediction <- predict(pruned.tree, test, type="vector") # give the predicted class
table(pruned.prediction, test$num)

#Can also use cros validation to find the best tree
tree.model <- tree(num ~ ., data=heartdata)
summary(tree.model)
cv.model <- cv.tree(tree.model)
plot(cv.model)
best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))] # which size is better?
best.size

cv.model.pruned <- prune.rpart(tree.model,cp=7)
summary(cv.model.pruned)
pruned.prediction <- predict(cv.model.pruned, test, type="vector") # give the predicted class
table(pruned.prediction, test$num)


#######################################################


##################################################################
install.packages("e1071")
set.seed(123)
svmModel <- svm(num ~ ., data = train1,
                  method = "svmRadial",
                  trControl = fitControl,
                  preProcess = c("center", "scale"),
                  tuneLength = 8,
                  metric = "ROC")
svmPrediction <- predict(svmModel, test1)
svml<-ifelse(svmPrediction>0.5,1,0)
svml1<-factor(svml,levels=c(0,1))
test1$num<-factor(test1$num,levels=c(0,1))
svmPredictionprob <- predict(svmModel, test1, type='prob')[2]
svmConfMat <- confusionMatrix(svml1, test1[,"num"])
svmConfMat
svml1

mean(test1$num==svml)

str(test1)
str(svml)
test[,"num"]
