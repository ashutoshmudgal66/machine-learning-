install.packages("mlbench")
library(mlbench)
data("BreastCancer")
View(BreastCancer)
str(BreastCancer)
BreastCancer<-BreastCancer[,-1]
library(VIM)
library(caret)
library(colorspace)
library(grid)
library(data.table)
library(Rcpp)
library(mice)
library(party)
#logistic regression
imputation_plot <- aggr(BreastCancer,col = c("yellow","blue"),
                        numbers = TRUE,sortVars = TRUE,
                        labels = names(BreastCancer),gap = 3,
                        ylab = c("Missing Data","Pattern"))
imputed_Data <- mice(BreastCancer, m=5, maxit = 50, method = 'cart', seed = 500)
completeData <- complete(imputed_Data,4)

imputation_plot2 <- aggr(completeData,col = c("yellow","blue"),
                         numbers = TRUE,sortVars = TRUE,
                         labels = names(completeData),gap = 3,
                         ylab = c("Missing Data","Pattern"))
set.seed(100)
trainingRowindex<-sample(1:nrow(completeData),0.75*nrow(completeData))
trainingData<-completeData[trainingRowindex,]
testData<-completeData[-trainingRowindex,]
testData

lmMod<-glm(Class ~.,data=trainingData,family = "binomial")
distPred<-predict(lmMod,testData)
summary(lmMod)
attach(trainingData)
testData$Class <-factor(testData$Class)
levels(testData$Class)<-levels(testData$Class)
testData$Dataoutput<-predict(lmMod,testData,type="response")
table(round(testData$Dataoutput),testData$Class)
sum(diag(table(round(testData$Dataoutput),testData$Class)))/sum(table(round(testData$Dataoutput),testData$Class))


##CTREE
output.tree<-ctree(Class~.,data=testData,
                   controls = ctree_control(maxdepth = 3))
#plot the tree
plot(output.tree)
testData$output<-predict(output.tree,testData)
table((testData$output),testData$Class)
sum(diag(table((testData$output),testData$Class)))/sum(table((testData$output),testData$Class))

##RPART
library(rpart)
fit<-rpart(Class~.,method = "class",data =trainingData)
printcp(fit)
plot(fit)
fit
testData$output<-predict(fit,testData,type="class")
confusion=table((testData$output),testData$Class)
confusion
accuracy<-sum(diag(confusion))/sum(confusion)
accuracy

#### RANDOM FOREST
library(randomForest)
fit<-randomForest(Class ~.,method = "class",data=trainingData)
output.forest<-randomForest(Class~.,data=trainingData,ntree=1600)
output.forest
testData$output.forest<-predict(fit,testData,type = "class")
testData$output.forest
confusion<-table(testData$output.forest,testData$Class)
accuracy<-sum(diag(confusion))/sum(confusion)
accuracy

## Naive Bayes
library(e1071)
modelNB<- naiveBayes(Class ~.,data = trainingData)
modelNB
pred<-predict(modelNB,testData)
pred
confusion1<-table(pred,testData$Class)
accuracy<-sum(diag(confusion1))/sum(confusion1)
accuracy

## KNN
library(class)
TrainLabels<- trainingData[,10]
TestLabels<- testData[,10]
knnModel<- knn(trainingData[,-10],testData[,-10],cl=TrainLabels,k=10,prob=T)
library(gmodels)
CrossTable(testData[,10],knnModel,prop.chisq = F)
conf_knn1<-table(TestLabels,knnModel)
sum(diag(conf_knn1))/sum(conf_knn1)







