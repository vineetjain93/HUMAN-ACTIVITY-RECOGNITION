##SERVER FILE################################################################
library(rpart)
library(caret)
library(e1071)
library(rattle)
library(MASS)
library(randomForest)


## SETTING THE RSTUDIO DIRECTORY TO OUR WORKING DIRECTORY and changing column width######

setwd("c:/Users/Vin/Desktop/HAR/Major Project/")
options(width=9999)

##READING IN RAW DATA FILES ###############################################


X_train <- read.table("./Data/X_train.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
X_test <- read.table("./Data/X_test.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)

y_train <- read.table("./Data/y_train.txt", quote="\"", comment.char="")
y_test <- read.table("./Data/y_test.txt", quote="\"", comment.char="")

activity_labels <- read.table("./Data/activity_labels.txt") 

###Reading in accuracy values from rds file ################################

if(file.exists("./accuracyvaluesrpart.rds")){
        accuracyvaluesrpart <- readRDS("./accuracyvaluesrpart.rds")
} else { accuracyvaluesrpart <- list(NULL)
}

if(file.exists("./accuracyvaluesnaivebayes.rds")){
        accuracyvaluesnaivebayes <- readRDS("./accuracyvaluesnaivebayes.rds")
} else { accuracyvaluesnaivebayes <- list(NULL)
}

if(file.exists("./accuracyvaluessvm.rds")){
        accuracyvaluessvm <- readRDS("./accuracyvaluessvm.rds")
} else { accuracyvaluessvm <- list(NULL)
}

if(file.exists("./accuracyvaluesrf.rds")){
        accuracyvaluesrf <- readRDS("./accuracyvaluesrf.rds")
}else { accuracyvaluesrf <- list(NULL)
}


## Additional functions ####################################################33

rand.which.max=function(x){
        index=((1:length(x))[x==(max(x))])
        return(sample(c(index,index),1))
}

############################################################################3

y_train$V1 <- as.factor(y_train$V1)
y_test$V1 <- as.factor(y_test$V1)
levelsOfActivities <- activity_labels$V2
levels(y_train$V1) <- levelsOfActivities
levels(y_test$V1) <- levelsOfActivities
## Creating a Testing subset (hardcoded at the moment)####################33#
##problem_id <- c(1,100,1000,50,500,1500,2000,34,22,123,653,234,654,443,33,999,2,3,4,5)

problem_id <- sample(1:length(X_test$V1),size=50,replace=FALSE)
testingSet  <- X_test[problem_id,]
rightPrediction <-y_test$V1[problem_id]
# levels(rightPrediction) <- levelsOfActivities

## MODEL CREATION ##########################################################

##DECISION TREES##
        if(!file.exists("./rpart.rds")){
                HARtree = rpart(as.factor(y_train$V1)~., data = X_train)
                saveRDS(HARtree,"./rpart.rds")
        }else {
                HARtree <- readRDS("./rpart.rds")
        }

##NAIVE BAYES##
        if(!file.exists("./naivebayes.rds")){
                HARbayes <- naiveBayes(y_train$V1~., data = X_train)
                saveRDS(HARbayes,"./naivebayes.rds")
        }else{
                HARbayes <- readRDS("./naivebayes.rds")
        }
##SVM##
        if(!file.exists("./svm.rds")){
                HARsvm <- svm(y_train$V1~., data = X_train)
                 saveRDS(HARsvm,"./svm.rds")
        }else{
                HARsvm <- readRDS("./svm.rds")
        }
##RANDOM FOREST##
        if(!file.exists("./rf.rds")){
                HARrf <- randomForest(y_train$V1~.,data = X_train)
                saveRDS(HARrf,"./rf.rds")
        }else{
                HARrf <- readRDS("./rf.rds")
        }
####################################################################

#PREDICTIONS#

##decision tree prediciton##

###on Training### 
if(!file.exists("./accuracyValuesrpart.rds"))
        {
        
        train.pred.rpart <- predict (HARtree,X_train)
        train.pred.rpart <- apply(train.pred.rpart,1,which.max)
        train.pred.rpart <- as.factor(train.pred.rpart)
        levels(train.pred.rpart) <- levelsOfActivities 
        train.matrix.rpart <- table(train.pred.rpart,y_train$V1)
        train.accuracy.rpart <- sum(diag(train.matrix.rpart))/sum(train.matrix.rpart)*100
        
        
        
        
        accuracyvaluesrpart <- c(accuracyvaluesrpart,train.accuracy.rpart=train.accuracy.rpart)
        
                
}

#on Testing 

if(!file.exists("./accuracyValuesrpart.rds"))
{
        
        testing.pred.rpart <- predict (HARtree,X_test)
        testing.pred.rpart <- apply(testing.pred.rpart,1,which.max)
        testing.pred.rpart <- as.factor(testing.pred.rpart)
        levels(testing.pred.rpart) <- levelsOfActivities 
        
        testing.matrix.rpart <- table(testing.pred.rpart,y_test$V1)
        testing.accuracy.rpart <- sum(diag(testing.matrix.rpart))/sum(testing.matrix.rpart)*100
        
        totalinstances <- sum(testing.matrix.rpart)
        totaltrueinstances <- sum(diag(testing.matrix.rpart))
        
        testing.precision.rpart <- NULL
        testing.sensitivity.rpart <- NULL
        testing.specificity.rpart <- NULL
        testing.fscore.rpart <- NULL
        
        for( i in 1:length(levelsOfActivities)){
                TP=testing.matrix.rpart[i,i]
                TN=totaltrueinstances-TP
                FP=sum(testing.matrix.rpart[,i])-TP
                FN=sum(testing.matrix.rpart[i,])-TP
                
                #precision - how often is it correct when it actually predicts yes  
                testing.precision.rpart=c(testing.precision.rpart,TP / (TP + FP) )
                #TPR- predicting yes when it's actually yes
                testing.sensitivity.rpart = c(testing.sensitivity.rpart,TP / (TP + FN))
                ##1-FPR - specificity - when it's no ,how often does it predict no.
                testing.specificity.rpart = c(testing.specificity.rpart,TN / (FP + TN))
                ##F-SCO
                testing.fscore.rpart = c(testing.fscore.rpart,2*TP /(2*TP + FP + FN))
                
                
        }
        
        accuracyvaluesrpart <- c(accuracyvaluesrpart,testing.accuracy.rpart=testing.accuracy.rpart)
        accuracyvaluesrpart <- c(accuracyvaluesrpart,testing.precision.rpart=list(testing.precision.rpart))
        accuracyvaluesrpart <- c(accuracyvaluesrpart,testing.sensitivity.rpart=list(testing.sensitivity.rpart))
        accuracyvaluesrpart <- c(accuracyvaluesrpart,testing.specificity.rpart=list(testing.specificity.rpart))
        accuracyvaluesrpart <- c(accuracyvaluesrpart,testing.fscore.rpart=list(testing.fscore.rpart))
}

#On TestingSet 
testingSet.pred.rpart <- predict(HARtree,testingSet)
testingSet.pred.rpart <- apply(testingSet.pred.rpart, 1, which.max)
testingSet.pred.rpart <- as.factor(testingSet.pred.rpart)
levels(testingSet.pred.rpart) <- levelsOfActivities

##naive bayes prediction ########

##On Training 

if(!file.exists("./accuracyValuesnaivebayes.rds"))
{
        
        train.pred.naivebayes <- predict (HARbayes,X_train,type = "raw")
        train.pred.naivebayes <- apply(train.pred.naivebayes,1,rand.which.max)
        train.pred.naivebayes <- as.factor(train.pred.naivebayes)
        levels(train.pred.naivebayes) <- levelsOfActivities 
        
        train.matrix.naivebayes <- table(train.pred.naivebayes,y_train$V1)
        train.accuracy.naivebayes <- sum(diag(train.matrix.naivebayes))/sum(train.matrix.naivebayes)*100
        
        accuracyvaluesnaivebayes <- c(accuracyvaluesnaivebayes,train.accuracy.naivebayes=train.accuracy.naivebayes)
}

##on Testing
if(!file.exists("./accuracyValuesnaivebayes.rds"))
{
        
        testing.pred.naivebayes <- predict (HARbayes,X_test,type = "raw")
        testing.pred.naivebayes <- apply(testing.pred.naivebayes,1,rand.which.max)
        testing.pred.naivebayes <- as.factor(testing.pred.naivebayes)
        levels(testing.pred.naivebayes) <- levelsOfActivities 
        
        testing.matrix.naivebayes <- table(testing.pred.naivebayes,y_test$V1)
        testing.accuracy.naivebayes <- sum(diag(testing.matrix.naivebayes))/sum(testing.matrix.naivebayes)*100
        
        totalinstances <- sum(testing.matrix.naivebayes)
        totaltrueinstances <- sum(diag(testing.matrix.naivebayes))
        
        testing.precision.naivebayes <- NULL
        testing.sensitivity.naivebayes <- NULL
        testing.specificity.naivebayes <- NULL
        testing.fscore.naivebayes <- NULL
        
        for( i in 1:length(levelsOfActivities)){
                TP=testing.matrix.naivebayes[i,i]
                TN=totaltrueinstances-TP
                FP=sum(testing.matrix.naivebayes[,i])-TP
                FN=sum(testing.matrix.naivebayes[i,])-TP
                
                testing.precision.naivebayes=c(testing.precision.naivebayes,TP / (TP + FP) )
                testing.sensitivity.naivebayes = c(testing.sensitivity.naivebayes,TP / (TP + FN))
                testing.specificity.naivebayes = c(testing.specificity.naivebayes,TN / (FP + TN))
                testing.fscore.naivebayes = c(testing.fscore.naivebayes,2*TP /(2*TP + FP + FN))
                
                
        }
        
        accuracyvaluesnaivebayes <- c(accuracyvaluesnaivebayes,testing.accuracy.naivebayes=testing.accuracy.naivebayes)
        accuracyvaluesnaivebayes <- c(accuracyvaluesnaivebayes,testing.precision.naivebayes=list(testing.precision.naivebayes))
        accuracyvaluesnaivebayes <- c(accuracyvaluesnaivebayes,testing.sensitivity.naivebayes=list(testing.sensitivity.naivebayes))
        accuracyvaluesnaivebayes <- c(accuracyvaluesnaivebayes,testing.specificity.naivebayes=list(testing.specificity.naivebayes))
        accuracyvaluesnaivebayes <- c(accuracyvaluesnaivebayes,testing.fscore.naivebayes=list(testing.fscore.naivebayes))

}

##On TestingSet
testingSet.pred.naivebayes <- predict(HARbayes,testingSet, type = "raw")
testingSet.pred.naivebayes <- apply(testingSet.pred.naivebayes, 1, rand.which.max)
testingSet.pred.naivebayes <- as.factor(testingSet.pred.naivebayes)
levels(testingSet.pred.naivebayes) <- levelsOfActivities

##svm prediction ################
#on training 
if(!file.exists("./accuracyValuessvm.rds"))
{
        
        train.pred.svm <- predict (HARsvm,X_train)
        train.pred.svm <- as.factor(train.pred.svm)
        levels(train.pred.svm) <- levelsOfActivities 
        
        train.matrix.svm <- table(train.pred.svm,y_train$V1)
        train.accuracy.svm <- sum(diag(train.matrix.svm))/sum(train.matrix.svm)*100
        
        accuracyvaluessvm <- c(accuracyvaluessvm,train.accuracy.svm=train.accuracy.svm)
}


#on testing 
if(!file.exists("./accuracyValuessvm.rds"))
{
        
        testing.pred.svm <- predict (HARsvm,X_test)
        testing.pred.svm <- as.factor(testing.pred.svm)
        levels(testing.pred.svm) <- levelsOfActivities 
        
        testing.matrix.svm <- table(testing.pred.svm,y_test$V1)
        testing.accuracy.svm <- sum(diag(testing.matrix.svm))/sum(testing.matrix.svm)*100
        
        totalinstances <- sum(testing.matrix.svm)
        totaltrueinstances <- sum(diag(testing.matrix.svm))
        
        testing.precision.svm <- NULL
        testing.sensitivity.svm <- NULL
        testing.specificity.svm <- NULL
        testing.fscore.svm <- NULL
        
        for( i in 1:length(levelsOfActivities)){
                TP=testing.matrix.svm[i,i]
                TN=totaltrueinstances-TP
                FP=sum(testing.matrix.svm[,i])-TP
                FN=sum(testing.matrix.svm[i,])-TP
                
                testing.precision.svm=c(testing.precision.svm,TP / (TP + FP) )
                testing.sensitivity.svm = c(testing.sensitivity.svm,TP / (TP + FN))
                testing.specificity.svm = c(testing.specificity.svm,TN / (FP + TN))
                testing.fscore.svm = c(testing.fscore.svm,2*TP /(2*TP + FP + FN))
                
                
        }
        
        accuracyvaluessvm <- c(accuracyvaluessvm,testing.accuracy.svm=testing.accuracy.svm)
        accuracyvaluessvm <- c(accuracyvaluessvm,testing.precision.svm=list(testing.precision.svm))
        accuracyvaluessvm <- c(accuracyvaluessvm,testing.sensitivity.svm=list(testing.sensitivity.svm))
        accuracyvaluessvm <- c(accuracyvaluessvm,testing.specificity.svm=list(testing.specificity.svm))
        accuracyvaluessvm <- c(accuracyvaluessvm,testing.fscore.svm=list(testing.fscore.svm))
}


#on TestingSet
testingSet.pred.svm <- predict(HARsvm,testingSet)
testingSet.pred.svm <- as.factor(testingSet.pred.svm)
levels(testingSet.pred.svm) <- levelsOfActivities 

##RANDOM FOREST## 

#on train
if(!file.exists("./accuracyValuesrf.rds"))
{
        
        train.pred.rf <- predict (HARrf,X_train)
        train.pred.rf <- round(train.pred.rf)
        train.pred.rf <- as.factor(train.pred.rf)
        levels(train.pred.rf) <- levelsOfActivities 
        
        train.matrix.rf <- table(train.pred.rf,y_train$V1)
        train.accuracy.rf <- sum(diag(train.matrix.rf))/sum(train.matrix.rf)*100
        
        accuracyvaluesrf <- c(accuracyvaluesrf,train.accuracy.rf=train.accuracy.rf)
}


#on testing 
if(!file.exists("./accuracyValuesrf.rds"))
{
        
        testing.pred.rf <- predict (HARrf,X_test)
        testing.pred.rf <- round(testing.pred.rf)
        testing.pred.rf <- as.factor(testing.pred.rf)
        levels(testing.pred.rf) <- levelsOfActivities 
        
        testing.matrix.rf <- table(testing.pred.rf,y_test$V1)
        testing.accuracy.rf <- sum(diag(testing.matrix.rf))/sum(testing.matrix.rf)*100
        
        totalinstances <- sum(testing.matrix.rf)
        totaltrueinstances <- sum(diag(testing.matrix.rf))
        
        testing.precision.rf <- NULL
        testing.sensitivity.rf <- NULL
        testing.specificity.rf <- NULL
        testing.fscore.rf <- NULL
        
        for( i in 1:length(levelsOfActivities)){
                TP=testing.matrix.rf[i,i]
                TN=totaltrueinstances-TP
                FP=sum(testing.matrix.rf[,i])-TP
                FN=sum(testing.matrix.rf[i,])-TP
                
                
                testing.precision.rf=c(testing.precision.rf,TP / (TP + FP) )
                
                testing.sensitivity.rf = c(testing.sensitivity.rf,TP / (TP + FN))  
                
                testing.specificity.rf = c(testing.specificity.rf,TN / (FP + TN))
                
                testing.fscore.rf = c(testing.fscore.rf,2*TP /(2*TP + FP + FN))
                
                
        }
        
        accuracyvaluesrf <- c(accuracyvaluesrf,testing.accuracy.rf=testing.accuracy.rf)
        accuracyvaluesrf <- c(accuracyvaluesrf,testing.precision.rf=list(testing.precision.rf))
        accuracyvaluesrf <- c(accuracyvaluesrf,testing.sensitivity.rf=list(testing.sensitivity.rf))
        accuracyvaluesrf <- c(accuracyvaluesrf,testing.specificity.rf=list(testing.specificity.rf))
        accuracyvaluesrf <- c(accuracyvaluesrf,testing.fscore.rf=list(testing.fscore.rf))
}
        
        



#on TestingSet
testingSet.pred.rf <- predict(HARrf,testingSet)
testingSet.pred.rf <- round(testingSet.pred.rf)
testingSet.pred.rf <- as.factor(testingSet.pred.rf)
levels(testingSet.pred.rf) <- levelsOfActivities 




####creation of rds for accuracyvalues #####################################

if(!file.exists("./accuracyvaluesrpart.rds"))
{
        saveRDS(accuracyvaluesrpart,"./accuracyvaluesrpart.rds")
}

if(!file.exists("./accuracyvaluesnaivebayes.rds"))
{
        saveRDS(accuracyvaluesnaivebayes,"./accuracyvaluesnaivebayes.rds")
}

if(!file.exists("./accuracyvaluessvm.rds"))
{
        saveRDS(accuracyvaluessvm,"./accuracyvaluessvm.rds")
}

if(!file.exists("./accuracyvaluesrf.rds"))
{
        saveRDS(accuracyvaluesrf,"./accuracyvaluesrf.rds")
}

## HAR FUNCTION##############################################################

har <- function (testId, Model) {
        if (Model == "Decision Trees") {prediction <- predict(HARtree, testingSet[testId, ])
                               prediction <- which.max(prediction)
                               return(as.character(levelsOfActivities[prediction]))        
        }
        
        if (Model == "Naive Bayes") {prediction <- predict(HARbayes,testingSet[testId, ])
                                     
                                     return(as.character(prediction))
        
        }
      
         if (Model == "Support Vector Machine (svm)"){ prediction <-predict(HARsvm, testingSet[testId, ])
                                                        names(prediction) <- NULL
                                                        return (as.character(prediction))
         }
        
        if (Model == "Random Forest"){ prediction <- predict(HARrf,testingSet[testId,])
                                       prediction <- round(prediction)
                                       return(as.character(levelsOfActivities[prediction]))
                                                                                                        
                                                
        }
}

## CONFUSION MATRIX############################################################

mat <- function (Model) {
        if (Model == "Decision Trees") return(confusionMatrix(testingSet.pred.rpart, rightPrediction)[[2]])
        if (Model == "Naive Bayes") return(confusionMatrix(testingSet.pred.naivebayes, rightPrediction)[[2]])
        
        if (Model == "Support Vector Machine (svm)") return(confusionMatrix(testingSet.pred.svm, rightPrediction)[[2]])

        if (Model == "Random Forest") return(confusionMatrix(testingSet.pred.rf,rightPrediction)[[2]])        
}

## ACCURACY #################################################################
acc <- function () {
        matrixrpart <- table(testingSet.pred.rpart,rightPrediction)
        accuracyrpart <- sum(diag(matrixrpart))/sum(matrixrpart)*100
        
        matrixnaivebayes<- table(testingSet.pred.naivebayes,rightPrediction)
        accuracynaivebayes <- sum(diag(matrixnaivebayes))/sum(matrixnaivebayes)*100
        
        matrixsvm <- table(testingSet.pred.svm,rightPrediction)
        accuracysvm <- sum(diag(matrixsvm))/sum(matrixsvm)*100
        
        matrixrf <- table(testingSet.pred.rf,rightPrediction)
        accuracyrf <- sum(diag(matrixrf))/sum(matrixrf)*100
        
        accuracy <- data.frame(Model=c("Decision Trees", "Naive Bayes","Support Vector Machine (svm)","Random Forest"),
                               
                               TrainAcc=c(accuracyvaluesrpart$train.accuracy.rpart,
                                          accuracyvaluesnaivebayes$train.accuracy.naivebayes,
                                          accuracyvaluessvm$train.accuracy.svm,
                                          accuracyvaluesrf$train.accuracy.rf),
                               
                               
                                TestAcc=c(accuracyvaluesrpart$testing.accuracy.rpart ,
                                          accuracyvaluesnaivebayes$testing.accuracy.naivebayes,
                                          accuracyvaluessvm$testing.accuracy.svm,
                                          accuracyvaluesrf$testing.accuracy.rf),
                              
                               
                                TestSetAccuracy = c(accuracyrpart,accuracynaivebayes,accuracysvm,accuracyrf))
        
        return(accuracy)
}
##PERFORMANCE###############################################################
perfrpart <- function() {
        
        performancerpart <- data.frame(PRECISION=accuracyvaluesrpart$testing.precision.rpart,
                                       SENSITIVITY=accuracyvaluesrpart$testing.sensitivity.rpart,
                                       SPEICIFICITY=accuracyvaluesrpart$testing.specificity.rpart,
                                       FSCORE=accuracyvaluesrpart$testing.fscore.rpart)
        
        rownames(performancerpart) <- levelsOfActivities
        return(performancerpart)        
}

perfnaivebayes <- function() {
        
        performancenaivebayes <- data.frame(PRECISION=accuracyvaluesnaivebayes$testing.precision.naivebayes,
                                            SENSITIVITY=accuracyvaluesnaivebayes$testing.sensitivity.naivebayes,
                                            SPEICIFICITY=accuracyvaluesnaivebayes$testing.specificity.naivebayes,
                                            FSCORE=accuracyvaluesnaivebayes$testing.fscore.naivebayes)
        
        rownames(performancenaivebayes) <- levelsOfActivities
      
        return(performancenaivebayes)        
}

perfsvm <- function() {
        
        performancesvm <- data.frame(PRECISION=accuracyvaluessvm$testing.precision.svm,
                                     SENSITIVITY=accuracyvaluessvm$testing.sensitivity.svm,
                                     SPEICIFICITY=accuracyvaluessvm$testing.specificity.svm,
                                     FSCORE=accuracyvaluessvm$testing.fscore.svm)
        
        rownames(performancesvm) <- levelsOfActivities
        return(performancesvm)        
}

perfrf <- function() {
        
        performancerf <- data.frame(PRECISION=accuracyvaluesrf$testing.precision.rf,
                                    SENSITIVITY=accuracyvaluesrf$testing.sensitivity.rf,
                                    SPEICIFICITY=accuracyvaluesrf$testing.specificity.rf,
                                    FSCORE=accuracyvaluesrf$testing.fscore.rf)
        
        rownames(performancerf) <- levelsOfActivities
        return(performancerf)        
}




##SHINY SERVER FUNCTION #####################################################

shinyServer( function(input, output) {
        output$inputValue <- renderPrint({input$testId})
        output$dataModel <- renderPrint({input$Model})
        output$prediction <- renderPrint({har(input$testId, input$Model)})
        output$rightPrediction <- renderPrint({as.character(rightPrediction[input$testId])})
        output$ConfMatrix <- renderPrint({mat(input$Model)})
        output$accuracy <- renderPrint({acc()})
        output$performancerpart <- renderPrint({perfrpart()})
        output$performancenaivebayes <- renderPrint({perfnaivebayes()})
        output$performancesvm <- renderPrint({perfsvm()})
        output$performancerf <- renderPrint({perfrf()})
}
)


