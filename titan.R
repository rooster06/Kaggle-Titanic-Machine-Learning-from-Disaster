rm(list = ls())

## *** Titanic: Machine Learning from Disaster *** 


####################################################
## packages required: MASS, randomForest, e1071 ####
####################################################

##################################
###### loading the data ##########
##################################
setwd("Desktop/titanic")
titanic.trainDat <- read.csv("train.csv", header = TRUE)
titanic.testDat <- read.csv("test.csv", header = TRUE)
head(titanic.trainDat)

##############################################
##### Data Exploration & PRE-PROCESSING ######
##############################################
## check the proportions of survivors based on port of entry
table(titanic.trainDat$Survived,titanic.trainDat$Embarked)
mosaicplot(~ Embarked+Survived, titanic.trainDat, color =TRUE)

##check the proportion of survival in different classes
table(titanic.trainDat$Pclass,titanic.trainDat$Survived)
mosaicplot(~Pclass+Survived,titanic.trainDat, color= TRUE)

##check the proportion of survival in diffent gender
table(titanic.trainDat$Survived,titanic.trainDat$Sex)
mosaicplot(~Sex+Survived,titanic.trainDat, color= TRUE)

## survials in different age groups
histogram(titanic.trainDat$Age[titanic.trainDat$Survived==0])
histogram(titanic.trainDat$Age[titanic.trainDat$Survived==1])

## fare distributions in diffrent classes, its weird how a few people have abnormal
## high fare price while a few have zero fare price
histogram(titanic.trainDat$Fare[titanic.trainDat$Pclass==1])
histogram(titanic.trainDat$Fare[titanic.trainDat$Pclass==2])
histogram(titanic.trainDat$Fare[titanic.trainDat$Pclass==3])
histogram(titanic.testDat$Fare[titanic.testDat$Pclass==1])
histogram(titanic.testDat$Fare[titanic.testDat$Pclass==2])
histogram(titanic.testDat$Fare[titanic.testDat$Pclass==3])

## initial guess was maybe a family member purchased tickets for everyone, but there
## seems to be no last name resemblance between high price ticket buyer and the zero price
## passangers
## just to account for the outliers in the fare prices i replaced the high fares in each 
## class with the average fare price in the respective class in both train and test dataset
aa <- which(titanic.trainDat$Pclass==1)
titanic.trainDat$Name[aa[(titanic.trainDat$Fare[aa]>400)]] 
titanic.trainDat$Name[aa[(titanic.trainDat$Fare[aa]==0)]]
titanic.trainDat$Fare[aa[(titanic.trainDat$Fare[aa]>400)]]<- mean(titanic.trainDat$Fare[aa])

aa <- which(titanic.trainDat$Pclass==2)
titanic.trainDat$Name[aa[(titanic.trainDat$Fare[aa]>50)]]
titanic.trainDat$Name[aa[(titanic.trainDat$Fare[aa]==0)]]
titanic.trainDat$Fare[aa[(titanic.trainDat$Fare[aa]>60)]] <- mean(titanic.trainDat$Fare[aa])

aa <- which(titanic.trainDat$Pclass==3)
titanic.trainDat$Name[aa[(titanic.trainDat$Fare[aa]>60)]]
titanic.trainDat$Name[aa[(titanic.trainDat$Fare[aa]==0)]]
class3 <- mean(titanic.trainDat$Fare[aa])
titanic.trainDat$Fare[aa[(titanic.trainDat$Fare[aa]>60)]] <- mean(titanic.trainDat$Fare[aa])

aab <- which(titanic.testDat$Pclass==1)
titanic.testDat$Fare[aab[(titanic.testDat$Fare[aab]>400)]]<- mean(titanic.testDat$Fare[aab])

aab <- which(titanic.testDat$Pclass==2)
titanic.testDat$Fare[aab[(titanic.testDat$Fare[aab]>60)]]<- mean(titanic.testDat$Fare[aab])

aab <- which(titanic.testDat$Pclass==3)
titanic.testDat$Fare[aab[(titanic.testDat$Fare[aab]>60)]]<- class3

## converting response and class covariate into factor level 
titanic.trainDat$Survived <- as.factor(titanic.trainDat$Survived)
titanic.trainDat$Pclass <- as.factor(titanic.trainDat$Pclass)
titanic.testDat$Pclass <- as.factor(titanic.testDat$Pclass)

## impute missing ages using the titles in the names predictor 
## for female- miss., mrs.
## for male -  mr., rev., master, Dr.
## imputing missing age values with average age for each title from both the train and
## the test set

## first lets seperate the data based on titles, the titles were obtained from
## eye balling the data

## first on train set
mr<-grep("Mr.", titanic.trainDat$Name, fixed = TRUE)
mrs<-grep("Mrs.", titanic.trainDat$Name, fixed = TRUE)
master <- grep("Master", titanic.trainDat$Name, fixed = TRUE)
rev <- grep("Rev.", titanic.trainDat$Name, fixed = TRUE)
dr <- grep("Dr.", titanic.trainDat$Name, fixed = TRUE)
miss <- grep("Miss", titanic.trainDat$Name, fixed = TRUE)

## next on test set
mr2<-grep("Mr.", titanic.testDat$Name, fixed = TRUE)
mrs2<-grep("Mrs.", titanic.testDat$Name, fixed = TRUE)
master2 <- grep("Master", titanic.testDat$Name, fixed = TRUE)
rev2 <- grep("Rev.", titanic.testDat$Name, fixed = TRUE)
dr2 <- grep("Dr.", titanic.testDat$Name, fixed = TRUE)
miss2 <- grep("Miss", titanic.testDat$Name, fixed = TRUE)

## check the reminder of the data in both the data sets for missing values
ktrain<-c(mr,mrs,master,dr,miss)
length(ktrain)
sum(is.na(titanic.trainDat$Age[-ktrain]))

## no missing values in reminder of train dataset

ktest<-c(mr2,mrs2,master2,dr2,miss2)
sum(is.na(titanic.testDat$Age[-ktest]))
titanic.testDat[89,]
## one missing value at index 89 in the reminder of test data for O'Donoghue, Ms. Bridget
## we can impute the mean of miss in this observation

### finding the means of ages for each title in each data set
## first means of ages for titles in the combined train and test data
mr.dat <- rbind(titanic.trainDat[mr,-2],titanic.testDat[mr2,])
mean.mr <- mean(mr.dat$Age[!(is.na(mr.dat$Age))])
mean.mr

mrs.dat <- rbind(titanic.trainDat[mrs,-2],titanic.testDat[mrs2,])
mean.mrs <- mean(mrs.dat$Age[!(is.na(mrs.dat$Age))])
mean.mrs

master.dat <- rbind(titanic.trainDat[master,-2],titanic.testDat[master2,])
mean.master <- mean(master.dat$Age[!(is.na(master.dat$Age))])
mean.master

miss.dat <- rbind(titanic.trainDat[miss,-2],titanic.testDat[miss2,])
mean.miss <- mean(miss.dat$Age[!(is.na(miss.dat$Age))])
mean.miss

dr.dat <- rbind(titanic.trainDat[dr,-2], titanic.testDat[dr2,])
mean.dr <- mean(dr.dat$Age[!(is.na(dr.dat$Age))])
mean.dr

## imputing the missing data
titanic.trainDat$Age[mr[which(is.na(titanic.trainDat$Age[mr]))]] <- mean.mr
titanic.trainDat$Age[mrs[which(is.na(titanic.trainDat$Age[mrs]))]] <- mean.mrs
titanic.trainDat$Age[master[which(is.na(titanic.trainDat$Age[master]))]] <- mean.master
titanic.trainDat$Age[miss[which(is.na(titanic.trainDat$Age[miss]))]] <- mean.miss
titanic.trainDat$Age[dr[which(is.na(titanic.trainDat$Age[dr]))]] <- mean.dr

titanic.testDat$Age[mr2[which(is.na(titanic.testDat$Age[mr2]))]] <- mean.mr
titanic.testDat$Age[mrs2[which(is.na(titanic.testDat$Age[mrs2]))]] <- mean.mrs
titanic.testDat$Age[master2[which(is.na(titanic.testDat$Age[master2]))]] <- mean.master
titanic.testDat$Age[miss2[which(is.na(titanic.testDat$Age[miss2]))]] <- mean.miss
titanic.testDat$Age[dr2[which(is.na(titanic.testDat$Age[dr2]))]] <- mean.dr
titanic.testDat$Age[89] <- mean.miss

## checking the embarked predictor shows that two of the observations have no data,
## lets discard these observations
dropobs <- which(titanic.trainDat$Embarked == "")
titanic.trainDat <- titanic.trainDat[-dropobs,]
titanic.trainDat$Embarked <- as.character(titanic.trainDat$Embarked)
titanic.trainDat$Embarked <- as.factor(titanic.trainDat$Embarked)

titanic.testDat$Fare[153] <- class3

## i ran through a combination of interaction terms in variables, and kept editing and rediting the
## same interaction terms code and the variable below gave a good improvement in predition

## since we know that women have a high proportion of survival and occupants of first class
## have high proportions of survival lets combine these two variables into a varibale called
## papi
titanic.trainDat$papa <- rep(0, 889)
titanic.trainDat$papa[titanic.trainDat$Pclass==1 & titanic.trainDat$Sex== "female"] <- 1
titanic.trainDat$papa[titanic.trainDat$Pclass==2 & titanic.trainDat$Sex== "female"] <- 2
titanic.trainDat$papa[titanic.trainDat$Pclass==3 & titanic.trainDat$Sex== "female"]<- 3


titanic.testDat$papa <- rep(0, 418)
titanic.testDat$papa[titanic.testDat$Pclass==1 & titanic.testDat$Sex== "female"] <- 1
titanic.testDat$papa[titanic.testDat$Pclass==2 & titanic.testDat$Sex== "female"] <- 2
titanic.testDat$papa[titanic.testDat$Pclass==3 & titanic.testDat$Sex== "female"]<- 3

### lets create a variable called tic which has the alphabet initial in the cabin variable
tic.train <- as.factor(as.character(substr(gsub("[^[:alpha:]]","",titanic.trainDat$Cabin),0,1)))
titanic.trainDat$tic<-tic.train
titanic.trainDat$tic[titanic.trainDat$tic == 'T'] <- ""
titanic.trainDat$tic<- factor(titanic.trainDat$tic)
tic.test <- as.factor(as.character(substr(gsub("[^[:alpha:]]","",titanic.testDat$Cabin),0,1)))
titanic.testDat$tic<- tic.test

##lets call the preprocessed train data as fdata
fdata <- titanic.trainDat

####################################################
######### MODEL FITTING AND SELECTION ##############  
####################################################

#*****************************
#**** logistic regression ****
#*****************************

## lets use validation set approach to select the best threshold for logistic regg
set.seed(1)
dim(fdata)
val.t <-sample(889,200)
val.train <- fdata[-val.t,]
val.test <- fdata[val.t,]
logit.val<- glm(Survived ~ Pclass+Parch*Sex+Age+SibSp+Fare+Embarked+papa, data=val.train, family = binomial)
summary(logit.val)
logit.val1<- glm(Survived ~ Pclass+Parch*Sex+Age+SibSp+Fare, data=val.train, family = binomial)
summary(logit.val1)
logit.val2<- glm(Survived ~ Pclass+Parch*Sex+Age+SibSp+Fare+papa+tic, data=val.train, family = binomial)
summary(logit.val2)
logit.val3<- glm(Survived ~ Pclass+Parch*Sex+Age+SibSp+tic+papa, data=val.train, family = binomial)
summary(logit.val3)
## this tells us that class, sex, age, sibsp, and (sex & parch), papa play an important role in 
## classification 

PER <- function(x){
  logit.probs <- predict(x, val.test, type = "response")
  per <- rep(0,10)
  for (i in 1:10) {
  out.test <- rep("1", 200)
  out.test[logit.probs < (i/10)] <- "0"
  out.test <- as.factor(out.test)
  y <- val.test$Survived
  per[i] <- sum(diag(table(out.test, y)))/200
  }
return(per)
}

PER(logit.val)
PER(logit.val1)
PER(logit.val2)
PER(logit.val3)

logit.probs <- predict(logit.val3, val.test, type = "response")
out.test <- rep("1", 200)
out.test[logit.probs < (0.6)] <- "0"
out.test <- as.factor(out.test)
y <- val.test$Survived
table(out.test, y)

## so the best prediction accuracy of logistic regression is 81.5 percent on the validation set

#**********************************##
#* Linear Discriminant Analysis****##
#**********************************##
library(MASS)
lda.titan <- lda(Survived ~ Pclass+Sex*Parch+Age+SibSp+Fare, data=val.train)
lda.titan1 <- lda(Survived ~ Pclass+Sex*Parch+Age+SibSp+Fare+papa, data=val.train)
lda.titan2 <- lda(Survived ~ Pclass+Sex*Parch+Age+SibSp+Fare+papa+tic, data=val.train)
lda.titan3 <- lda(Survived ~ Pclass+Sex*Parch+Age+SibSp+Fare+papa+tic+Embarked, data=val.train)

## lets find the threshold that gives a good accuracy for lda
lda.per <- function(x){
lda.accu <- rep(0,5)
Lda.pred <- predict(x,val.test)
for (i in 1:5){
  pred <- rep(0, 200)
  pred[Lda.pred$posterior[,2] > i/10] <- 1
  t <- table(pred,val.test$Survived)
  lda.accu[i]<- sum(diag(t))/200
}
return(lda.accu)
}
lda.per(lda.titan)
lda.per(lda.titan1)
lda.per(lda.titan2)
lda.per(lda.titan3)
Lda.pred <- predict(lda.titan3,val.test)
pred <- rep(0, 200)
pred[Lda.pred$posterior[,2] > 0.5] <- 1
table(pred,val.test$Survived)

## lda gives a 80.5 percent predicition accuracy, the idea behind choosing lda was that 
## the age variable seems to have a normal distribution,but oh well it dint seem to do any better

#***********************##
#**** random forest ****##
#***********************##
library(randomForest)
set.seed(1)

tunedforest <- tuneRF(titanic.trainDat[,c(-1,-2,-4,-9,-11,-13,-14)],titanic.trainDat$Survived,
              ntreeTry = 500, stepFactor = 1)

tunedforest1 <- tuneRF(titanic.trainDat[,c(-1,-2,-4,-9,-11,-14)],titanic.trainDat$Survived,
                      ntreeTry = 500, stepFactor = 1)

tunedforest2 <- tuneRF(titanic.trainDat[,c(-1,-2,-4,-9,-11,-13)],titanic.trainDat$Survived,
                      ntreeTry = 500, stepFactor = 1)

tunedforest3 <- tuneRF(titanic.trainDat[,c(-1,-2,-4,-9,-11)],titanic.trainDat$Survived,
                      ntreeTry = 500, stepFactor = 1)

forest.titanic <- randomForest(Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked,data = fdata,subset = -val.t,mtry = 2, ntree = 500)
forest.titanic1 <- randomForest(Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+tic,data = fdata,subset = -val.t,mtry = 2, ntree = 500)
forest.titanic2 <- randomForest(Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa,data = fdata,subset = -val.t,mtry = 2, ntree = 500)
forest.titanic3 <- randomForest(Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa+tic,data = fdata,subset = -val.t,mtry = 3, ntree = 500)

a <- predict(forest.titanic3, val.test)
table(a, val.test$Survived)


## we get an 84.5% accuracy building a regression tree on Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa+tic with 3 predictors at every step

##*******************##
##****** svm ********##
##*******************##
library(e1071)
svm.titanic <- svm(Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked, data = val.train,kernel="radial", gamma= 1,cost= 1e5)
summary(svm.titanic)
set.seed(1)
tune.out <- tune(svm,Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked, data = val.train,kernel="radial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))

tune.out1 <- tune(svm,Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa, data = val.train,kernel="radial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
tune.out2 <- tune(svm,Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa+tic, data = val.train,kernel="radial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

sum(diag(table(val.test$Survived,predict(tune.out$best.model, val.test))))/200
sum(diag(table(val.test$Survived,predict(tune.out1$best.model, val.test))))/200
sum(diag(table(val.test$Survived,predict(tune.out2$best.model, val.test))))/200


set.seed(1)
tune.out <- tune(svm,Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked, data = val.train,kernel="polynomial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),degree=c(0.5,1,2,3,4)))
tune.out1 <- tune(svm,Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa, data = val.train,kernel="polynomial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),degree=c(0.5,1,2,3,4)))
tune.out2 <- tune(svm,Survived~ Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa+tic, data = val.train,kernel="polynomial", 
                 ranges=list(cost=c(0.1,1,10,100,1000),degree=c(0.5,1,2,3,4)))


sum(diag(table(val.test$Survived,predict(tune.out$best.model, val.test))))/200
sum(diag(table(val.test$Survived,predict(tune.out1$best.model, val.test))))/200
sum(diag(table(val.test$Survived,predict(tune.out2$best.model, val.test))))/200
table(val.test$Survived,predict(tune.out$best.model, val.test))

## prediction accuracy in 81% at best with polynomial kernel in svms on the validation set

#######################################
########## PREDICTIONS ################ 
#######################################
## the random forest showed the best prediction accuracy so lets use that for 
## the final predition
forest.titanic <- randomForest(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+papa+Embarked,data = fdata,mtry = 2, ntree = 1000)
Survived <- predict(forest.titanic, titanic.testDat)
PassengerId <- titanic.testDat$PassengerId
titanic.TestPred <- cbind(PassengerId, Survived)
titanic.TestPred[,2] <- titanic.TestPred[,2]-1
write.csv(titanic.TestPred, file = "foo1.csv", row.names = FALSE)



