library(caret)
library(class)
library(CORElearn)
library(ROSE)
library(corrplot)
library(dplyr)
library(MASS)
library(smotefamily)
library(ecodist)
library(analogue)
library(hmeasure)
library(arules)
data1<-read.csv("ACO_tosend2.csv")
data1 <-data.frame(data1)
data1$TTR.60.<- as.factor(data1$TTR.60.)
data1$ttr.75.<- as.factor(data1$ttr.75.)
data1$Female<- as.factor(data1$Female)
data1$mais.igual.75<- as.factor(data1$mais.igual.75)
data1$Heart.Failure<- as.factor(data1$Heart.Failure)
data1$Hypertension<- as.factor(data1$Hypertension)
data1$Diabetes<- as.factor(data1$Diabetes)
data1$Stroke<- as.factor(data1$Stroke)
data1$Vascular<- as.factor(data1$Vascular)
data1$Episode.Bleeding<- as.factor(data1$Episode.Bleeding)
data1$Episode.MI<- as.factor(data1$Episode.MI)
data1$Episode.Stroke<- as.factor(data1$Episode.Stroke)
data1$Episode.Death<- as.factor(data1$Episode.Death)
data1$Diagnosticos.agrupados<- as.factor(data1$Diagnosticos.agrupados)
data1$CHADSVASC<- as.factor(data1$CHADSVASC)
data1$Morte.CV<- as.factor(data1$Morte.CV)
data1[is.na(data1[,19]),19]<-0
data2=data1[,1:35]
set.seed(22)
idxdatatrain<-createDataPartition(data2$Episode.Bleeding,p=0.7,list=FALSE)

#original
data.train=data2[idxdatatrain,]
data.test=data2[-idxdatatrain,]

########################################
#####    Necessary data            #####
########################################

data.train_knn<-data.train[,c(2,18,27,28,33:35)]


data.test_knn<-data.test[,c(2,18,27,28,33:35)]


data.train_knn$mais.igual.75<-ifelse(data.train_knn$mais.igual.75==1,1,0)
data.train_knn$Vascular<-ifelse(data.train_knn$Vascular==1,1,0)
data.train_knn$CHADSVASC<-as.numeric(data.train_knn$CHADSVASC)
data.train_knn$CHADSVASC<-data.train_knn$CHADSVASC-1
data.test_knn$mais.igual.75<-ifelse(data.test_knn$mais.igual.75==1,1,0)
data.test_knn$Vascular<-ifelse(data.test_knn$Vascular==1,1,0)
data.test_knn$CHADSVASC<-as.numeric(data.test_knn$CHADSVASC)
data.test_knn$CHADSVASC<-data.test_knn$CHADSVASC-1

scaled_train<-as.data.frame(scale(data.train_knn[,-7],center=T, scale=T))
scaled_train$Episode.Bleeding<-data.train_knn$Episode.Bleeding
scaled_test<-as.data.frame(scale(data.test_knn[,-7],center=T, scale=T))
scaled_test$Episode.Bleeding<-data.test_knn$Episode.Bleeding

set.seed(22)
data.trainSMOTE<-SMOTE(data.train_knn[,-7],data.train_knn$Episode.Bleeding, dup_size=0)$data
data.trainSMOTE$CHADSVASC<-as.integer(data.trainSMOTE$CHADSVASC)
data.trainSMOTE$mais.igual.75<-ifelse(data.trainSMOTE$mais.igual.75>0,1,0)
data.trainSMOTE$Vascular<-ifelse(data.trainSMOTE$Vascular>0,1,0)
data.trainSMOTE$Total.Days<-as.integer(data.trainSMOTE$Total.Days)
data.trainSMOTE$Total.Number.of.Tests<-as.integer(data.trainSMOTE$Total.Number.of.Tests)
data.trainSMOTE$Age<-as.integer(data.trainSMOTE$Age)
scaled_train_smote<-as.data.frame(scale(data.trainSMOTE[,-7],center=T, scale=T))
scaled_train_smote$Episode.Bleeding<-data.trainSMOTE$class

########################################
#####             EDA              #####
########################################
ggplot(data1, aes(Episode.Bleeding,Total.Number.of.Tests))+geom_boxplot(color="skyblue")
ggplot(data1, aes(Episode.Bleeding,Age))+geom_boxplot(color="skyblue")
ggplot(data1, aes(Episode.Bleeding,Total.Days))+geom_boxplot(color="skyblue")



tb1<-table(bio$Episode.Bleeding,bio$ttr.75.)
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$TTR.60.)
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$Heart.Failure)
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$mais.igual.75) #dep
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$Hypertension)
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$Diabetes)
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$Stroke)
chisq.test(tb1)
tb1<-table(bio$Episode.Bleeding,bio$Vascular)  #dep
chisq.test(tb1) 
tb1<-table(bio$Episode.Bleeding,bio$CHADSVASC)  #dep
chisq.test(tb1) 

########################################
#####     Knn for original data    #####
########################################
set.seed(22) 
ct<-trainControl(method="cv", number=5)
knn_c<-train(Episode.Bleeding~.,data=data.train_knn, method="knn",trControl=ct,preProcess=c("center","scale"),tuneLength=10)
plot(knn_c)
predict<-knn(scaled_train,scaled_test,
             scaled_train$Episode.Bleeding,k=5,prob=TRUE)

confusionMatrix(predict,scaled_test$Episode.Bleeding,positive="1")
scores.knn <- attr(predict,"prob")
scores.knn[predict==0] <- 1-scores.knn[predict==0]


results <- HMeasure(data.test_knn$Episode.Bleeding,scores.knn)
results
plotROC(results)



########################################
#####     Knn for SMOTE data       #####
########################################

ct<-trainControl(method="cv", number=5)
knn_c<-train(class~.,data=data.trainSMOTE, method="knn",trControl=ct,preProcess=c("center","scale"),tuneLength=10)
plot(knn_c)

predict<-knn(scaled_train_smote,scaled_test,
             scaled_train_smote$Episode.Bleeding,k=5,prob=TRUE)

confusionMatrix(predict,data.test_knn$Episode.Bleeding,positive = "1")

scores.knn <- attr(predict,"prob")
scores.knn[predict==0] <- 1-scores.knn[predict==0]

results <- HMeasure(data.test_knn$Episode.Bleeding,scores.knn)
results
plotROC(results)




########################################
#####   Association Rules Mining   #####
########################################
alm<-data1[,c(4,5,26,28:37)]
alm<-alm[,-c(10,12,13)]
tData <- as (alm, "transactions")
rules <- apriori (data=alm, parameter=list (supp=0.01,conf = 0.08), appearance = list (default="lhs",rhs="Episode.Bleeding=1"), control = list (verbose=F))
inspect(head(rules))
rules_lift<-sort(rules,by="lift",decreasing = TRUE)
inspect(head(rules_lift,n=10))
