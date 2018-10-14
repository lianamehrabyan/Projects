set.seed(22)
library(randomForest)
library(caret)
library(e1071)
library(ROSE)
library(lattice)
library(grid)
library(DMwR)



#dataset original
data1 <-data.frame(ACO_tosend2)
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
data1$CHADSVASC<- data1$CHADSVASC
data1$Morte.CV<- as.factor(data1$Morte.CV)
data1[is.na(data1[,19]),19]<-0

data1sd<- lapply(data1, function(x) if(is.numeric(x)){
  scale(x, center=TRUE, scale=TRUE)
} else x)
data1sd<-as.data.frame(data1sd)



#dataset sem as datas e episodios
data2=data1[,1:35]
data2<-data.frame(data2)
data2sd=data1sd[,1:35] #standardizado
data2sd<-data.frame(data2sd)


#data train and data test
set.seed(22)
idxdatatrain<-createDataPartition(y=data2$Episode.Bleeding,p=0.7,list=FALSE)
data.train=data2[idxdatatrain,]
data.train<-as.data.frame(data.train)
table(data.train[,35])
data.test=data2[-idxdatatrain,]
data.test<-as.data.frame(data.test)
table(data.test[,35])

#
set.seed(22)
idxdatatrainpca<-createDataPartition(y=datapcasd$Episode.Bleeding,p=0.7,list=FALSE)
data.trainpca=data2[idxdatatrainpca,]
data.trainpca<-as.data.frame(data.trainpca)
table(data.train[,35])
data.testpca=data2[-idxdatatrainpca,]
data.testpca<-as.data.frame(data.testpca)
table(data.testpca[,35])

####dataset oversampling####
set.seed(22)
datatraino<-ovun.sample(Episode.Bleeding~.,data=data.train, method="over")
data.traino<-data.frame(datatraino$data)
table(data.traino$Episode.Bleeding)

####dataset oversampling SMOTE####
set.seed(22)
datatrainoS<-SMOTE(Episode.Bleeding~.,data=data.train, dup_size=0)
data.trainoS<-data.frame(datatrainoS)
table(data.trainoS$Episode.Bleeding)

####dataset undersampling####
set.seed(22)
datatrainu<-ovun.sample(Episode.Bleeding~.,data=data.train, method="under")
data.trainu<-data.frame(datatrainu$data)
table(data.trainu$Episode.Bleeding)

####dataset both####
set.seed(22)
datatrainb<-ovun.sample(Episode.Bleeding~.,data=data.train, method="both")
data.trainb<-data.frame(datatrainb$data)
table(data.trainb$Episode.Bleeding)

####dataset pca over####
set.seed(22)
datatrainpcao<-ovun.sample(Episode.Bleeding~.,data=data.trainpca, method="over")
data.trainpcao<-data.frame(datatrainpcao$data)
table(data.trainpcao$Episode.Bleeding)

####dataset pca over SMOTE####
set.seed(22)
datatrainpcaoS<-SMOTE(Episode.Bleeding~.,data=data.trainpca, dup_size=0)
data.trainpcaoS<-data.frame(datatrainpcaoS)
table(data.trainpcaoS$Episode.Bleeding)

####dataset pca under####
set.seed(22)
datatrainpcau<-ovun.sample(Episode.Bleeding~.,data=data.trainpca, method="under")
data.trainpcau<-data.frame(datatrainpcau$data)
table(data.trainpcau$Episode.Bleeding)

####dataset pca both####
set.seed(22)
datatrainpcab<-ovun.sample(Episode.Bleeding~.,data=data.trainpca, method="both")
data.trainpcab<-data.frame(datatrainpcab$data)
table(data.trainpcab$Episode.Bleeding)


###Backward###

library(InformationValue)

###Original Data###
set.seed(22)
mv <- glm(data.traino$Episode.Bleeding~1,data=data.traino, family=binomial(link="logit"))
mo <-  glm(data.traino$Episode.Bleeding ~ data.traino$Vascular+data.traino$Stroke+ data.traino$Hypertension + 
             data.traino$Heart.Failure + data.traino$mais.igual.75 + data.traino$Age + data.traino$Female + 
             data.traino$X..of.Tests.in.Range + data.traino$Total.Number.of.Tests + data.traino$Days.3.5 + 
             data.traino$Days.1.8 + data.traino$Days.3 + data.traino$Days..2 + data.traino$X..days..3 + 
             data.traino$ttr.75. + data.traino$TTR.60. + data.traino$X..Days.Within.Range + 
             data.traino$Total.Days + data.traino$CHADSVASC, data = data.traino, family = binomial(link = "logit"))
step(mo, dir="forward", test="Chisq", data=data.traino)


step(mv,scope = list(upper=mo),
          direction="both",test="Chisq",data=data.traino)

mof <- glm(data.test$Episode.Bleeding ~ data.test$Days.3 +
           data.test$Age + data.test$Vascular + data.test$Stroke +
             data.test$Days.Within.Range,
           data = data.test, family = binomial(link = "logit"))



moft<-predict(mof,data.test, type="response")

optCutOff <- optimalCutoff(data.test$Episode.Bleeding, moft, optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, moft, threshold = optCutOff)
plotROC(data.test$Episode.Bleeding, moft)
youdensIndex(data.test$Episode.Bleeding, moft, threshold = optCutOff)
bmoft<-ifelse(moft>optCutOff,1,0)
confusionMatrix(data.test$Episode.Bleeding, moft, threshold = optCutOff)
sensitivity(data.test$Episode.Bleeding, moft, threshold = optCutOff)
specificity(data.test$Episode.Bleeding, moft, threshold = optCutOff)
accuracy.meas(data.test$Episode.Bleeding, moft, threshold = optCutOff)
#On the result of oversampling stepwise





#######Original Oversampling######
set.seed(22)
mov <-  glm(data.traino$Episode.Bleeding ~ data.traino$Vascular+data.traino$Stroke+ 
              data.traino$Hypertension + data.traino$Heart.Failure + data.traino$mais.igual.75 + 
              data.traino$Age + data.traino$Female + data.traino$X..of.Tests.in.Range + 
              data.traino$Total.Number.of.Tests + data.traino$Days.3.5 + data.traino$Days.1.8 + 
              data.traino$Days.3 + data.traino$Days..2 + data.traino$X..days..3 + 
              data.traino$ttr.75. + data.traino$TTR.60. + data.traino$X..Days.Within.Range + 
              data.traino$Total.Days + data.traino$CHADSVASC, data = data.train, 
            family = binomial(link = "logit"))
step(mov, dir="backward", test="Chisq", data=data.traino)
movf <-glm(data.test$Episode.Bleeding ~ data.test$Vascular + 
             data.test$Stroke + data.test$Heart.Failure + data.test$Age + 
             data.test$X..of.Tests.in.Range + data.test$Total.Number.of.Tests + 
             data.test$Days.1.8 + data.test$Days.3 + data.test$TTR.60. + 
             data.test$Total.Days + data.test$CHADSVASC, family = binomial(link = "logit"), 
           data = data.test)

movft<-predict(movf,data.test, type = "response")

optCutOffov <- optimalCutoff(data.test$Episode.Bleeding, movft,optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, movft, threshold = optCutOffov)
plotROC(data.test$Episode.Bleeding, movft)
bmovft<-ifelse(movft>optCutOffov,1,0)
confusionMatrix(data.test$Episode.Bleeding, movft, threshold = optCutOffov)
sensitivity(data.test$Episode.Bleeding, movft, threshold = optCutOffov)
specificity(data.test$Episode.Bleeding, movft, threshold = optCutOffov)
accuracy.meas(data.test$Episode.Bleeding, movft, threshold = optCutOffov)
#######Original Oversampling SMOTE######
set.seed(22)
movs <-  glm(data.trainoS$Episode.Bleeding ~ data.trainoS$Vascular+data.trainoS$Stroke+ 
               data.trainoS$Hypertension + data.trainoS$Heart.Failure + data.trainoS$mais.igual.75 + 
               data.trainoS$Age + data.trainoS$Female + data.trainoS$X..of.Tests.in.Range + 
               data.trainoS$Total.Number.of.Tests + data.trainoS$Days.3.5 + data.trainoS$Days.1.8 + 
               data.trainoS$Days.3 + data.trainoS$Days..2 + data.trainoS$X..days..3 + 
               data.trainoS$ttr.75. + data.trainoS$TTR.60. + data.trainoS$X..Days.Within.Range + 
               data.trainoS$Total.Days + data.trainoS$CHADSVASC, data = data.trainoS, 
             family = binomial(link = "logit"))
step(movs, dir="backward", test="Chisq", data=data.trainoS)
movsf <- glm(data.test$Episode.Bleeding ~ data.test$Vascular + 
               data.test$Stroke + data.test$Heart.Failure + data.test$Age + 
               data.test$X..Days.Within.Range + data.test$Total.Number.of.Tests, 
             family = binomial(link = "logit"), data = data.test)

movsft<-predict(movsf,data.test, type = "response")

optCutOffovs <- optimalCutoff(data.test$Episode.Bleeding, movsft,optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, movsft, threshold = optCutOffovs)
plotROC(data.test$Episode.Bleeding, movsft)
bmovsft<-ifelse(movsft>optCutOffovs,1,0)
confusionMatrix(data.test$Episode.Bleeding, movsft, threshold = optCutOffovs)
sensitivity(data.test$Episode.Bleeding, movsft, threshold = optCutOffovs)
specificity(data.test$Episode.Bleeding, movsft, threshold = optCutOffovs)
accuracy.meas(data.test$Episode.Bleeding, movsft, threshold = optCutOffovs)

#######Original Undersampling######
set.seed(22)
mou <-  glm(data.trainu$Episode.Bleeding ~ data.trainu$Vascular+data.trainu$Stroke+ 
              data.trainu$Hypertension + data.trainu$Heart.Failure + data.trainu$mais.igual.75 + 
              data.trainu$Age + data.trainu$Female + data.trainu$X..of.Tests.in.Range + 
              data.trainu$Total.Number.of.Tests + data.trainu$Days.3.5 + data.trainu$Days.1.8 + 
              data.trainu$Days.3 + data.trainu$Days..2 + data.trainu$X..days..3 + 
              data.trainu$ttr.75. + data.trainu$TTR.60. + data.trainu$X..Days.Within.Range + 
              data.trainu$Total.Days + data.trainu$CHADSVASC, data = data.trainu, 
            family = binomial(link = "logit"))
step(mou, dir="backward", test="Chisq", data=data.trainu)
mouf <- glm(data.test$Episode.Bleeding ~  data.test$Total.Number.of.Tests + 
              data.test$Days.1.8 + data.test$Days..2 + data.test$X..days..3 + 
              data.test$Total.Days + data.test$CHADSVASC, family = binomial(link = "logit"), 
            data = data.test)

mouft<-predict(mouf,data.test, type = "response")

optCutOffou <- optimalCutoff(data.test$Episode.Bleeding, mouft,optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, mouft, threshold = optCutOff)
plotROC(data.test$Episode.Bleeding, mouft)
bmouft<-ifelse(mouft>optCutOffou,1,0)
confusionMatrix(data.test$Episode.Bleeding, mouft, threshold = optCutOffou)
sensitivity(data.test$Episode.Bleeding, mouft, threshold = optCutOffou)
specificity(data.test$Episode.Bleeding, mouft, threshold = optCutOffou)
youdensIndex(data.test$Episode.Bleeding, mouft, threshold = optCutOffou)


mlast<-glm(data.test$Episode.Bleeding~data.test$Days.3+data.test$X..of.Tests.in.Range+
             data.test$Total.Number.of.Tests+data.test$Vascular+
             data.test$Age+data.test$Heart.Failure+data.test$Stroke,
           family = binomial(link = "logit"), 
           data = data.test)

mlastf<-predict(mlast,data.test,type="response")
optCutOfflastf <- optimalCutoff(data.test$Episode.Bleeding, mlastf,optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, mlastf, threshold = optCutOfflastf)
plotROC(data.test$Episode.Bleeding, mlastf)
bmovsft<-ifelse(mlastf>optCutOffovs,1,0)
confusionMatrix(data.test$Episode.Bleeding, mlastf, threshold = optCutOfflastf)
sensitivity(data.test$Episode.Bleeding, mlastf, threshold = optCutOfflastf)
specificity(data.test$Episode.Bleeding, mlastf, threshold = optCutOfflastf)
accuracy.meas(data.test$Episode.Bleeding, mlastf, threshold = optCutOfflastf)



#Test on remaining variables after univariate and coefficient differences 

co<-glm(data.test$Episode.Bleeding~
          
          
          data.test$Days.3.5+data.test$TTR.60.+
          data.test$CHADSVASC +
          data.test$Total.Days*data.test$Total.Number.of.Tests
          ,
        family=binomial(link="logit"),data=data.traino)
mo<-predict(co,data.test, type = "response")

summary(co)
optCutOffco <- optimalCutoff(data.test$Episode.Bleeding, mo,optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, mo, threshold = optCutOffco)
plotROC(data.test$Episode.Bleeding, mo)
bmo<-ifelse(mo>optCutOffou,1,0)
confusionMatrix(data.test$Episode.Bleeding, mo, threshold = optCutOffco)
sensitivity(data.test$Episode.Bleeding, mo, threshold = optCutOffco)
specificity(data.test$Episode.Bleeding, mo, threshold = optCutOffco)
youdensIndex(data.test$Episode.Bleeding, mo, threshold = optCutOffco)

co1<-glm(data1$Episode.Bleeding~data1$Vascular+data1$Stroke+
          data1$Heart.Failure+data1$Age+data1$Female+
          data1$Total.Number.of.Tests+data1$Days.3.5+data1$Days.1.8+
          data1$Days.3+data1$Days..2+data1$TTR.60.+data1$Total.Days,
        family=binomial(link="logit"),data=data1)
summary(co1)

# Test on remaining variables after univariate and differenciation 
# of factor's values and p-values

co11<-glm(data.test$Episode.Bleeding~data.test$CHADSVASC +data.test$Total.Days+
            data.test$X..Days.Within.Range+ data.test$X..of.Tests..3.5,
              family=binomial(link="logit"),data=data.test)
mo11<-predict(co11,data.test, type = "response")
summary(co11)

optCutOffco11 <- optimalCutoff(data.test$Episode.Bleeding, mo11,optimiseFor = "Both")[1]
misClassError(data.test$Episode.Bleeding, mo11, threshold = optCutOffco11)
plotROC(data.test$Episode.Bleeding, mo11)
bmo11<-ifelse(mo11>optCutOffou,1,0)
confusionMatrix(data.test$Episode.Bleeding, mo11, threshold = optCutOffco11)
sensitivity(data.test$Episode.Bleeding, mo11, threshold = optCutOffco11)
specificity(data.test$Episode.Bleeding, mo11, threshold = optCutOffco11)
youdensIndex(data.test$Episode.Bleeding, mo11, threshold = optCutOffco11)


co11Data<-glm(data2$Episode.Bleeding~data1$CHADSVASC+data2$Days.3.5+
                data2$Days.3+data2$Total.Number.of.Tests+
                data2$TTR.60.+data2$Total.Days,
              family=binomial(link="logit"),data=data2)

maup<-predict(co11Data,data=data2,type="response")
#Check linearity

library(tidyverse)
library(broom)

# Select only numeric predictors
mydata <- data2 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(maup/(1-maup))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")



#Look for interaction terms

integ1<-glm(data.test$Episode.Bleeding~
          
          data.test$Days.3.5+data.test$Days.3+
          data.test$Days.3.5 *data.test$Days.3 ,
        family=binomial(link="logit"),data=data.test)
summary(integ1)




