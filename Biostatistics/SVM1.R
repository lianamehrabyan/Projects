######Melhores datasets:
###Por Youden:
#Winner: PCA Both Undersampling & Oversampling: 4 PCs + CHADSVASC ->0.5364
#Runner-up: Original Standardizado Oversampling with SMOTE: 6 Variáveis->0.47893
#Bronze: PCA Both, Over com e sem Smote: Todas as variáveis ->0.45594

###Por Accuracy:
#Winner: Original Both: Todas as variáveis ->0.7708
#Runner-up: PCA Both Undersampling & Oversampling: 4 PCs + CHADSVASC ->0.7604
#Bronze: PCA Oversampling SMOTE: 4PCs +  CHADSVASC ->0.7188





grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
ct<-trainControl(method="cv", number=6)


####################Original###########################
set.seed(22)
svm<-train(Episode.Bleeding~.,data=data.trainsd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm
plot(svm)
testsvm=predict(svm,data.testsd)
confusionMatrix(testsvm,data.testsd[,35],positive = "1")
#Accuracy : 0.9062  Sensitivity : 0.0  Specificity : 1.0  Youden's : 
#PPV:NaN NPV:0.90625
testsvm=as.numeric(testsvm)
roc_obj=roc(response=data.testsd[,35],predictor=testsvm)
auc(roc_obj)
#AUC=0.5

###############Original Standardizado both Undersampling & Oversampling #################

set.seed(22)
svm_b<-train(Episode.Bleeding~.,data=data.trainbsd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_b
plot(svm_b)
testsvm_b=predict(svm_b,data.testsd)
confusionMatrix(testsvm_b,data.testsd[,35],positive = "1")
performance(testsvm_b,"auc")@y.values
#Accuracy : 0.7708  Sensitivity : 0.66667  Specificity : 0.78161  Youden's : 0.44828
#PPV:0.24000 NPV:0.95775

testsvm_b=as.numeric(testsvm_b)
roc_objb=roc(response=data.testsd[,35],predictor=testsvm_b)
auc(roc_objb)
#AUC=0.7241
###############Original Standardizado Oversampling #################

set.seed(22)
svm_o<-train(Episode.Bleeding~.,data=data.trainosd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_o
plot(svm_o)
testsvm_o=predict(svm_o,data.testsd)
confusionMatrix(testsvm_o,data.testsd[,35],positive = "1")
#Accuracy : 0.7083  Sensitivity : 0.33333  Specificity : 0.74713  Youden's : 0.08046
#PPV:0.12000 NPV:0.91549
testsvm_o=as.numeric(testsvm_o)
roc_objo=roc(response=data.testsd[,35],predictor=testsvm_o)
auc(roc_objo)
#AUC=0.5402

###############Original Standardizado Oversampling with SMOTE #################
set.seed(22)
svm_oS<-train(Episode.Bleeding~.,data=data.trainoSsd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_oS
plot(svm_oS)
testsvm_oS=predict(svm_oS,data.testsd)
confusionMatrix(testsvm_oS,data.testsd[,35],positive = "1")
#Accuracy : 0.6771  Sensitivity : 0.66667  Specificity : 0.67816  Youden's : 0.34483
#PPV:0.17647 NPV:0.95161
testsvm_oS=as.numeric(testsvm_oS)
roc_objoS=roc(response=data.testsd[,35],predictor=testsvm_oS)
auc(roc_objoS)
#AUC=0.6724



###############Original Standardizado Undersampling #################
set.seed(22)
svm_u<-train(Episode.Bleeding~.,data=data.trainusd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_u
plot(svm_u)
testsvm_u=predict(svm_u,data.testsd)
confusionMatrix(testsvm_u,data.testsd[,35],positive = "1")
#Accuracy : 0.6042  Sensitivity : 0.77778  Specificity : 0.58621  Youden's : 0.36399
#PPV:0.16279 NPV:0.96226
testsvm_u=as.numeric(testsvm_u)
roc_obju=roc(response=data.testsd[,35],predictor=testsvm_u)
auc(roc_obju)
#AUC=0.682


###################PCA original###################

set.seed(22)
svm_pca<-train(Episode.Bleeding~.,data=data.trainpca, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pca
plot(svm_pca)
testsvm_pca=predict(svm_pca,data.testpca)
confusionMatrix(testsvm_pca,data.testpca[,16],positive = "1")
#Accuracy : 0.9062  Sensitivity : 0.0  Specificity : 1.0  Youden's : 
#PPV:NaN NPV:0.90625
testsvm_pca=as.numeric(testsvm_pca)
roc_objpca=roc(response=data.testpca[,16],predictor=testsvm_pca)
auc(roc_objpca)
#AUC=0.5


###############PCA Both Undersampling & Oversampling #################
set.seed(22)
svm_pcab<-train(Episode.Bleeding~.,data=data.trainpcab, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcab
plot(svm_pcab)
testsvm_pcab=predict(svm_pcab,data.testpca)
confusionMatrix(testsvm_pcab,data.testpca[,16],positive = "1")
#Accuracy : 0.6875  Sensitivity : 0.77778  Specificity : 0.67816  Youden's : 0.45594
#PPV:0.20000 NPV:0.96721
testsvm_pcab=as.numeric(testsvm_pcab)
roc_objpcab=roc(response=data.testpca[,16],predictor=testsvm_pcab)
auc(roc_objpcab)
#AUC=0.728


###############PCA Oversampling #################
set.seed(22)
svm_pcao<-train(Episode.Bleeding~.,data=data.trainpcao, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcao
plot(svm_pcao)
testsvm_pcao=predict(svm_pcao,data.testpca)
confusionMatrix(testsvm_pcao,data.testpca[,16],positive = "1")
#Accuracy : 0.6875  Sensitivity : 0.77778  Specificity : 0.67816  Youden's : 0.45594
#PPV:0.20000 NPV:0.96721
testsvm_pcao=as.numeric(testsvm_pcao)
roc_objpcao=roc(response=data.testpca[,16],predictor=testsvm_pcao)
auc(roc_objpcao)
#AUC=0.728

###############PCA Oversampling SMOTE#################
set.seed(22)
svm_pcaoS<-train(Episode.Bleeding~.,data=data.trainpcaoS, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcaoS
plot(svm_pcaoS)
testsvm_pcaoS=predict(svm_pcaoS,data.testpca)
confusionMatrix(testsvm_pcaoS,data.testpca[,16],positive = "1")
#Accuracy : 0.6875  Sensitivity : 0.77778  Specificity : 0.67816  Youden's : 0.45594
#PPV:0.20000 NPV:0.96721
testsvm_pcaoS=as.numeric(testsvm_pcaoS)
roc_objpcaoS=roc(response=data.testpca[,16],predictor=testsvm_pcaoS)
auc(roc_objpcaoS)
#AUC=0.728

###############PCA Undersampling #################
set.seed(22)
svm_pcau<-train(Episode.Bleeding~.,data=data.trainpcau, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcau
plot(svm_pcau)
testsvm_pcau=predict(svm_pcau,data.testpca)
confusionMatrix(testsvm_pcau,data.testpca[,16],positive = "1")
#Accuracy : 0.5625  Sensitivity : 0.77778  Specificity : 0.54023  Youden's : 0.31801
#PPV:0.14894 NPV:0.95918
testsvm_pcau=as.numeric(testsvm_pcau)
roc_objpcau=roc(response=data.testpca[,16],predictor=testsvm_pcau)
auc(roc_objpcau)
#AUC=0.659

###############################################################################
##################Analysys of the models with less Variables#######################
###############################################################################

############################# PCA ############################

###4 Componentes Principais e CHADSVASC###

###################PCA original###################
set.seed(22)
svm_pcat<-train(Episode.Bleeding ~ Comp.1+Comp.2+Comp.3+Comp.4+CHADSVASC,data=data.trainpca, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcat
plot(svm_pcat)
testsvm_pcat=predict(svm_pcat,data.testpca)
confusionMatrix(testsvm_pcat,data.testpca[,16],positive = "1")
#Accuracy : 0.9062  Sensitivity : 0.0  Specificity : 1.0  Youden's : 
#PPV:NaN NPV:0.90625
testsvm_pcau=as.numeric(testsvm_pcau)
roc_objpcau=roc(response=data.testpca[,16],predictor=testsvm_pcau)
auc(roc_objpcau)
#AUC=0.659

###############PCA Both Undersampling & Oversampling #################
set.seed(22)
svm_pcabt<-train(Episode.Bleeding ~ Comp.1+Comp.2+Comp.3+Comp.4+CHADSVASC,data=data.trainpcab, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcabt
plot(svm_pcabt)
testsvm_pcabt=predict(svm_pcabt,data.testpca)
confusionMatrix(testsvm_pcabt,data.testpca[,16],positive = "1")
#Accuracy : 0.7604  Sensitivity : 0.77778  Specificity : 0.75862  Youden's : 0.5364
#PPV:0.25000 NPV:0.97059
testsvm_pcabt=as.numeric(testsvm_pcabt)
roc_objpcabt=roc(response=data.testpca[,16],predictor=testsvm_pcabt)
auc(roc_objpcabt)
#AUC=0.7682

###############PCA Oversampling #################
set.seed(22)
svm_pcaot<-train(Episode.Bleeding ~ Comp.1+Comp.2+Comp.3+Comp.4+CHADSVASC,data=data.trainpcao, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcaot
plot(svm_pcaot)
testsvm_pcaot=predict(svm_pcaot,data.testpca)
confusionMatrix(testsvm_pcaot,data.testpca[,16],positive = "1")
#Accuracy : 0.6771  Sensitivity : 0.66667  Specificity : 0.67816  Youden's : 0.34483
#PPV:0.17647 NPV:0.95161
testsvm_pcaot=as.numeric(testsvm_pcaot)
roc_objpcaot=roc(response=data.testpca[,16],predictor=testsvm_pcaot)
auc(roc_objpcaot)
#AUC=0.6724


###############PCA Oversampling SMOTE#################
set.seed(22)
svm_pcaoSt<-train(Episode.Bleeding ~ Comp.1+Comp.2+Comp.3+Comp.4+CHADSVASC,data=data.trainpcaoS, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcaoSt
plot(svm_pcaoSt)
testsvm_pcaoSt=predict(svm_pcaoSt,data.testpca)
confusionMatrix(testsvm_pcaoSt,data.testpca[,16],positive = "1")
#Accuracy : 0.7188  Sensitivity : 0.66667  Specificity : 0.72414  Youden's : 0.39081
#PPV:0.20000 NPV:0.95455
testsvm_pcaoSt=as.numeric(testsvm_pcaoSt)
roc_objpcaoSt=roc(response=data.testpca[,16],predictor=testsvm_pcaoSt)
auc(roc_objpcaoSt)
#AUC=0.6954

###############PCA Undersampling #################
set.seed(22)
svm_pcaut<-train(Episode.Bleeding ~ Comp.1+Comp.2+Comp.3+Comp.4+CHADSVASC,data=data.trainpcau, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_pcaut
plot(svm_pcaut)
testsvm_pcaut=predict(svm_pcaut,data.testpca)
confusionMatrix(testsvm_pcaut,data.testpca[,16],positive = "1")
#Accuracy : 0.5938  Sensitivity : 0.77778  Specificity : 0.57471  Youden's : 0.35249
#PPV:0.15909 NPV:0.96154
testsvm_pcaut=as.numeric(testsvm_pcaut)
roc_objpcaut=roc(response=data.testpca[,16],predictor=testsvm_pcaut)
auc(roc_objpcaut)
#AUC=0.6762


####################Original###########################
#Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular + CHADSVASC

####################Original###########################

set.seed(22)
svmt<-train(Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular + CHADSVASC,data=data.trainsd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svmt
plot(svmt)
testsvmt=predict(svmt,data.testsd)
confusionMatrix(testsvmt,data.testsd[,35],positive = "1")
#Accuracy : 0.9062  Sensitivity : 0.0  Specificity : 1.0  Youden's : 
#PPV:NaN NPV:0.90625
testsvmt=as.numeric(testsvmt)
roc_objt=roc(response=data.testsd[,35],predictor=testsvmt)
auc(roc_objt)
#AUC=0.5

###############Original Standardizado both Undersampling & Oversampling #################

set.seed(22)
svm_bt<-train(Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular + CHADSVASC,data=data.trainbsd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_bt
plot(svm_bt)
testsvm_bt=predict(svm_bt,data.testsd)
confusionMatrix(testsvm_bt,data.testsd[,35],positive = "1")
#Accuracy : 0.6667  Sensitivity : 0.55556  Specificity : 0.67816  Youden's : 0.23372
#PPV:0.15152 NPV:0.93651
testsvm_bt=as.numeric(testsvm_bt)
roc_objbt=roc(response=data.testsd[,35],predictor=testsvm_bt)
auc(roc_objbt)
#AUC=0.6169


###############Original Standardizado Oversampling #################

set.seed(22)
svm_ot<-train(Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular + CHADSVASC,data=data.trainosd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_ot
plot(svm_ot)
testsvm_ot=predict(svm_ot,data.testsd)
confusionMatrix(testsvm_ot,data.testsd[,35],positive = "1")
#Accuracy : 0.6562  Sensitivity : 0.66667  Specificity : 0.65517  Youden's : 0.32184
#PPV:0.12000 NPV:0.91549
testsvm_ot=as.numeric(testsvm_ot)
roc_objot=roc(response=data.testsd[,35],predictor=testsvm_ot)
auc(roc_objot)
#AUC=0.6609

###############Original Standardizado Oversampling with SMOTE #################
set.seed(22)
svm_oSt<-train(Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular +  CHADSVASC  ,data=data.trainoSsd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_oSt
plot(svm_oSt)
testsvm_oSt=predict(svm_oSt,data.testsd)
confusionMatrix(testsvm_oSt,data.testsd[,35],positive = "1")
#Accuracy : 0.7083  Sensitivity : 0.77778  Specificity : 0.70115  Youden's : 0.47893
#PPV:0.21212 NPV:0.96825
testsvm_oSt=as.numeric(testsvm_oSt)
roc_objoSt=roc(response=data.testsd[,35],predictor=testsvm_oSt)
auc(roc_objoSt)
#AUC=0.7395

###############Original Standardizado Undersampling #################
set.seed(22)
svm_ut<-train(Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular + CHADSVASC,data=data.trainusd, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_ut
plot(svm_ut)
testsvm_ut=predict(svm_ut,data.testsd)
confusionMatrix(testsvm_ut,data.testsd[,35],positive = "1")
#Accuracy : 0.5417  Sensitivity : 0.77778  Specificity : 0.51724  Youden's : 0.29502
#PPV:0.14286 NPV:0.95745
testsvm_ut=as.numeric(testsvm_ut)
roc_objut=roc(response=data.testsd[,35],predictor=testsvm_ut)
auc(roc_objut)
#AUC=0.6475







###############Original Standardizado Oversampling with SMOTE #################
set.seed(22)
svm_oS3t<-train(Episode.Bleeding ~ Total.Days + Days..2 + Days.3 + Age + Vascular +  CHADSVASC +Number.Tests.3 ,data=data.trainoSsd3, method="svmLinear",tuneGrid = grid,trControl=ct,preProcess=c("center","scale"),tuneLength=10)
svm_oS3t
plot(svm_oS3t)
testsvm_oS3t=predict(svm_oS3t,data.test3sd)
confusionMatrix(testsvm_oS3t,data.test3sd[,35],positive = "1")
#Accuracy : 0.7083  Sensitivity : 0.77778  Specificity : 0.70115  Youden's : 0.47893
#PPV:0.21212 NPV:0.96825

