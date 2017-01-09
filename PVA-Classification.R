#importing the data
library(readr)
pva=read_csv("C:/Users/sethi/Desktop/pva.csv")

#viewing the summary of the data
view(pva)
summary(pva)
dim(pva)
str(pva)
attach(pva)

#making a copy of the data
pva1=pva
attach(pva1)

#performing data cleaning 

#PVASTATE-if NA, then 0, otherwise 1
pva1$PVASTATE[! is.na(pva1$PVASTATE)]=1
pva1$PVASTATE[is.na(pva1$PVASTATE)]=0
unique(pva1$PVASTATE)

pva2=pva1

#RECINHSE-if NA, then 0, otherwise 1
pva1$RECINHSE[! is.na(pva1$RECINHSE)]=1
pva1$RECINHSE[is.na(pva1$RECINHSE)]=0
unique(pva1$RECINHSE)

#RECP3-if NA, then 0, otherwise 1
pva1$RECP3[! is.na(pva1$RECP3)]=1
pva1$RECP3[is.na(pva1$RECP3)]=0
unique(pva1$RECP3)

#RECPGVG-if NA, then 0, otherwise 1
pva1$RECPGVG[! is.na(pva1$RECPGVG)]=1
pva1$RECPGVG[is.na(pva1$RECPGVG)]=0
unique(pva1$RECPGVG)

#RESWEEP-if NA, then 0, otherwise 1
pva1$RECSWEEP[! is.na(pva1$RECSWEEP)]=1
pva1$RECSWEEP[is.na(pva1$RECSWEEP)]=0
unique(pva1$RECSWEEP)

#PEPSTRFL-if NA, then 0, otherwise 1
pva1$PEPSTRFL[! is.na(pva1$PEPSTRFL)]=1
pva1$PEPSTRFL[is.na(pva1$PEPSTRFL)]=0
unique(pva1$PEPSTRFL)

#MAJOR-if NA, then 0, otherwise 1
pva1$MAJOR[! is.na(pva1$MAJOR)]=1
pva1$MAJOR[is.na(pva1$MAJOR)]=0
unique(pva1$MAJOR)

#HOMEOWNR-if NA, then 0, otherwise 1
pva1$HOMEOWNR[is.na(pva1$HOMEOWNR)]=0
pva1$HOMEOWNR=ifelse(pva1$HOMEOWNR=="H",1,0)
unique(pva1$HOMEOWNR)

#GENDER-if (NA,A or J) then U, otherwise M or F
pva1$GENDER[is.na(pva1$GENDER)]="U"
pva1$GENDER=ifelse(pva1$GENDER=="A","U",pva1$GENDER)
pva1$GENDER=ifelse(pva1$GENDER=="J","U",pva1$GENDER)
unique(pva1$GENDER)

#CHILD03, CHILD07, CHILD12, CHILD18-if NA, then 0, otherwise 1
pva1$CHILD03=ifelse(is.na(pva1$CHILD03),0,1)
pva1$CHILD07=ifelse(is.na(pva1$CHILD07),0,1)
pva1$CHILD12=ifelse(is.na(pva1$CHILD12),0,1)
pva1$CHILD18=ifelse(is.na(pva1$CHILD18),0,1)
unique(pva1$CHILD03)
unique(pva1$CHILD07)
unique(pva1$CHILD12)
unique(pva1$CHILD18)

pva3=pva1

#NUMCHLD-if NA, then replace it with the mode, i.e. 1
table(pva1$NUMCHLD)
pva1$NUMCHLD[is.na(pva1$NUMCHLD)]=1
unique(pva1$NUMCHLD)

#DOMAIN is split into 2 variables-URBANICITY & SESTATUS
#URBANICITY-first char of DOMAIN
#if NA, then replace it with the mode, i.e. S
table(pva1$URBANICITY)
pva1$URBANICITY=substr(pva1$DOMAIN,1,1)
pva1$URBANICITY[is.na(pva1$URBANICITY)]="S"
unique(pva2$URBANICITY)

#SESTATUS-#URBANICITY-second char of DOMAIN
#if NA, then replace it with the mode, i.e. 2
table(pva1$SESTATUS)
pva1$SESTATUS=substr(pva1$DOMAIN,2,2)
pva1$SESTATUS[is.na(pva1$SESTATUS)]=2
unique(pva1$SESTATUS)

#removing the original variable-DOMAIN
pva1=subset(pva1,select=c(-DOMAIN))

#INCOME-if NA, then replace it with the mode, i.e. 5
table(pva1$INCOME)
pva1$INCOME[is.na(pva1$INCOME)]=5
unique(pva1$INCOME)

#HPHONE_D
unique(pva1$HPHONE_D)

#WEALTH1 & WEALTH2
#if NA, then replace it with the mode, i.e. 9 for both variables
table(pva1$WEALTH1)
table(pva1$WEALTH2)
pva1$WEALTH1[is.na(pva1$WEALTH1)]=9
pva1$WEALTH2[is.na(pva1$WEALTH2)]=9
unique(pva1$WEALTH1)
unique(pva1$WEALTH2)

#finding the percentage of missing values in each variable in the data set
mv=sapply(pva1, function(x){sum(is.na(x))/length(x)}*100)
mv

#removing the variables with more than 50% missing values
pva1=subset(pva1,select=mv<50)

#making a copy of the transformed data so far
pva1_1=pva1

#testing
#mv1=sapply(df, function(x){sum(is.na(x))/length(x)}*100)
#mv1
#testing over

#removing non relevant variables 
pva1_1=subset(pva1_1,select=-c(TCODE,ZIP,STATE,OSOURCE,MDMAUD_A,MDMAUD_F,MDMAUD_R,RFA_2R,MDMAUD,GEOCODE2,DATASRCE,ODATEDW,DOB,NOEXCH,CLUSTER,CLUSTER2,AGEFLAG,RFA_2A,RFA_2F,TARGET_D,CHILD03,CHILD07,CHILD12,CHILD18))
pva1_1=subset(pva1_1,select=-c(ADATE_2,ADATE_3,ADATE_4,ADATE_5,ADATE_6,ADATE_7,ADATE_8,ADATE_9,ADATE_10,ADATE_11,ADATE_12,ADATE_13,ADATE_14,ADATE_16,ADATE_17,ADATE_18,ADATE_19,ADATE_21,ADATE_22,ADATE_24))
pva1_1=subset(pva1_1,select=-c(RFA_2,RFA_3,RFA_4,RFA_5,RFA_6,RFA_7,RFA_8,RFA_9,RFA_10,RFA_11,RFA_12,RFA_13,RFA_14,RFA_16,RFA_17,RFA_18,RFA_19,RFA_21,RFA_22,RFA_24))
pva1_1=subset(pva1_1,select=-c(MAXADATE,MINRDATE,MAXRDATE))
pva1_1=subset(pva1_1,select=-c(FISTDATE,LASTDATE,NEXTDATE))
pva1_1=subset(pva1_1,select=-c(CONTROLN))

#TIMELAG=LASTTIME-FIRSTTIME
#imputing missing values in TIMELAG as 0 (because NEXTTIME=LASTTIME)
pva1_1$TIMELAG[is.na(pva1_1$TIMELAG)]=0
unique(pva1_1$TIMELAG)

#AGE
#imputing missing values in AGE as its median value
median_age=median(pva1_1$AGE[!is.na(pva1_1$AGE)])
pva1_1$AGE[is.na(pva1_1$AGE)]=median_age

#subsetting the data for performing PCA on the variables related to the neighborhood of the PVA 
#data for PCA
pva1_pca=subset(pva1_1,select = c(23:308))

#data with original variables
pva1_main=subset(pva1_1,select=c(1:22,309:324))
summary(pva1_main)

#converting the following transformed variables as numeric (0 or 1)
pva1_main$PVASTATE=as.numeric(pva1_main$PVASTATE)
pva1_main$RECINHSE=as.numeric(pva1_main$RECINHSE)
pva1_main$RECP3=as.numeric(pva1_main$RECP3)
pva1_main$RECPGVG=as.numeric(pva1_main$RECPGVG)
pva1_main$RECSWEEP=as.numeric(pva1_main$RECSWEEP)
pva1_main$MAJOR=as.numeric(pva1_main$MAJOR)
pva1_main$PEPSTRFL=as.numeric(pva1_main$PEPSTRFL)

#converting the target variable and other character variables to factor 
pva1_main$GENDER=as.factor(pva1_main$GENDER)
pva1_main$SESTATUS=as.factor(pva1_main$SESTATUS)
pva1_main$URBANICITY=as.factor(pva1_main$URBANICITY)
pva1_main$TARGET_B=as.factor(pva1_main$TARGET_B)

#outlier treatment on the following numeric variables
#winsorizing-setting the upper threshold as Q3+1.5*IQR
pva1_main$HIT[pva1_main$HIT>(3+1.5*IQR(pva1_main$HIT))]=(3+1.5*IQR(pva1_main$HIT))
pva1_main$MAXRAMNT[pva1_main$MAXRAMNT>(21+1.5*IQR(pva1_main$MAXRAMNT))]=(21+1.5*IQR(pva1_main$MAXRAMNT))
pva1_main$MINRAMNT[pva1_main$MINRAMNT>(10+1.5*IQR(pva1_main$MINRAMNT))]=(10+1.5*IQR(pva1_main$MINRAMNT))
pva1_main$AVGGIFT[pva1_main$AVGGIFT>(15+1.5*IQR(pva1_main$AVGGIFT))]=(15+1.5*IQR(pva1_main$AVGGIFT))
pva1_main$LASTGIFT[pva1_main$LASTGIFT>(20+1.5*IQR(pva1_main$LASTGIFT))]=(20+1.5*IQR(pva1_main$LASTGIFT))
pva1_main$NGIFTALL[pva1_main$NGIFTALL>(14+1.5*IQR(pva1_main$NGIFTALL))]=(14+1.5*IQR(pva1_main$NGIFTALL))
pva1_main$RAMNTALL[pva1_main$RAMNTALL>(135+1.5*IQR(pva1_main$RAMNTALL))]=(135+1.5*IQR(pva1_main$RAMNTALL))
pva1_main$NUMPROM[pva1_main$NUMPROM>(65+1.5*IQR(pva1_main$NUMPROM))]=(65+1.5*IQR(pva1_main$NUMPROM))
pva1_main$CARDPROM[pva1_main$NUMPROM>(26+1.5*IQR(pva1_main$CARDPROM))]=(26+1.5*IQR(pva1_main$CARDPROM))
pva1_main$NUMPRM12[pva1_main$NUMPRM12>(13+1.5*IQR(pva1_main$NUMPRM12))]=(13+1.5*IQR(pva1_main$NUMPRM12))
pva1_main$CARDPM12[pva1_main$CARDPM12>(6+1.5*IQR(pva1_main$CARDPM12))]=(6+1.5*IQR(pva1_main$CARDPM12))
pva1_main$CARDGIFT[pva1_main$CARDGIFT>(8+1.5*IQR(pva1_main$CARDGIFT))]=(8+1.5*IQR(pva1_main$CARDGIFT))
pva1_main$MALEMILI[pva1_main$MALEMILI>(0+1.5*IQR(pva1_main$MALEMILI))]=(0+1.5*IQR(pva1_main$MALEMILI))
pva1_main$MALEVET[pva1_main$MALEVET>(37+1.5*IQR(pva1_main$MALEVET))]=(37+1.5*IQR(pva1_main$MALEVET))
pva1_main$VIETVETS[pva1_main$VIETVETS>(39+1.5*IQR(pva1_main$VIETVETS))]=(39+1.5*IQR(pva1_main$VIETVETS))
pva1_main$WWIIVETS[pva1_main$WWIIVETS>(43+1.5*IQR(pva1_main$WWIIVETS))]=(43+1.5*IQR(pva1_main$WWIIVETS))
pva1_main$LOCALGOV[pva1_main$LOCALGOV>(9+1.5*IQR(pva1_main$LOCALGOV))]=(9+1.5*IQR(pva1_main$LOCALGOV))
pva1_main$STATEGOV[pva1_main$STATEGOV>(6+1.5*IQR(pva1_main$STATEGOV))]=(6+1.5*IQR(pva1_main$STATEGOV))
pva1_main$FEDGOV[pva1_main$FEDGOV>(4+1.5*IQR(pva1_main$FEDGOV))]=(4+1.5*IQR(pva1_main$FEDGOV))

#removing old files
rm(pva,pva1)

#making a copy of the data to the system
write.table(pva1_main,"C:/Users/sethi/Desktop/pva1_main.txt", sep=",")

#making a copy of the pca data
pva1_pca1=pva1_pca

#removing missing values in 
pva1_pca1[is.na(pva1_pca1)]=0

#installing package to perform normalization before PCA
install.packages("som")
library("som")
normalized_pva_pca=normalize(pva1_pca1,byrow=FALSE)
normalized_pva_pca=as.data.frame(normalized_pva_pca)
names(normalized_pva_pca)

#applying PCA
pr_out=prcomp(normalized_pva_pca, retx = TRUE, center = TRUE, scale. = TRUE)
summary(pr_out)
dim(pr_out$x)

#creating a dataframe of the principal components
pva1_pcs=as.data.frame(pr_out$x)

#selecting 42 principal components which account for 75% of the total variance
pva1_prcomps=subset(pva1_pcs,select = c(1:42))

#making a copy of the data to the system
write.table(pva1_prcomps,"C:/Users/sethi/Desktop/pva1_prcomps.txt", sep=",")

#merging the two dataframes-pva1_main and pva1_prcomps
df=cbind(pva1_main,pva1_prcomps)

##making a copy of the data to the system
write.table(df,"C:/Users/sethi/Desktop/pva_df.txt", sep=",")

PVA=data.frame(data)

#setting seed for reproducibility
set.seed(1234)

#Partition the data into training and testing 
train=sample (1:nrow(PVA), 7000)

#Define the train and test frame
df.train=PVA[train,]
df.test=PVA[-train,]

str(df.train)

#df$GENDER=as.factor(df$GENDER)
#df$SESTATUS=as.factor(df$SESTATUS)
df.train$URBANICITY=as.factor(df.train$URBANICITY)
df.train$TARGET_B=as.factor(df.train$TARGET_B)

#*******Random Forests*******
#creating a random forest model-1
rf=randomForest(TARGET_B~.,data=df.train,n.trees=100,interaction.depth=9,importance=T,proximity=T)

#predicting the class for the test data set
tree.pred_test=predict(rf,df.test,type="class")

#creating confusion matrix
table(tree.pred_test,df.test$TARGET_B)

#Using the importance() function, we can view the importance of each variable
importance (rf) 
varImpPlot (rf)

#creating a random forest model-2
rf=randomForest(TARGET_B~.,data=df.train,n.trees=250,interaction.depth=8,importance=T,proximity=T)

#predicting the class for the test data set
tree.pred=predict(rf,df.test,type="class")

#creating confusion matrix
table(tree.pred,df.test$TARGET_B) 

#Using the importance() function, we can view the importance of each variable
importance (rf) 
varImpPlot (rf)

#creating a random forest model-3
rf=randomForest(TARGET_B~.,data=df.train,n.trees=500,interaction.depth=9,importance=T,proximity=T)

#predicting the class for the test data set
tree.pred=predict(rf,df.test,type="class")

#creating confusion matrix
table(tree.pred,df.test$TARGET_B) 

#Using the importance() function, we can view the importance of each variable
importance (rf) 
varImpPlot (rf)
#*******Random Forests*******

#*******Boosted Trees*******
df_train=subset(df.train,select = -c(MALEMILI))
df_test=subset(df.test,select = -c(MALEMILI))

install.packages("gbm")
library(gbm)

set.seed(123) 
ntrees=5000
bt = gbm(TARGET_B~.,data=df_train,distribution="bernoulli",n.trees = ntrees,interaction.depth = 15)

summary(bt)
gbm.perf(bt)

tree.pred_bt=predict(bt,df_test,n.trees = ntrees,type="link") 
table(tree.pred_bt,df_test$TARGET_B)
#*******Boosted Trees*******

#*******SVM********
install.packages("e1071")
library("e1071")
svm_clf=svm(TARGET_B~.,data = df_train,kernel="poly",degree=3,gamma=10,cost=5)
summary(svm_clf)
  
svm.pred=predict(svm_clf,df_test)
table(svm.pred,df_test$TARGET_B)

#changing parameter values
svm_clf=svm(TARGET_B~.,data = df_train,kernel="radial",gamma=1,cost=0.5)
summary(svm_clf)

svm.pred=predict(svm_clf,df_test)
table(svm.pred,df_test$TARGET_B)

#tuning SVM parameters
svm_clf_tune=tune.svm(TARGET_B~.,data=df_train, kernel="radial", cost=c(0.7,1.1), gamma=c(.8,1.2))
print(svm_clf_tune)

svm_clf=svm(TARGET_B~.,data = df_train,kernel="radial",gamma=1.1,cost=0.8)
summary(svm_clf)

svm.pred=predict(svm_clf,df_test)
table(svm.pred,df_test$TARGET_B)

#********SVM********

