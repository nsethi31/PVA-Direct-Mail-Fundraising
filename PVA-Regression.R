#importing the data set
df=read.csv("C:/Users/sethi/Desktop/pva_df.csv")

df$SESTATUS=as.factor(df$SESTATUS)
str(df)

#Filtering data set where TARGET_B=1 (i.e. considering only donors)
PVA=df[df$TARGET_B==1,]
attach(PVA)

#Removing the TARGET_B variable
PVA=subset(PVA,select = -c(TARGET_B))

#Numeric variables for checking the correlation between them
PVA_num=subset(PVA,select = -c(GENDER,URBANICITY,SESTATUS))

#loading package "caret" for calculating correlation between numeric variables
install.packages("caret")
library("caret")

#setting cutoff value of correlation coeff=0.7
ax=findCorrelation(x = cor(PVA_num), cutoff = 0.7)
#removing variables with correaltion coeff>0.7
PVA_num1=PVA_num[,-ax]
#variables removed- PVASTATE,NUMPROM,NUMPRM12,NGIFTALL,MINRAMNT,LASTGIFT,AVGGIFT

#setting seed for reproducibility
set.seed(1234)

#Partition the data into training and testing 
train=sample (1:nrow(PVA_num1), 2450)

#Define the train and test set
PVA_train=PVA_num1[train,]
PVA_test=PVA_num1[-train,]

linreg=lm(TARGET_D~.,data = PVA_train)
summary(linreg)

#Stewise Regression
install.packages("MASS")
library(MASS)
fit=lm(TARGET_D~.,data = PVA_num1)
step=stepAIC(fit, direction="both")
step$anova # display results 

#Final model contains-TARGET_D ~ RECINHSE + RECPGVG + HOMEOWNR + WWIIVETS + MAJOR + 
#WEALTH2 + PEPSTRFL + CARDPROM + CARDPM12 + RAMNTALL + CARDGIFT + 
 # MAXRAMNT + HPHONE_D + PC2 + PC3 + PC5 + PC15 + PC36

#subsetting the train and test set by keeping these variables

PVA_train=subset(PVA_train,select = c(TARGET_D,RECINHSE,RECPGVG , HOMEOWNR , WWIIVETS , MAJOR , 
                                      WEALTH2 , PEPSTRFL , CARDPROM , CARDPM12 , RAMNTALL , CARDGIFT , 
                                      MAXRAMNT , HPHONE_D , PC2 , PC3 , PC5 , PC15 , PC36))

#fitting the regression model on train data
fit=lm(TARGET_D~.,data = PVA_train)
summary(fit)

PVA_test=subset (PVA_test,select = c(TARGET_D,RECINHSE,RECPGVG , HOMEOWNR , WWIIVETS , MAJOR , 
                  WEALTH2 , PEPSTRFL , CARDPROM , CARDPM12 , RAMNTALL , CARDGIFT , 
                  MAXRAMNT , HPHONE_D , PC2 , PC3 , PC5 , PC15 , PC36))


#applying the trained model on test data
lr_pred=predict.lm(fit,PVA_test)
summary(lr_pred)

#or this
PVA_test1=subset(PVA_test,select = -TARGET_D)
pred=predict(fit,PVA_test1)
summary(pred)


