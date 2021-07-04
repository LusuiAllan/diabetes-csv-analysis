#Load data 
MyData<- read.csv("diabetes2.csv")
head(MyData)
summary(MyData)

#Clean data
#Check missing values
any(is.na(MyData))
#check null values
any(is.null(MyData))
#Results is False meaning there are no missing values and no null value

#Test outliers of each variable
install.packages("outliers")
library(outliers)
outlier(MyData)
hist(MyData$Pregnancies, xlab= "Pregnancies")
hist(MyData$Glucose, xlab= "Glucose")
hist(MyData$BloodPressure, xlab= "BloodPressure")
hist(MyData$SkinThickness, xlab= "Skin Thickness")
hist(MyData$Insulin, xlab= "Insulin")
hist(MyData$BMI, xlab= "BMI")
hist(MyData$DiabetesPedigreeFunction, xlab= "Diabetes Pedigree Function")
hist(MyData$Age, xlab= "Age")


#Replace outlier with median
MyData$BloodPressure[MyData$BloodPressure %in% 0]<-median(MyData$BloodPressure)
MyData$Glucose[MyData$Glucose %in% 0]<-median(MyData$Glucose)
MyData$SkinThickness[MyData$SkinThickness%in% 0]<-median(MyData$SkinThickness)
MyData$Insulin[MyData$Insulin %in% 0]<-median(MyData$Insulin)
MyData$BMI[MyData$BMI %in% 0]<-median(MyData$BMI)
summary(MyData)

library(outliers)
outlier(MyData)

#Split the data into two
install.packages("caret")
library(caret)
dt <- createDataPartition(MyData$Outcome,p=.80,list=F)
training <- MyData[dt,]
testing <-MyData[-dt,]
#Fit a model
model <- glm(Outcome ~.,family="binomial", data = training)
summary(model)
#From summary we can see all independent variables are significant except BloodPressure,SkinThickness and Insulin

#Final model
model6<-glm(Outcome~.-SkinThickness-Age-BloodPressure-Insulin,family="binomial", data= training)
summary(model6)

#checking on testing data
train_prob<-predict(model6,training,type = "response")
train_pred<-ifelse(train_prob<0.5,0,1)
train_acc<-mean(train_pred == training$Outcome)

test_prob<-predict(model6,testing,type = "response")
test_pred<-ifelse(test_prob<0.5,0,1)
test_acc<-mean(test_pred == testing$Outcome)

cat("Model 6 training accuracy:  ",train_acc,"\n",
    "Model 6 testing accuracy:  ",test_acc,"\n\n" )


#goodness of fit
glm(formula =  Outcome~.-Age-BloodPressure-SkinThickness,family="binomial", data=training)

#oddsratio
exp(cbind(OR = coef(model6),confint(model6)))


