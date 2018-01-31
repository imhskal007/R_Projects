#Load Data - Update any attributes with ? as NA
#Auto_mpg <- read.table("/Users/Lux/Desktop/510_R/auto-mpg.data",na.strings="?",stringsAsFactors=FALSE)
# Testing...import file.
Auto_mpg <- read.table("C:/Users/email/Documents/R_Project_510/auto-mpg.data",na.strings="?",stringsAsFactors=FALSE)
#Update Column names
colnames(Auto_mpg) <- c("Mpg","Cylinders","Displacement","Horsepower","Weight","Acceleration","Model_Year","Origin","Car_Name")
#Delete Records where attribute has value NA - this will delete 6 records
Auto_mpg<-Auto_mpg[complete.cases(Auto_mpg),]
#Convert Horsepower variable to numeric
Auto_mpg$Horsepower=as.numeric(as.character(Auto_mpg$Horsepower[1:392]))
summary(Auto_mpg)
#First 300 rows in Predict table 1to 300
Predict_Data <- Auto_mpg[1:300,]
#Next 92 records or Remaining Records
Actual_Data <- Auto_mpg[301:nrow(Auto_mpg),]
#Verify five point values of all variables
summary(Predict_Data)
#compare all continuous variables

# Plot Regression line for all 4 variables - to check for realtionship

layout(matrix(1:4,2,2))
plot(Predict_Data$Acceleration,Predict_Data$Mpg)
abline (lm(Predict_Data$Mpg ~ Predict_Data$Acceleration))
plot(Predict_Data$Weight,Predict_Data$Mpg)
abline (lm(Predict_Data$Mpg ~ Predict_Data$Weight))
plot(Predict_Data$Displacement,Predict_Data$Mpg)
abline (lm(Predict_Data$Mpg ~ Predict_Data$Displacement))
plot(Predict_Data$Horsepower,Predict_Data$Mpg)
abline (lm(Predict_Data$Mpg ~ Predict_Data$Horsepower))
#Histogram of Independent Variables , density observation 
layout(matrix(1:4,2,2))
hist(Predict_Data$Acceleration, prob=TRUE)
lines(density(Predict_Data$Acceleration),,col="RED", lwd=2)
hist(Predict_Data$Weight, prob=TRUE)
lines(density(Predict_Data$Weight),,col="RED", lwd=2)
hist(Predict_Data$Horsepower, prob=TRUE)
lines(density(Predict_Data$Horsepower),,col="RED", lwd=2)
hist(Predict_Data$Displacement, prob=TRUE)
lines(density(Predict_Data$Displacement),,col="RED", lwd=2)
#Distribution
layout(matrix(1:4,2,2))
plot(sort(Predict_Data$Acceleration))
plot(sort(Predict_Data$Weight))
plot(sort(Predict_Data$Horsepower))
plot(sort(Predict_Data$Displacement))
#identifying Outliers
outlier_Ac<-boxplot.stats(Predict_Data$Acceleration)$out
#View(outlier_Ac) # 3 Outliers
outlier_Dis<-boxplot.stats(Predict_Data$Displacement)$out
#View(outlier_Dis) #No Outliers
outlier_Hp<-boxplot.stats(Predict_Data$Horsepower)$out
#View(outlier_Hp) #No Outliers
outlier_Weight<-boxplot.stats(Predict_Data$Weight)$out
#View(outlier_Weight) #No Outliers
outlier_Mpg<-boxplot.stats(Predict_Data$Mpg)$out
#View(outlier_Mpg) # 2 Outliers
#Deleting outliers based on MPG
Predict_Data <- Predict_Data[Predict_Data$Mpg < 39.4,]
# to test---Predict_Data <- Predict_Data[Predict_Data$Mpg < min(outlier_Mpg-0.1),]
#Deleting outliers based on Acceleration
Predict_Data <- Predict_Data[Predict_Data$Acceleration < 23.5,]
Predict_Data <- Predict_Data[Predict_Data$Acceleration > 8,]
#Building model for Mpg Prediction
#Q1 - part 1 Model - with Mpg and Horsepower
lm.testHp <- lm (Predict_Data$Mpg ~ Predict_Data$Horsepower)
summary(lm.testHp)
coefficients(lm.testHp)
plot(lm.testHp)
anova(lm.testHp) # Analysing Variance Table
predict(lm.testHp,data=Actual_Data,level=0.95,interval = "confidence") # Prediction of upper and lower level of MPG 
confint(lm.testHp,conf.level=0.95)
#Q1 (a) residuals vs. the predictor variable
layout(matrix(1:2,2,2))
plot(Predict_Data$Horsepower,resid(lm.testHp),main="1(a)Residuals Vs Predictor Variable")
abline(0,0)
#Q1 (b) absolute value of the residuals vs. the predictor variable
ab_res<-abs(resid(lm.testHp)) #absolute value of residuals for the model
plot(Predict_Data$Horsepower,ab_res,main="1(b)Absolute Residuals Vs Predictor Variable")

#Q1 (C) Histogram of Residuals
hist(resid(lm.testHp))
hist(stdres(lm.testHp)) #Normalised Residual values
#Q1 - Part 2 Test with 92 records model Horsepower(Histogram)
ComHp<-coef(lm.testHp)[1]+coef(lm.testHp)[2]*Actual_Data$Horsepower
ResHp<-Actual_Data$Mpg-ComHp
Actual_Data<-cbind(Actual_Data,ComHp,ResHp)
plot(Actual_Data$Mpg,Actual_Data$ComHp, main="Actual Mpg Vs Predicted Mpg from model Hp ")
layout(matrix(1:2,2,2))
hist(Actual_Data$ResHp, prob=TRUE)
lines(density(Actual_Data$ResHp),,col="RED", lwd=2)
hist(resid(lm.testHp), prob=TRUE)
lines(density(resid(lm.testHp)),,col="RED", lwd=2)
#Q1 - Conclusion 
plot(Predict_Data$Horsepower,Predict_Data$Mpg)# 300 records with which model is created
lines(Actual_Data$Horsepower,ComHp,lty=1) 
# Lines indicate the predicted value based on the model,
# More the horspower , lesser the MPG
#Q2 - Part 1 Selected Model 6 - with Mpg and Acceleration + Displacement 0.7319
cor.test(Predict_Data$Acceleration,Predict_Data$Displacement)
Q2_mod6 <- lm (Predict_Data$Mpg ~ Predict_Data$Acceleration +Predict_Data$Displacement)
summary(Q2_mod6)
coefficients(Q2_mod6)
layout(matrix(1:4,2,2))
plot(Q2_mod6)
confint(Q2_mod6)
#Q2 - Part 2 - Test with 92 records model Ac & Dp
Q2AcDp<-coef(Q2_mod6)[1]+coef(Q2_mod6)[2]*Actual_Data$Acceleration+coef(Q2_mod6)[3]*Actual_Data$Displacement
ResQ2<-Actual_Data$Mpg-Q2AcDp
Actual_Data<-cbind(Actual_Data,Q2AcDp,ResQ2)
layout(matrix(1:2,2,2))
hist(resid(Q2_mod6), prob=TRUE)
lines(density(resid(Q2_mod6)),,col="RED", lwd=1) # CLose to Normal
hist(Actual_Data$ResQ2, prob=TRUE)
lines(density(Actual_Data$ResQ2),,col="RED", lwd=1)
#(a)
res<-resid(Q2_mod6)
layout(matrix(1:2,2,2))
plot(Predict_Data$Acceleration,res,main="1(a)Residuals Vs Predictor Variable1")
abline(0,0)
plot(Predict_Data$Displacement,res,main="1(a)Residuals Vs Predictor Variable1")
abline(0,0)

#Q1 (b) absolute value of the residuals vs. the predictor variable
ab_res<-abs(resid(Q2_mod6))
plot(Predict_Data$Acceleration,ab_res,main="1(b)Absolute Residuals Vs Predictor Variable")
plot(Predict_Data$Displacement,ab_res,main="1(b)Absolute Residuals Vs Predictor Variable")

#Q1 (b) Histogram of Residuals
hist(resid(Q2_mod6), prob=TRUE, main="Residual")
lines(density(resid(Q2_mod6)),,col="RED", lwd=1,)
# Include MASS Library
hist(stdres(Q2_mod6),prob=TRUE, main="Normalised Residual") #Normalised Residual values
lines(density(stdres(Q2_mod6)),,col="RED", lwd=1)
#=============================================================================#*



