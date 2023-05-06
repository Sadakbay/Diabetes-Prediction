library(tidyverse)  # for data manipulation
library(dlstats)    # for package download stats
library(dplyr)
library(pROC)
library(car)
library("ggpubr")
library(tidyverse)
data <- read.csv("/Users/serikzhan/Desktop/Spring 2022/Data Analysis and Visualization with R (CS 555)/Project/diabetes.csv")
data

# Summary of the data
summary(data)


#Cheking for outliers
par(mfrow=c(2,1))
boxplot(data$Glucose, xlab = "Glucose", main = "Glucose")
boxplot(data$BloodPressure, xlab = "BloodPressure", main = "Blood Pressure")
boxplot(data$SkinThickness, xlab = "SkinThickness", main = "Skin Thickness")
boxplot(data$Insulin, xlab = "Insulin", main = "Insulin")
boxplot(data$BMI, xlab = "BMI", main = "Body Mass Index")
boxplot(data$Age, xlab = "Age", main = "Age")

plot(data$Outcome, data$Insulin, main = 'Insulin levels')


# Calculate Correlation Coefficients
install.packages("corrplot")
library(corrplot)

cor(data)
corrplot(cor(data),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL) 


# Plotting means of different groups 
data_Glucose = select(data, Glucose, Outcome)
data_BloodPressure = select(data, BloodPressure, Outcome)
data_SkinThickness = select(data, SkinThickness, Outcome)
data_Insulin = select(data, Insulin, Outcome)
data_BMI = select(data, BMI, Outcome)
data_Age = select(data, Age, Outcome)



ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol = 3)

plot1 = ggboxplot(data_Glucose, x = "Outcome", y = "Glucose",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Glucose level", xlab = "Outcome")

plot2 = ggboxplot(data_BloodPressure, x = "Outcome", y = "BloodPressure",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "BloodPressure level", xlab = "Outcome")

plot3 = ggboxplot(data_SkinThickness, x = "Outcome", y = "SkinThickness",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "SkinThickness level", xlab = "Outcome")

plot4 = ggboxplot(data_Insulin, x = "Outcome", y = "Insulin",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Insulin level", xlab = "Outcome")

plot5 = ggboxplot(data_BMI, x = "Outcome", y = "BMI",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Body Mass Index level", xlab = "Outcome")

plot6 = ggboxplot(data_Age, x = "Outcome", y = "Age",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Age", xlab = "Outcome")

# Calculating one way ANOVA

Glucose.aov <- aov(Glucose ~ Outcome, data = data_Glucose)
summary(Glucose.aov)

BloodPressure.aov <- aov(BloodPressure ~ Outcome, data = data_BloodPressure)
summary(BloodPressure.aov)

SkinThickness.aov <- aov(SkinThickness ~ Outcome, data = data_SkinThickness)
summary(SkinThickness.aov)

Insulin.aov <- aov(Insulin ~ Outcome, data = data_Insulin)
summary(Insulin.aov)

BMI.aov <- aov(BMI ~ Outcome, data = data_BMI)
summary(BMI.aov)

Age.aov <- aov(Age ~ Outcome, data = data_Age)
summary(Age.aov)



# Age above 35
par(mfrow=c(2,3))

newdata <- data[which(data$Age > 35),]
newdata
nrow(newdata)

Age2.aov <- aov(Age ~ Outcome, data = newdata)
summary(Age2.aov)

ggboxplot(newdata, x = "Outcome", y = "Age",
          color = "red",
          palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Age", xlab = "Outcome")

corrplot(cor(newdata),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL) 

Glucose2.aov <- aov(Glucose ~ Outcome, data = newdata)
summary(Glucose2.aov)

BloodPressure2.aov <- aov(BloodPressure ~ Outcome, data = newdata)
summary(BloodPressure2.aov)

SkinThickness2.aov <- aov(SkinThickness ~ Outcome, data = newdata)
summary(SkinThickness2.aov)

Insulin2.aov <- aov(Insulin ~ Outcome, data = newdata)
summary(Age2.aov)

BMI2.aov <- aov(BMI ~ Outcome, data = newdata)
summary(BMI2.aov)


#backward selection
backmodel = lm(newdata$Glucose ~ + newdata$BloodPressure + newdata$SkinThickness+ newdata$Insulin +newdata$BMI + newdata$Age)
b1 = step(backmodel, direction="backward")

#forward selection
forwardmodel = step(lm(newdata$Glucose~1), direction="forward", scope=(~ newdata$BloodPressure + newdata$SkinThickness+ newdata$Insulin +newdata$BMI + newdata$Age))

#ANOVA 

anova(backmodel)


# Logistic Regression

glm.fit <- glm(newdata$Outcome ~ newdata$Glucose + newdata$BloodPressure + newdata$SkinThickness+ newdata$Insulin +newdata$BMI + newdata$Age, family = binomial)
summary(glm.fit)


# ROC
roc(newdata$Outcome~newdata$Glucose, main = 'Glucose', plot = TRUE)
roc(newdata$Outcome~newdata$Insulin, main = 'Insulin', plot = TRUE)
roc(newdata$Outcome~newdata$BMI, main = 'BMI', plot = TRUE)
roc(newdata$Outcome~newdata$BloodPressure, main = 'BloodPressure', plot = TRUE)
roc(newdata$Outcome~newdata$SkinThickness, main = 'SkinThickness', plot = TRUE)
roc(newdata$Outcome~newdata$Age, main = 'Age', plot = TRUE)









