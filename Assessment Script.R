


## HEADER ####
## Who: Krishnakumar
## What: Assessment on Statistical analysis for data science(C7081)
## When:2022:11:06
####

## Contents ####
## Introduction
## setup
## linear Regression
## multiple Regression
## Decision Tree
## Random Forest
## Logistic regression
## Result
## literature citation


## introduction ####
# The data set contains several parameters which are considered important during the application for Masters Programs.
# The important parameters are GRE score, TOEFL score, university rating, SOP (Statement of Purpose), LOR (Letter of Recommendation), CGPA, research and chance of admit.
# In this data set, 400 entries are included.
# we are going to predict that which are all the factors are important for getting the admission into the university.

## setup ####
setwd("C:/Users/DELL/Downloads/admission into university")
library(readr)
adm_data <- read_csv("adm_data.csv")
names(adm_data)
str(adm_data)
plot(adm_data)  #just seeing the correlation
# GRE Score,TOEFL Score and CGPA are important because they are highly correlated.
# Summary of all variables
summary(adm_data)


## Linear Regression ####
adm.lm <- lm(ChanceofAdmit ~ CGPA, data = adm_data)
adm.lm
#Make liner regression with Chance of Admit as a independent variable and CGPA has a dependent variable

# summary
summary(adm.lm)
#The estimates (Estimate) for the model parameters – the value of the y-intercept (in this case -1.071) and the estimated effect of Chance of Admit on CGPA (0.208).
#The final three lines are model diagnostics – the most important thing to note is the p-value (here it is 2.2e-16, or almost zero), which will indicate whether the model fits the data well.

# plot the data
plot(
  ChanceofAdmit ~ CGPA,
  data = adm_data,
  xlab = "Chance of Admit",
  ylab = "CGPA",
  main = "Admission plot",
  col = "blue",
  lwd = 1,
  pch = 20
)

# Add regression line
abline(reg = adm.lm, col = "red", lwd = 3)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(adm.lm)

# residual distribution
par(mfrow = c(1, 1))
hist(residuals(adm.lm), main = "")
# Residuals are the unexplained variance. They are not exactly the same as model error, but they are calculated from it, so seeing a bias in the residuals would also indicate a bias in the error.
# The most important thing to look for is that the red lines representing the mean of the residuals are all basically horizontal and centered around zero. This means there are no outlier.
# In the Normal Q-Q plot in the top right, we can see that the real residuals from our model form an almost perfectly one-to-one line with the theoretical residuals from a perfect model.
# Based on these residuals, we can say that our model meets the assumption of homoscedasticity.

# Visualize the results with Graph
library(ggplot2)
adm.graph <-
  ggplot(adm_data, aes(x = ChanceofAdmit, y = CGPA)) + geom_point()
adm.graph

# Add regression line
adm.graph <- adm.graph + geom_smooth(method = "lm", col = "blue")
adm.graph

# Conclusion
# From these results, we can say that there is a significant positive relationship between Chance of Admit and CGPA (p-value < 0.001), with a 0.208-unit (+/- 0.01) increase in Chance of Admit for every unit increase in CGPA.

## Multiple regression ####
# Check for linearity
# We can check this using two scatter plots: one for GRE Score and Chance of admit, and one for TOEFL Score and Chance of admit.
plot(adm_data$ChanceofAdmit, adm_data$GREScore)
plot(adm_data$ChanceofAdmit, adm_data$TOEFLScore)
adm.multi <-
  lm(ChanceofAdmit ~ GREScore + TOEFLScore, data = adm_data)
adm.multi

# summary
summary(adm.multi)
# The estimated value of chance of admit on GRE Score is 0.0058, while the estimated value of TOEFL Score is 0.0094.


# Check for homoscedasticity
par(mfrow = c(2, 2))
plot(adm.multi)
par(mfrow = c(1, 1))

# Create Equation for Regression Model
 # y = a + b1x1 + b2x2 +...bnxn

# Apply Equation for predicting New Values
# we can predict the chance of Admission when we set new values to GRE Score and TOEFL Score.

Y = -2.128+(0.005814)*325+(0.009412)*105
Y

#Conclusion
#If the person with GRE Score is 325 and TOEFL Score is 105 and his chance of Admit is 0.74%

# Visualize the results with a graph
plotting.data <- expand.grid(
  GREScore = seq(min(adm_data$GREScore), max(adm_data$GREScore), length.out =
                   30),
  TOEFLScore = c(
    min(adm_data$TOEFLScore),
    mean(adm_data$TOEFLScore),
    max(adm_data$TOEFLScore)
  )
)

plotting.data$predicted.y <-
  predict.lm(adm.multi, newdata = plotting.data)

plotting.data$GREScore <- round(plotting.data$GREScore, digits = 2)

multi.plot <-
  ggplot(adm_data, aes(x = GREScore, y = ChanceofAdmit)) + geom_point()
multi.plot

multi.plot <- multi.plot +
  geom_line(data = plotting.data,
            aes(x = GREScore, y = predicted.y, color = TOEFLScore),
            size = 1.25)
multi.plot

multi.plot <- multi.plot + theme_bw() +
  labs(
    title = "Chance of Admit \n as a function of GREScore to work and TOEFLScore",
    x = "GRE Score",
    y = "Chance of Admit",
    color = "TOEFL Score"
  )
multi.plot


## Decision tree ####
library(tree)
library(party)
library(randomForest)
input.data <- adm_data[c(1:105),]
output.tree <-
  ctree(ChanceofAdmit ~ CGPA + GREScore + TOEFLScore, data = input.data)
plot(output.tree)



# conclusion
# From the decision tree we can conclude that anyone whose CGPA is more than 8.8 and GRE Score is more than 311 will have high admission chance.

## Random forest ####
library(randomForest)
library(party)
adm.reg <- randomForest(ChanceofAdmit ~ ., data = adm_data)
print(adm.reg)
round(importance(adm.reg), 2)

# Conclusion
# From the random forest we can conclude that the CGPA,GRE Score and TOEFL Score are the important factors for getting the admission into the university.

## Logistic Regression ####
? glm
glm.adm <- glm(ChanceofAdmit ~ ., data = adm_data)
glm.adm
summary(glm.adm)

# Coefficients
# Intercept coefficient(-1.2594325)
# GRE Score coefficient(0.0017374)
# TOEFL Score Coefficient(0.0029196)
# LOR Coefficient( 0.0223531)
# CGPA Coefficient(0.1189395)
# Research Coefficient(0.0245251)
# These are all the factors impact on the Chance of Admit.

# p-value
# The p-value of University Rating and SOP are so high (more than 0.05) which implies that insignificant to the of Chance of Admit.
# while other factors such as GRE Score,TOEFL Score,CGPA and LOR with low p-value(less than 0.05)which are significant to the Chance of Admit.

# Predict()
glm.predict <- (predict(glm.adm, type = "response"))
glm.predict[1:10]

## Result ####
# By performing various statistical analysis CGPA,GRE Score and TOEFL Score these are all the crucial factors obtain the Chance of Admit to the University.

## literature citation ####
  
# Mohan S Acharya, Asfia Armaan, Aneeta S Antony : A Comparison of Regression Models for Prediction of Graduate Admissions, IEEE International Conference on Computational Intelligence in Data Science 2019.
