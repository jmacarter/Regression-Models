## Q1
#Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.

#* -6.071 <-
#* 33.991
#* -4.256
#* -3.206
data(mtcars)
df <- mtcars
df$cyl <- as.factor(df$cyl)
fit <- lm(mpg ~ cyl + wt, data = df)
summary(fit)
# expected change in mpg if increase of 4 for cyl
fit$coefficients[3]
-6.071


## Q2
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Compare the adjusted by weight effect of 8 cylinders as compared to 4 the unadjusted. What can be said about the effect?.

# * Including or excluding weight does not appear to change anything regarding the estimated impact of number of cylinders on mpg.
# * Within a given weight, 8 cylinder vehicles have an expected 12 mpg drop in fuel efficiency.
# * Holding weight constant, cylinder appears to have more of an impact on mpg than if weight is disregarded.
# * Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded. <--
     
fit <- lm(mpg ~ cyl + wt, data = df)
fitUW <- lm(mpg ~ cyl, data = df)
summary(fit)
summary(fitUW)
# Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.


## Q3
# Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a factor variable and weight as confounder. Now fit a second model with mpg as the outcome model that considers the interaction between number of cylinders (as a factor variable) and weight. Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.

# * The P-value is small (less than 0.05). Thus it is surely true that there is no interaction term in the true model.
# * The P-value is small (less than 0.05). Thus it is surely true that there is an interaction term in the true model.
# * The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms may not be necessary. <--
# * The P-value is small (less than 0.05). So, according to our criterion, we reject, which suggests that the interaction term is necessary
# * The P-value is small (less than 0.05). So, according to our criterion, we reject, which suggests that the interaction term is not necessary.
# * The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms is necessary.

library("lmtest")
fit <- lm(mpg ~ cyl + wt, data = df)
fitU <- lm(mpg ~ cyl*wt, data = df)
lrtest(fit, fitU)
# The likelihood-ratio test is appropriate only if the two models you are comparing are nested, i. e., if one can be retrieved from the other, e. g., by fixing parameters (e. g., to zero). Models with more parameters will always fit better, the question the LR test answers is whether the increase in fit is defensible given the amount of added parameters.  To compare non-nested models, you may use information criteria such as AIC or BIC (the smaller the better).


## Q4
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight inlcuded in the model as
# 
# lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# How is the wt coefficient interpretted?
# 
# * The estimated expected change in MPG per half ton increase in weight. 
# * The estimated expected change in MPG per half ton increase in weight for the average number of cylinders.
# * The estimated expected change in MPG per one ton increase in weight.
# * The estimated expected change in MPG per half ton increase in weight for for a specific number of cylinders (4, 6, 8).
# * The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8). <--

fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit)
# The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).


## Q5
# Consider the following data set
# x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
# y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the hat diagonal for the most influential point

# * 0.2025
# * 0.2804
# * 0.9946
# * 0.2287

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
lm.influence(fit)
max(lm.influence(fit)$hat)

0.9946



## Q6
# Consider the following data set
# x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
# y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# 
# Give the slope dfbeta for the point with the highest hat value.

# * 0.673
# * -.00134
# * -134
# * -0.378

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
(imf <- influence.measures(fit))
df <- as.data.frame(imf$infmat)
df[which(df$hat == max(df$hat)),'dfb.x']
-133.8226


## Q7
# Consider a regression relationship between Y and X with and without adjustment for a third variable Z. Which of the following is true about comparing the regression coefficient between Y and X with and without adjustment for Z.
# It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.

