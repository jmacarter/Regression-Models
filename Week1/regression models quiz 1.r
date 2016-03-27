# Regression Models
# Coursera
# Quiz 1


# Question 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
# Manual method
mu <- sum(w*x)/sum(w)
round(mu,4)
# Alternate method:
lm(x ~ 1, weights = w)$coefficients



# Question 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit.origin <- lm( y ~ x - 1 )
summary(fit.origin)
lm(y ~ 0 + x)$coefficients

# Question 3
data(mtcars)
fit <- lm(mpg ~ wt, mtcars)
summary(fit)

lm(mpg ~ wt, data = mtcars)$coefficients


#Question 4
$$ \begin{align} \hat \beta_1 &= Cor(X,Y) \frac{Sd(Y)}{Sd(X)} \ &= (0.5) \frac{Sd(Y)}{0.5Sd(Y)} \ &= 1 \end{align} $$

#Question 5
1.5 * 0.4

# Question 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xbar = (x - mean(x)) / sd(x)
xbar[1]

# Question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)$coefficients

#Question 8
#You know that both the predictor and response have mean 0. What can be said about the intercept when you fit a linear regression?

#It must be exactly one.
#It must be identically 0. <--
#Nothing about the intercept can be said from the information given.
#It is undefined as you have to divide by zero.


# Question 9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

#Question 10
# Consider taking the slope having fit Y as the outcome and X as the predictor, $\beta_1$ and the slope from fitting X as the outcome and Y as the predictor, $\gamma_1$, and dividing the two as $\beta_1/\gamma_1$. What is this ratio always equal to?
# 
# $$ \begin{align} \beta_1 &= \frac{\sum_i X_i Y_i}{\sum_i Y_i^2} \ \gamma_1 &= \frac{\sum_i X_i Y_i}{\sum_i X_i^2} \ \frac{\beta_1}{\gamma_1} &= \frac{\sum_i Y_i^2}{\sum_i X_i^2} \ &= \frac{Var(X)}{Var(Y)} \end{align} $$
#      
# Cor(Y,X)
# 1
# 2SD(Y)/SD(X)
# Var(Y)/Var(X) <-
