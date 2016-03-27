## Q1
## Nice recap on logit analysis on http://www.ats.ucla.edu/stat/r/dae/logit.htm
library(MASS)
?shuttle
fit <- glm(use ~ wind, data = shuttle, family=binomial)
summary(fit)
## Give the estimated odds ratio for autolander use comparing head winds, labeled as "head" in the variable headwind (numerator) to tail winds (denominator)
(logodds = fit$coefficients[2]) # head wind over tail wind, having tail wind instead of head wind changes the log odds of use by -0.03, or changes the odd ratios of use by exp(-0.03) = 0.97. The 'auto' use is less likely for tail wind. How less likely: exp(-0.03).
# logoddHEAD = logoddTAIL + 0.03 => oddHEAD/oddTAIL = 0.97 (NB: oddHEAD = pAUTO/pNONAUTO if HEAD)
exp(logodds) # well it is about the same chance (=0.97)

## Q2
fit <- glm(use ~ wind + magn, data = shuttle, family=binomial)
summary(fit)
(logodds = fit$coefficients[2])
exp(logodds)


## Q3
# If you fit a logistic regression model to a binary variable, for example use of the autolander, then fit a logistic regression model for one minus the outcome (not using the autolander) what happens to the coefficients?
fit <- glm(use ~ wind + magn, data = shuttle, family=binomial)
df <- shuttle
df$use <- relevel(shuttle$use,"noauto")
fit2 <- glm(use ~ wind + magn, data = df, family=binomial)
summary(fit2)
summary(fit)
#The coefficients reverse their signs (log odds). The odd ratios are inverted (1/X)

## Q4
data(InsectSprays)
str(InsectSprays)
fit <- glm(count ~ spray, data=InsectSprays, family=poisson)
summary(fit)
# Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
# the estimated count for SPRAY A would be exp(fit$coefficients[1])
# the estimated count for SPRAY B would be exp(fit$coefficients[1]+fit$coefficients[2])
# so the relative rates (SPRAY A over SPRAY B)
exp(fit$coefficients[1])/exp(fit$coefficients[1]+fit$coefficients[2]) # 0.95 => so SPRAY B is expected to be slightly better to kill insects 

## Q5
# glm(count ~ x + offset(t), family = poisson),  x is a factor variable comparing a treatment (1) to a control (0) and t is the natural log of a monitoring time. 
#  What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), family = poisson) where t2 <- log(10) + t
df <- InsectSprays
df$t <- seq(1:nrow(df))
df$t2 <- log(10) + df$t
fit <- glm(count ~ spray + offset(t), data=df, family=poisson)
fitT <- glm(count ~ spray + offset(t2), data=df, family=poisson)
summary(fit)
summary(fitT)
# no change in the coefficient of spray (or x), while there is some change in the intercept
# More mathematically we have
# b0 + b1 X + log(10) + t = b0' + b1' X + t
# => bo = b0' + log(10)
# => b1 = b1'   (no change in the coefficient of X)

## Q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- c(0) # 2 LINES
splineTerms <- sapply(knots, function(knot) (x>0)*(x-knot))
xMat <- cbind(x,splineTerms)
fit <- lm(y ~ xMat)
yhat <- predict(fit)
summary(fit)
plot(x,y,frame = FALSE, pch = 21, bg = 'lightblue',cex=2)
lines(x,yhat,col='red',lwd=2)
# slope is 
fit$coefficients[2]+fit$coefficients[3]
# b0 + b1 X + b2 (X-c) => slope = b1+b2