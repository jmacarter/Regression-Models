require(datasets); data(swiss); require(GGally); require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"),params = c(method = "loess"))
g

summary(lm(Fertility ~ . , data = swiss))

summary(lm(Fertility ~ . , data = swiss))$coefficients

summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients



#How can adjustment reverse the sign of an effect? Let's try a simulation.
n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))

g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + geom_point(size = 4) 
g

g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4) 
g2

#What if we include an unnecessary variable?
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)


#Insect Sprays
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

?InsectSprays