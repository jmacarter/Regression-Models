library(manipulate)
library(UsingR)

myHist <- function(mu) {
     hist(galton$child,col="blue",breaks=100)
     lines(c(mu, mu), c(0,150), col="red", lwd=5)
     mse <- mean((galton$child - mu)^2)
     text(63,150, paste("mu = ",mu))
     text(63,140, paste("MSE = ", round(mse,2)))
}
manipulate(myHist(mu), mu = slider(62,74, step=0.5))


plot(galton$parent,galton$child,pch=19,col="blue")



myHist <- function(mu){
     mse <- mean((galton$child - mu)^2)
     g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
     g <- g + geom_vline(xintercept = mu, size = 3)
     g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
     g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))


ggplot(galton, aes(x = parent, y = child)) + geom_point()


library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g



y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
     g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
     g <- g  + scale_size(range = c(2, 20), guide = "none" )
     g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
     g <- g + geom_point(aes(colour=freq, size = freq))
     g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
     g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
     mse <- mean( (y - beta * x) ^2 )
     g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
     g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))



lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)