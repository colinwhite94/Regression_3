################################
## R Code for Climate Dataset ##
################################

## Load libraries and data
setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW3")

library(MASS)
climate <- read.table("rain.txt",header=TRUE)
n = nrow(climate)

plot(climate$Precip,climate$Runoff,pch=19)

## Fit a Linear model
climate.lm <- lm(Runoff~Precip,data=climate)
summary(climate.lm)
head(climate)

## Plot the Fitted Regression Line
plot(climate$Precip,climate$Runoff,xlab="Precipitation",pch=19,ylab="Runoff")
abline(a=climate.lm$coef[1],b=climate.lm$coef[2],lwd=3,col="red")

###

plot(climate.lm$fitted.values,climate.lm$residuals,pch=19, ylab="Standardized Residuals",xlab="Fitted Values")
abline(0,0,col = "red",lwd = 2,lty = 2)


hist(stdres(climate.lm),freq = FALSE,breaks = 20, main = "Histogram of Residuals", xlab="Standardized Residuals")
curve(dnorm,from = -3,to = 3,col = "cornflowerblue",lwd = 2,
      lty = 2,add = TRUE)

qqnorm(stdres(climate.lm))
abline(0,1,col = "red",lwd =2,lty = 2)

#acf(climate$Runoff)
#acf(climate.lm$residuals)

#confint(climate.lm,level=0.95,parm="precip")


confint(climate.lm)

## Conduct tests of hypothesis on slope and intercept
summary(climate.lm) ## Two-sided
t.stat <- (climate.lm$coef[2]-0)/summary(climate.lm)$coef[2,2]
p.value <- 2 * (1 - pt(t.stat,df=nrow(climate)-2))

t.stat
p.value


## Calculate confidence interval for mean
pred.conf <- predict.lm(climate.lm,newdata=data.frame(Precip=4.5),
                   interval="confidence",level=0.95)

pred.conf

## Calculate prediction intervals  - one observation
pred.pred <- predict.lm(climate.lm,newdata=data.frame(Precip=4.5),
                   interval="prediction",level=0.95)
pred.pred
## Calculate prediction intervals  - for a lot of possible values

all.preds <- predict.lm(climate.lm,
                        newdata=data.frame(Precip=seq(2,30,length=100)),
                        interval="prediction",level=0.95)
head(cbind(seq(2,30,length=100),all.preds))

all.conf <- predict.lm(climate.lm,
                        newdata=data.frame(Precip=seq(2,30,length=100)),
                        interval="confidence",level=0.95)

plot(climate$Precip,climate$Runoff,pch=19)

abline(a=climate.lm$coef[1],b=climate.lm$coef[2],
       col="red",lwd=3)
lines(seq(2,30,length=100),all.preds[,2],
      col="green",lty=2) ##Plot Lower bound
lines(seq(2,30,length=100),all.preds[,3],
      col="green",lty=2) ##Plot Upper bound

lines(seq(2,30,length=100),all.conf[,2],
      col="blue",lty=2) ##Plot Lower bound
lines(seq(2,30,length=100),all.conf[,3],
      col="blue",lty=2) ##Plot Upper bound

## Cross Validation Excercise (calculate coverage)

n.test <- round(0.2 * n)
cv.reps = 1000

coverage = numeric(cv.reps)
width = numeric(cv.reps)
PRMSE = numeric(cv.reps)
bias = numeric(cv.reps)

for(i in 1:cv.reps){
  
  test.obs <- sample(1:n,n.test)
  test.data <- climate[test.obs,]
  train.data <- climate[-test.obs,]
  train.lm <- lm(Runoff~Precip,data=train.data)
  test.preds <- predict.lm(train.lm,newdata=test.data,
                           interval="prediction",level = 0.95)
  coverage[i] <- mean(test.data$Runoff > test.preds[,2] & 
                        test.data$Runoff < test.preds[,3])
  width[i] = mean(test.preds[,3] - test.preds[,2])
  bias[i] = mean(test.preds[,1] - test.data$Runoff)
  PRMSE[i] = sqrt(mean((test.preds[,1] - test.data$Runoff)^2))
  
}

mean(coverage)   ### compare with 0.95
mean(width)
mean(bias)       
mean(PRMSE)

range(climate$Runoff)
sd(climate$Runoff)




## Fit a centered LM
Precip.cntr <- climate$Precip-mean(climate$Precip)

## the scale function (by default) subtracts off the mean and divides by standard deviation
Precip.cntr2 <- scale(climate$Precip,scale = FALSE)  

climate.lm.cntr <- lm(Runoff~Precip.cntr,data=climate)
summary(climate.lm.cntr)
confint(climate.lm.cntr)

## Predict using the centered model
climate.lm.cntr$coef[1]+climate.lm.cntr$coef[2]*(360-mean(climate$Precip))


### original model
predict.lm(climate.lm,newdata=data.frame(Precip=360))




