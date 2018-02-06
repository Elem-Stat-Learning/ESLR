library(MASS)
library(ISLR)

par(mfrow=c(1,1))
names(Boston)
plot(medv~lstat, Boston)
fit1 <- lm(medv~lstat, data=Boston)
fit1

summary(fit1)
abline(fit1, col="red")
names(fit1)


confint(fit1)

predict(fit1, data.frame(lstat=c(5,10,15)), interval="confidence")


fit2 <- lm(medv~lstat+age, data=Boston)
summary(fit2)

# fit all variables in dataset
fit3 <- lm(medv ~., data=Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

#remove variables age and indus from model
fit4 <- update(fit3, ~. -age-indus)

# interaction terms
fit5 <- lm(medv~lstat*age, Boston)
summary(fit5)

#Quadratic fit
fit6 <- lm(medv~lstat + I(lstat^2),Boston)
summary(fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat) 
points(lstat, fitted(fit6),col="red",pch=20)

#Easier way to fit polynomial terms
fit7 <- lm(medv~poly(lstat,4))
points(lstat, fitted(fit7),col="blue", pch=20 )

#plotting symbols available to us
plot(1:20, 1:20, pch=1:20, cex=2)

fix(Carseats)

names(Carseats)
summary(Carseats)
fit1 <- lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(fit1)

#shows coding of qualatitive variables in R
contrasts(Carseats$ShelveLoc)

# Writing a function 
# ... allows list of arguments
regplot <-  function(x,y,...){
  fit <- lm(y~x)
  plot(x,y,...)
  abline(fit, col="red")
}
attach(Carseats)
regplot(Price,Sales,xlab="Price",ylab="Sales")
