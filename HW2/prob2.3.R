#Using lm(method)

x <- 1:20
y <- x+rnorm(20)

fit <- lm(y~x+ I(x^2))

plot(x,y, main = 'Data fit')
abline(fit)

coef(fit)


#Using the "direct" method 

xtxi <- solve(t(x) %*% x)

xtxi %*% t(x) %*% y

#this method fails 

solve(crossprod(x,x),crossprod(x,y))

# this model also fails. 
#the two direct methods for computing the slope both fail due the predictor and
# response are strongly correlated.

cor(x,y)

# They have a correlation coefficent of 0.98
