install.packages("ISLR")
library(ISLR)

# Chapter 2 Lab: Introduction to R

# Basic Commands

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x + y
ls()
rm(x,y)
ls()

x <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
x
x <- matrix(c(1,2,3,4), 2, 2)
matrix(c(1,2,3,4), 2, 2,byrow = TRUE)
sqrt(x)
x^2

## simulation
# set.seed: Set the seed of R‘s random number generator, 
# which is useful for creating simulations or random objects that can be reproduced.
set.seed(1303)
rnorm(5)

set.seed(1303)
rnorm(5)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

## Graphics

x <- rnorm(50)
y <- x + rnorm(50, mean = 50,sd = .1)

plot(x, y)
plot(x, y, xlab = "this is the x-axis",
     ylab = "this is the y-axis",
     main = "Plot of X vs Y")
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

?seq
x <- 1:10
x
x <- seq(1, 10)
x


# Indexing Data

A <- matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)


# Additional Graphical and Numerical Summaries

x <- seq(-pi, pi,length = 50)
y <- x
f <- outer(x, y, function(x,y) cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa <- (f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)


plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders <- as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
summary(Auto)
summary(mpg)