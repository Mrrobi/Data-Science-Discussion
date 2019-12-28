#Simple linear regression using R
#############################################################

##### Set working direction
setwd("~/Documents/Courses_Fall2016/STA206_TA/Lab2")

##### Read in data
copier = read.table("Copier.txt", header=FALSE)

copier
class(copier)
summary(copier)

##### Fitting
fit = lm(V1~V2, data=copier)
summary(fit)

MSE = 8.914^2
15.0352 - qt(0.975,43) * 0.4831  # lower bound of 95% confidence interval for beta_1
15.0352 + qt(0.975,43) * 0.4831  # upper bound of 95% confidence interval for beta_1

coef(fit)
fit$coefficients

residuals(fit)
fit$residuals

fitted(fit)	
fit$fitted

fitted(fit)[5]	# fifth fitted value
residuals(fit)[5]	# fifth residual

anova(fit)

##### Plotting
par(mfrow=c(2,2))  #plot 2*2 figures in one panel

plot(copier$V2, copier$V1, xlab='# of copiers', ylab='# of minutes', main='Data and fitted line')	
abline(fit, col='red')	#adding the fitted line (red)

plot(fit, which=1)	# plot the residuals against the fitted value
plot(fit, which=2)	# plot the normal-qq plot of the residuals

##### Finding Prediction intervals
newX=data.frame(V2=5)
# interval specifies the type of intervals you want to obtain
# level specifies the confidence level
predict(fit, newX, interval='confidence', level=0.95) 
predict(fit, newX, interval='confidence', level=0.99)
predict(fit, newX, interval='prediction', level=0.95) 
predict(fit, newX, interval='prediction', level=0.99)

##### Box-Cox Transformation
sales = read.table("Sales.txt", header=FALSE)

sales
fit_sales = lm(V1~V2, data=sales)

par(mfrow=c(2,2))
plot(sales$V2, sales$V1, xlab='Year', ylab='Sales', main='Scatter plot of the data')	
abline(fit_sales, col='red')
plot(fit_sales, which=1)
plot(fit_sales, which=2)

library(MASS)
par(mfrow=c(1,1))
boxcox(V1~V2, data=sales)

newfit_sales = lm(V1^(1/2)~V2, data=sales)

summary(fit_sales)
summary(newfit_sales)

par(mfrow=c(2,2))
plot(sales$V2, (sales$V1)^(1/2), xlab='Year', ylab='Sqrt of Sales', main='Scatter plot of transformed data')	
abline(newfit_sales, col='red')
plot(newfit_sales, which=1)
plot(newfit_sales, which=2)

##### Save the Results
save(fit, file="copier.RData")
load("copier.RData")
save.image(file="copier.RData")

##### More about R plotting
### Plot 1
# Define the cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Graph cars using blue points overlayed by a line
par(mfrow=c(1,1))
plot(cars, type="o", col="blue")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)

### Plot 2
# Define 2 vectors
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)

# Graph cars using a y axis that ranges from 0 to 12
plot(cars, type="o", col="blue", ylim=c(0, 12))

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)

### Plot 3
# Define 2 vectors
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)

# Calculate range from 0 to max value of cars and trucks
g_range <- range(0, cars, trucks)

# Graph autos using y axis that ranges from 0 to max 
# value in cars or trucks vector.  Turn off axes and 
# annotations (axis labels) so we can specify them ourself
plot(cars, type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE)

# Make x axis using Mon-Fri labels
axis(1, at=1:5, lab=c("Mon","Tue","Wed","Thu","Fri"))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, las=1, at=4*0:g_range[2])

# Create box around plot
box()

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font, and slightly larger
title(main="Autos", col.main="red", font.main=4, cex.main=1.5)

# Label the x and y axes with dark green text
title(xlab="Days", col.lab=rgb(0,0.5,0))
title(ylab="Total", col.lab=rgb(0,0.5,0))

# Create a legend at topleft that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots 
legend("topleft", c("cars","trucks"), cex=0.8, 
       col=c("blue","red"), pch=c(21,22), lty=c(1,2))