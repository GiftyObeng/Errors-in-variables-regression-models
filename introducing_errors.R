rm(list = ls())
library(latex2exp)
library(tidyverse)
library(ggplot2)

#################################################################
# monte carlo simulation
# step 1: predefine your model
x <- seq(1:10)                                 # independent variable
n = length(x)                                  # sample size
a <- 0.5                                       # intercept
b1 <- 2                                        # slope of the model
y_predefined <- a + b1*x                       # dependent variable (predefined
plot( resid(lm(y_predefined~x)) )                                             # model) 

#test <- lm(y_predefined~x)

## trying ggplots
y <- data.frame(y_predefined, x)
graphics.off()
xy_plot <- ggplot(  data= y, aes(x= x, y= y_predefined))   +
  geom_point(size=3)  +
  xlab("x (with no errors)")  +
  ylab(" y (with no errors) ") +
  stat_smooth(method="lm", mapping=aes(x= x, y= y_predefined),
              size=1, color="black") +
  ggtitle("True Model, y = 0.5 + 2x") +
  theme_classic() +
  theme( plot.margin=unit(x=c(1,1,1,1), units="lines"))

xy_plot
dev.copy(pdf, 'model.pdf', width = 8.0, height = 5.0)
dev.off()



#########################################################################
#########################################################################
# introducing error in the dependent variable, y
set.seed(1234)
sigma_y <- 1
mean_error_y <- 0
num_of_rep <- 1000
slope_error_y <- numeric(num_of_rep)        # empty list to contain 
#all the estimates of b1
intercept_error_y <- numeric(num_of_rep)        # empty list to contain 
#all the estimates of a
residual_error_y <- numeric(num_of_rep)


graphics.off()
par(mfrow= c(1, 2))
plot (x, y_predefined , xlab = 'x (with no errors)', ylab = 'y (with errors)',
      main=  expression(paste( 'Fitted Regression lines with an error in y')), 
      ylim=c(-3, 25), xlim=c(0,10))


for (i in 1:num_of_rep) {
  y_with_error = y_predefined + rnorm(n, mean_error_y, sigma_y)
  zlm = lm (y_with_error ~ x)
  abline(zlm, col(blue))
  residual_error_y[[i]] <- as.vector(summary(zlm)$sigma)
  slope_error_y[[i]] = as.vector( zlm$coefficients[2])
  intercept_error_y[[i]] = as.vector( zlm$coefficients[1])
}

curve( 0.5 + 2*x, col = 'red'  , add = TRUE )    # regression line
curve( mean(intercept_error_y) + mean(slope_error_y)*x, col = 'green' ,lwd=3, lty=2 , add = TRUE )    # regression line
legend("topleft", legend= c('True line', 'error in y'),
       col=c("red", "green"),
       lty=1:2, cex=0.45)


#####residuals
hist(  residual_error_y, xlab = 'Residuals', ylab = 'Frequency',
       main = 'Residual Plot')
#xlab="Residuals", ylab="Frequency", 
   #                         main=" Residuals of the 1000 fitted lines")
# z = mean(intercept_error_y) + mean(slope_error_y)*x
# plot(x, resid(lm(z~x)),  ylab="Residuals", xlab="Observations", 
#      main=" Residuals of y=0.517366  + 1.997955 x") 
# abline(0, 0)                  # the horizon
dev.copy(pdf, 'r.pdf', width = 9.0, height = 5.0)
dev.off()
###### scatter plots and density plots of the estimates
graphics.off()
par(mfrow = c(2, 2))

plot(density(intercept_error_y), ylab = 'Frequency' , xlab = 'intercept estimates',
     main = TeX(sprintf("$E(\\hat{\\beta}_0)=$%f",
                        mean(intercept_error_y)))  )

abline(v = a, col="blue") 
abline(v = mean(intercept_error_y), col="red",lwd=1, lty=2 )



plot(density(slope_error_y), ylab = 'Frequency' , xlab = 'slope estimates',
     main = TeX(sprintf("$E(\\hat{\\beta}_1)=$%f",
                        mean(slope_error_y)))  )
abline(v = b1, col="blue") 
abline(v = mean(slope_error_y), col="red", lwd=1, lty=2)

scatter.smooth(intercept_error_y, xlab = 'iterations', ylab =
                 'intercept estimates')
scatter.smooth(slope_error_y, xlab = 'iterations', ylab = 
                  'slope estimates')
dev.copy(pdf, 'y_error.pdf')
dev.off() 

# legend(1, 95, legend=c(TeX(sprintf("$E(\\hat{\\beta}_0)=$%f",
#                                    mean(intercept_error_y))),
#                        TeX(sprintf("$E(\\hat{\\beta}_0)=$%f",
#                                    a ))),
#        col=c("red", "blue"), lty=1:2, cex=0.8,
#        box.lty=2)

sd(slope_error_y)/sqrt( num_of_rep )


##########################################################################
#########################################################################
############################################################################
# introducing error in the dependent variable, x
set.seed(1243)
x <- seq(1:10)
a <- 0.5                                       # intercept
b1 <- 2                                        # slope of the model
y_predefined <- a + b1*x 
sigma_x <- 1
mean_error_x <- 0
num_of_rep <- 1000
slope_error_x <- numeric(num_of_rep)        # empty list to contain all the estimates of 
# the slope, b1
intercept_error_x <- numeric(num_of_rep)        # empty list to contain all the estimates of 
# the intercept, a
residual_error_x <- numeric(num_of_rep)
graphics.off()
par(mfrow = c(1, 2))
plot (x, y_predefined, xlab = 'x (with errors)', ylab = 'y (with no errors)',
      main=  expression(paste( 'Fitted regression line with an error in x')), 
      ylim=c(-3, 25), xlim=c(0,10))

for (i in 1:num_of_rep) {
  x_with_error = x + rnorm(n, mean_error_x, sigma_x)
  wlm = lm (y_predefined ~ x_with_error)
  abline(wlm)
  residual_error_x[[i]] <- as.vector(summary(wlm)$sigma)
  slope_error_x[[i]] = as.vector( wlm$coefficients[2])
  intercept_error_x[[i]] = as.vector( wlm$coefficients[1])
}
x <- seq(1:10)
x <-  x + rnorm(n, mean_error_x, sigma_x)
curve( 0.5 + 2*x, col = 'red'  , add = TRUE )    # regression line
curve( mean(intercept_error_x) + mean(slope_error_x)*x, col = 'green'  , add = TRUE )    # regression line
legend("topleft", legend=c('True line', 'error in x'),
       col=c("red", "green"),
       lty=1, cex=0.45)

####essay phase
#####residuals
#y <- mean(intercept_error_x) + mean(slope_error_x)*x
#plot(x, resid(lm(y~x)),  ylab="Residuals", xlab="Observations", 
#     main=" Residuals of y= 1.391569 + 1.834374x ") 
hist(residual_error_x , xlab = 'Residuals', ylab = 'Frequency',
     main = 'Residual Plot')
#xlab="Residuals", main=" Residuals")
dev.copy(pdf, 'xerror.pdf', width = 9.0, height = 5.0)
dev.off() 


##### scatter plots of the slope and intercept
graphics.off()
par(mfrow = c(2, 2))

#### density function of the slope and intercept
plot(density(intercept_error_x), ylab = 'Frequency' , xlab = 'intercept estimates',
     main = TeX(sprintf("$E(\\hat{\\beta}_0)=$%f", mean(intercept_error_x))) )
abline(v = a, col="blue")
abline(v = mean(intercept_error_x), col="red", lwd=1, lty=2)

plot(density(slope_error_x), ylab = 'Frequency' , xlab = 'slope estimates',
     main = TeX(sprintf("$E(\\hat{\\beta}_i )=$%f", mean(slope_error_x))) )
abline(v = b1, col="blue")  
abline(v = mean(slope_error_x), col="red", lwd=1, lty=2)


##scatter plot
scatter.smooth(intercept_error_x , xlab = 'iterations',
               ylab = 'intercept estimates')
scatter.smooth(slope_error_x, xlab = 'iterations', ylab = 'slope estimates')
dev.copy(pdf, 'x_error.pdf')
dev.off() 


#legend('center', legend= TeX(sprintf("$\\hat{\\beta}_1(x, y)=$%f"
#                           , b1 ,col= "blue",
#                            lty=1 ,cex=0.8) ) )

mean(slope_error_x)
mean(intercept_error_x)
sd(slope_error_x)/sqrt( num_of_rep )



###########################################################################
###########################################################################
############################################################################
# introducing error in the dependent variable, y and x
par(mar=c(1,1,1,1))
par(mar = c(0.2, 0.8, 0.5 ,0.5))
set.seed(1242)
sigma_x <- 1
sigma_y <- 1
mean_error_x <- 0
mean_error_y <- 0
num_of_rep <- 1000
slope_error_xy <- numeric(num_of_rep) # empty list to contain all the estimates of 
# the slope, b1
intercept_error_xy <- numeric(num_of_rep) # empty list to contain all the estimates of 
# the intercept, a
residual_error_xy <- numeric(num_of_rep)
x <- seq(1:10)
a <- 0.5                                       # intercept
b1 <- 2                                        # slope of the model
y_predefined <- a + b1*x  
graphics.off()
par(mfrow = c(2,2))
#par(mar = c(0.2831346, 0.8549798, 0.5965588 ,0.5204135))
plot (x, y_predefined, xlab = 'x (with errors)', ylab = 'y (with errors)',
      main= 'Fitted of regression line with an error in y and x and x')

for (i in 1:num_of_rep) {
  x_with_error = x + rnorm(n, mean_error_x, sigma_x)
  y_with_error = y_predefined + rnorm(n, mean_error_y, sigma_y)
  wzlm = lm (y_with_error ~ x_with_error)
  abline(wzlm)
  residual_error_xy[[i]] <- as.vector(summary(wzlm)$sigma)
  intercept_error_xy[[i]] = as.vector( wzlm$coefficients[1])
  slope_error_xy[[i]] = as.vector( wzlm$coefficients[2])
}

x <- seq(1:10)
x <-  x + rnorm(n, mean_error_x, sigma_x)
y <-  y_predefined + rnorm(n, mean_error_y, sigma_x)
curve( 0.5 + 2*x, col = 'red'  , add = TRUE )    # regression line
curve( mean(intercept_error_xy) + mean(slope_error_xy)*x, col = 'green'  , add = TRUE )    # regression line
legend("topleft", legend=c('True line', 'error in x and y'),
       col=c("red", "green"),
       lty=1, cex=0.3)
hist(residual_error_xy)

#dev.copy(pdf, 'xy_error_residuals_line.pdf', width = 9.0, height = 5.0)
#dev.off()


# ### residuals
# y <- mean(intercept_error_xy) + mean(slope_error_xy)*x
# plot(x, resid(lm(y~x)),  ylab="Residuals", xlab="Observations", 
#      main=" Residuals of y= 1.457827 + 1.831444x ") 
# abline(0, 0)


##### scatter plots of the slope and intercept
#graphics.off()
#par(mfrow = c(2, 2))

plot(density(intercept_error_xy), ylab = 'Frequency' , xlab = 'intercept estimates',
     main = TeX(sprintf("$E(\\hat{\\beta}_0)=$%f", mean(intercept_error_xy))) )
abline(v = a, col="blue")
abline(v = mean(intercept_error_xy), col="red", lwd=1, lty=2)

plot(density(slope_error_xy), ylab = 'Frequency' , xlab = 'slope estimates',
     main = TeX(sprintf("$E(\\hat{\\beta}_1)=$%f", mean(slope_error_xy))) )
abline(v = b1, col="blue")  
abline(v = mean(slope_error_xy), col="red", lwd=1, lty=2)


scatter.smooth(intercept_error_xy,  xlab = 'iterations', ylab = 
                 'slope estimates' )
scatter.smooth(slope_error_xy,  xlab = 'iterations', ylab = 
                 'slope estimates')


dev.copy(pdf, 'xy_all.pdf')
dev.off()
#legend('center', legend= TeX(sprintf("$\\hat{\\beta}_1(x, y)=$%f"
#                           , b1 ,col= "blue",
#                            lty=1 ,cex=0.8) ) )

mean(slope_error_xy)
mean(intercept_error_xy)
sd(slope_error_xy)/sqrt( num_of_rep )

