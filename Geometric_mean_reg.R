rm(list = ls())

set.seed(123)
stepsize = 1
x <- seq(1,10, stepsize)
w <- x + rnorm(length(x), 0, 1)
y <- 0.5 + 2*x 
z <- y + rnorm(length(x), 0, 1)


library(smatr)
# Source: https://influentialpoints.com/notes/ncrmd1a.htm
line.cis(z, w,method='OLS')

#Fit reduced major axis line
line.cis(z, w)
slope.test(z, w)

#correlation test
a=z-w
b=z+w
cor.test (a,b,method = "pearson")


graphics.off()
plot(w, z, main = 'Geometric Mean Regression', ylim = c(-1, 25))
abline(lm(w~z), col = 'red', lwd = 2)
abline(lm(z~w), col = 'green', lwd = 2)
curve( 0.7667934 +1.9627916*x , add= TRUE , col = 'black', lwd = 2)
legend("topleft", legend= c('w on z', 'z on w', 'GMR'),
       col=c("red", "green", 'black'),
       lty=1:3, cex=0.8)
dev.copy(pdf, 'GMR_10.pdf', width = 8, height = 5)
dev.off()


#Manual computation
#Source http://bsmith.mathstat.dal.ca/stat3340/Rcode/simpleR.txt
Sww=sum((w-mean(w))^2)
Swz=sum((w-mean(w))*(z-mean(z)))
Szz=sum((z-mean(z))^2)

b1 <- sign(Swz)*sqrt(Szz/Sww)

b0=mean(z)-b1*mean(w)


#get predicted values, the residuals
yp=b0+b1*w
resids=z-yp
SSE=sum(resids^2)  #residual SS
SST=Szz           #total SS
SSR=SST-SSE        #regression SS
SS=c(SSR,SSE,SST)  #print the SS
n=length(z)        #how many data points
df=c(1,n-2,n-1)    #regression, error and total degrees of freedom
MS=SS/df           #calculate the mean squares
cbind(SS,df,MS)    #joint the SS, df and MS columns side by side

cat("Fobs = ", MS[1]/MS[2], "with 1 numerator and ",n-2, " denominator degrees of freedom", sep=" ")


#calculate the 95% confidence interval for the slope

MSE=SSE/(n-2)      #MSE
#upper .025'th percentile of a t with n-2 df.
t=qt(.975,n-2)  
t

SEb1=sqrt(MSE/Sww) #standard error of beta_1
SEb1
c(b1-t*SEb1,b1+t*SEb1) #report the conidence interval

#calculate a 95% CI for the mean of Y when x=x0

(sqrt(sum(w*w)/(length(w) * sum( (w - mean(w))^2 )  )))

