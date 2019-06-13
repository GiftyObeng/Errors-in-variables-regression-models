rm(list = ls())
library(mcr)
set.seed(123)
x <- seq(1,10)
w <- x + rnorm(length(x), 0, 1)
y <- 0.5 + 2*x 
z <- y + rnorm(length(x), 0, 1)


# Passing-Bablok Regression
######################################################################
######################################################################
Passing <- mcreg(w, z,method.reg="PaBa",
                    #method.ci="analytical", slope.measure="radian",
                    mref.name="X",mtest.name="Y")
plot(Passing, add.legend=TRUE,identity=TRUE, main=
       "Passing-Bablok regression",
     ci.area=TRUE,add.cor=TRUE, ylim = c(-5, 40))
dev.copy(pdf, 'passing-bablok.pdf')
dev.off()

# Passing2 <- mcreg(w, z,method.reg="PaBa",method.ci="analytical",
#                   slope.measure="tangent",
#                   mref.name="X",mtest.name="Y")

# plot(Passing2, ci.area=FALSE,reg.col="darkgreen",reg.lty=2,identity=
#            FALSE,add.legend=FALSE,
#      draw.points=FALSE,add=TRUE,add.cor=FALSE)
# abline(lm(y~x))
# includeLegend(place="topleft",models=list(Passing,Passing2),model.names
#                  =c("PaBa Radian","PaBa Tangent"),
#               colors=c("darkblue","darkgreen"),lty=c(1,2),design="1",
#                 digits=2)
