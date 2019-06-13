rm(list = ls())
library(mcr)
set.seed(123)
x <- seq(1:10)
w <- x + rnorm(length(x), 0, 1)
y <- 0.5 + 2*x 
z <- y + rnorm(length(x), 0, 1)

# Deming regression fit 
#regression coefficients are calculated
#  with analytical method
deming<- mcreg(w, z,error.ratio=1,method.reg="Deming", 
           method.ci="jackknife", mref.name="X",mtest.name="Y")

# output
printSummary(deming)
getCoefficients(deming)
graphics.off()
plot(deming, ylim = c(0, 30))
dev.copy(pdf, 'deming_Reg.pdf')
dev.off()

# residulas
graphics.off()
par(mfrow = c(1, 2))
plotResiduals(deming, res.type="optimized", xaxis="both" )
plotResiduals(deming, res.type="y", xaxis="yhat")


# The confidence intervals for regression coefficients
# are calculated with bootstrap (BCa) method
deming2<- mcreg(w, z,error.ratio=1,method.reg="Deming",
                method.ci="bootstrap", method.bootstrap.ci = "BCa",
                mref.name="X",mtest.name="Y")


# plot difference
par(mfrow = c(1, 3))
plotDifference( deming ) # Default plot.type=3
plotDifference( deming, plot.type=5)
plotDifference( deming, plot.type=7, ref.line.lty=3, ref.line.col="green3" )
dev.copy(pdf, 'deming_Reg_plot_difference1.pdf')
dev.off()

#### comparing jackknife to bootsrtap
compareFit( deming, deming2 )

####plot difference
par(mfrow = c(1, 3))
plotDifference( deming2 ) # Default plot.type=3
plotDifference( deming2, plot.type=5)
plotDifference( deming2, plot.type=7, ref.line.lty=3, ref.line.col="green3" )
dev.copy(pdf, 'deming_Reg_plot_difference2.pdf')
dev.off()


####
# plot bias
graphics.off()
# Grafical comparison of systematical Bias of two models
plotBias(deming, zeroline=TRUE,zeroline.col="black",zeroline.lty=1,
         ci.area=TRUE,ci.border=FALSE, ci.area.col=grey(0.9),
         main = " Systematic bias between X and Y",
    sub="Comparison of Jackknife and BCa-Bootstrap confidence interval ")

plotBias(deming2, ci.area=FALSE, ci.border=TRUE, ci.border.lwd=2,
         ci.border.col="red",bias=FALSE ,add=TRUE)
includeLegend(place="topleft",models=list(deming,deming2), lwd=c(10,2),
              lty=c(2,1),colors=c(grey(0.9),"red"), bias=TRUE,
              design="1", digits=4)
dev.copy(pdf, 'systematic_bias.pdf')
dev.off()


# Drawing of proportional bias
plotBias(deming, ci.area=TRUE, ci.border=TRUE)
plotBias(deming, ci.area=TRUE, ci.border=TRUE, prop=TRUE)
plotBias(deming, ci.area=TRUE, ci.border=TRUE, prop=TRUE, cut.point=0.6)
plotBias(deming, ci.area=TRUE, ci.border=TRUE, prop=TRUE, cut.point=0.6,
         xlim=c(0.4,0.8),cut.point.col="orange", cut.point.lwd=3, main ="")
dev.copy(pdf, 'proportional_bias.pdf')
dev.off()


## calculating bias
# calcBias( deming, x.levels = c(1,2,3))
# calcBias( deming, x.levels = c(1,2,3), type = "proportional")
# calcBias( deming, x.levels = c(1,2,3), type = "proportional", percent = FALSE)
