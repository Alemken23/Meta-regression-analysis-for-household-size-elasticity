
library(robumeta)
library(metafor)
library(PublicationBias)
library(clubSandwich)
library(car)

## FUNNEL PLOT ##
mean.d.data <- read.csv("mean_hhe_updated.csv")
head(mean.d.data)

mra.d <-rma(ave_est,ave_se, data = mean.d.data)

png("Paper2Figure111.png", width = 150, height = 110, units = 'mm', res = 300)

resh3<-rma(ave_est,ave_se, data = mean.d.data)
funnel(resh3, yaxis ="sei",steps = 5,digits=1, back="white", shade="white",
       hlines="white", pch=20, pch.fill=20,col = "blueviolet", main="", 
       xlab = "Household-size Elasticity",ylim = c(0,1.8),xlim = c(-3,4),
       las=1,cex.axis=.8, cex.lab=.8, cex.main=.9,cex=.7,inset = .01)
abline(v=0, lty=1)
#abline(v=mean(mean.d.data$ave_est))


########
legend('topright',legend= c('Sample mean','Zero line'),lty = c(3,1),bty="n",cex=.8,inset = .01)
legend('topleft',legend= c('Studies'),pch=c(20),col = c("blueviolet"),bty="n", cex=.8,inset=.02)

dev.off()

par(mfrow=c(1,2))

##People, distribution plot of total estimates
HE_data=read.csv("HE_data_updated.csv")
hist(HE_data$est,main = "b) People Elasticity", xlab = "Elasticity Estimates",
     cex.axis=1.2, cex.lab=1.2, xlim = c(-1,4),ylim = c(0,450), breaks = 7,
     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)


## Triming

## People, distribution plot of total estimates

## This is fine but probably a bit distorted by the outliers. we can trim those...  you can also pool them but here I think it OK to trim..

#hist(HE_data$est,main = "a) People elasticity values", xlab = "Elasticity Estimates",
#     
#     cex.axis=1.2, cex.lab=1.2, ylim = c(1,80), xlim = c(-1,2),breaks = seq(-3.6,5,.2),
#     
#     cex.main=1.4, border = "black", col = "grey", las=1,freq=T)
#abline(v=mean(HE_data$est), lty=1,lwd=1.9) # can change to be whatever

#abline(v=median(HE_data$est), lty=2,lwd=1.9)  # can change to be whatever

#legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n" )
dev.off()

#png("Paper2Figure2.png", width = 450, height = 150, units = 'mm', res = 300)

png("Paper2Figure1.png", width = 200, height = 140, units = 'mm', res = 300)
#  exclude the bottom and top 1%

par(mfrow=c(1,2))
quantile(HE_data$est, c(.01,.99))



trim.datah <- subset(HE_data$est, HE_data$est > quantile(HE_data$est, c(.01)))   

trim.datah <- subset(HE_data$est, HE_data$est < quantile(HE_data$est, c(.99)))   



# use to inform setting the range: use sensible values based on these

max(trim.datah)

min(trim.datah)



hist(trim.datah,main = "a) HHS elasticity values", xlab = "Elasticity Estimates",
     
     cex.axis=.6, cex.lab=.7, ylim = c(1,35), xlim = c(-.5,2),breaks = seq(-3.,2,.095),
     
     cex.main=.7, border = "black", col = "grey", las=1,freq=T)



abline(v=mean(trim.datah), lty=1,lwd=1.5) # can change to be whatever

abline(v=median(trim.datah), lty=2,lwd=1.5)  # can change to be whatever

legend("topleft", legend = c("Mean", "Median"), lty=c(1,2), bty="n",cex=.56,inset =.008 )

###############################################################################
###############################################################################
### plotting the number of studies over time ###


HE.c = read.csv("cum_HE_updated.csv")
names(HE.c)
#barplot(IE.c$cum,names.arg=IE.c$year,xlab="Publication Year",
#       ylab="Number of Studies",col="grey",
#      main="",border="blue",ylim = c(0,180))

#barplot(cum~year, data = IE.c)

barplot(HE.c$cum,names.arg = HE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,80),
        main = "b) Growth in studies reporting HHS elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=0.56,cex.lab=0.6, cex.main=.7, cex = 0.45,
        las=1, space = 2,axis(1, at = round(seq(1970, 2021, 10))))



dev.off()

barplot(HE.c$cum,names.arg = HE.c$year,beside = TRUE, axes = TRUE,
        ylim=c(0,80),
        main = "b) Growth in studies reporting people elasticity",
        xlab = "Publication Year",ylab = "Number of Studies",
        cex.axis=0.56,cex.lab=0.6, cex.main=.7, cex = 0.45,
        las=1, space = 1,axis(1, at = round(seq(1970, 2021, 10))))


axis(1, xaxp = c(1970, 1990, 2020))


axis



axis(side=1,at=c(1967,2021))


