
predt.95=read.csv("Income2019_Prediction95.csv")
predt.80=read.csv("Income2019_Prediction80.csv")
head(predt.95)
head(predt.80)
dev.off()
par <- par(mfrow = c(3, 2)) # set window for lots of plots

#par(mai=c(0.4,0.4,0.3,0.2))

# mai = c(bottom, left, top, right)
# las =1 rotates the axis values
#

## Total demand
## Correction1: PET-PEESE Model

plot(predt.80$income_19, predt.80$upper80_tcr1,type = 'n', ylim = c(-0.4, 0.80),
     xaxt='n',
      las=1, cex.axis=1.4, cex.lab=1.5, cex.main=1.5, cex=.9,
     ylab = 'Elasticity Estimate', xlab = '',
     main="A. PET-PEESE Model: Total Demand")

axis(side=1, at=axTicks(1), cex.axis=1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

polygon(c(predt.95$income_19, rev(predt.95$income_19)), 
        c(predt.95$upper95_tcr1,rev(predt.95$lower95_tcr1)),
        col = "gray88", border = NA)

lines(predt.95$income_19, predt.95$upper95_tcr1, col = 'grey',lwd=1)
lines(predt.95$income_19, predt.95$lower95_tcr1, col = 'grey', lwd=1)


###############################################################################
#Fill in the 80%CI
lines(predt.80$income_19, predt.80$upper80_tcr1, col = 'grey')
lines(predt.80$income_19, predt.80$lower80_tcr1, col = 'grey')

polygon(c(predt.80$income_19, rev(predt.80$income_19)), 
        c(predt.80$upper80_tcr1,rev(predt.80$lower80_tcr1)),
        col = "gray53", border = NA)

#the mean
#lines(predt.80$income_19, rep(0.396, times =23, col = 'grey30'), lty = 3)
# the predicted values
lines(predt.80$income_19, predt.80$predict_tcr1, col = 'black', lty = 1, lwd=2)

legend("bottomright", c("Light grey: 95% CI", "Dark grey: 80% CI"), bty="n", cex =1.3 )
legend("bottom", lty = c(1),lwd=c(2), c("Predicted value"), bty="n", pt.cex=1, cex=1.3)
                                        
#legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
#       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)


## Correction2: WAAP Model

plot(predt.80$income_19, predt.80$upper80_tcr2,type = 'n', ylim = c(-0.4, 0.80),
     las=1, cex.axis=1.4, cex.lab=1.5, cex.main=1.5, cex=.9, xaxt='n',
     ylab = 'Elasticity Estimate', xlab = '',
     main="B. WAAP Model: Total Demand")
axis(side=1, at=axTicks(1), cex.axis=1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

polygon(c(predt.95$income_19, rev(predt.95$income_19)), 
        c(predt.95$upper95_tcr2,rev(predt.95$lower95_tcr2)),
        col = "gray88", border = NA)

lines(predt.95$income_19, predt.95$upper95_tcr2, col = 'grey',lwd=1)
lines(predt.95$income_19, predt.95$lower95_tcr2, col = 'grey',lwd=1)

###############################################################################

#overlay the 80% CI
lines(predt.80$income_19, predt.80$upper80_tcr2, col = 'grey')
lines(predt.80$income_19, predt.80$lower80_tcr2, col = 'grey')

polygon(c(predt.80$income_19, rev(predt.80$income_19)), 
        c(predt.80$upper80_tcr2,rev(predt.80$lower80_tcr2)),
        col = "gray53", border = NA)
# the mean
#lines(predt.80$income_19, rep(0.396, times =23, col = 'grey30'), lty = 3) 
#the predicted values
lines(predt.80$income_19, predt.80$predict_tcr2, col = 'black', lty = 1,lwd=2)

legend("bottomright", c("Light grey: 95% CI", "Dark grey: 80% CI"), bty="n", cex =1.3 )
legend("bottom", lty = c(1),lwd=c(2), c("Predicted value"), bty="n", pt.cex=1, cex=1.3)



##Indoor demand
## Correction1: PET-PEESE Model 

plot(predt.80$income_19, predt.80$upper80_icr1,type = 'n', ylim = c(-0.4, 0.90),
     las=1, cex.axis=1.4, cex.lab=1.5, cex.main=1.5, cex=.9,xaxt='n',
     ylab = 'Elasticity Estimate', xlab = '',
     main="C. PET-PEESE Model: Indoor Demand")
axis(side=1, at=axTicks(1), cex.axis=1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))
polygon(c(predt.95$income_19, rev(predt.95$income_19)), 
        c(predt.95$upper95_icr1,rev(predt.95$lower95_icr1)),
        col = "gray88", border = NA)

lines(predt.95$income_19, predt.95$upper95_icr1, col = 'grey',lwd=1)
lines(predt.95$income_19, predt.95$lower95_icr1, col = 'grey', lwd=1)


###############################################################################
#Fill in the 80% CI
lines(predt.80$income_19, predt.80$upper80_icr1, col = 'grey')
lines(predt.80$income_19, predt.80$lower80_icr1, col = 'grey')

polygon(c(predt.80$income_19, rev(predt.80$income_19)), 
        c(predt.80$upper80_icr1,rev(predt.80$lower80_icr1)),
        col = "gray53", border = NA)

#lines(predt.80$income_19, rep(0.396, times =23, col = 'grey30'), lty = 3)
lines(predt.80$income_19, predt.80$predict_icr1, col = 'black', lty = 1, lwd=2)

legend("bottomright", c("Light grey: 95% CI", "Dark grey: 80% CI"), bty="n", cex =1.3 )
legend("bottom", lty = c(1),lwd=c(2), c("Predicted value"), bty="n", pt.cex=1, cex=1.3)

## Correction2: WAAP Model 


plot(predt.80$income_19, predt.80$upper80_icr2,type = 'n', ylim = c(-0.4, 0.80),
     las=1, cex.axis=1.4, cex.lab=1.5, cex.main=1.5, cex=.9,  xaxt='n',
     ylab = 'Elasticity Estimate', xlab = '',
     main="D. WAAP Model: Indoor Demand")
axis(side=1, at=axTicks(1), cex.axis=1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))
polygon(c(predt.95$income_19, rev(predt.95$income_19)), 
        c(predt.95$upper95_icr2,rev(predt.95$lower95_icr2)),
        col = "gray88", border = NA)
lines(predt.95$income_19, predt.95$upper95_icr2, col = 'grey',lwd=1)
lines(predt.95$income_19, predt.95$lower95_icr2, col = 'grey',lwd=1)

###############################################################################
#Overlay 80% CI
lines(predt.80$income_19, predt.80$upper80_icr2, col = 'grey')
lines(predt.80$income_19, predt.80$lower80_icr2, col = 'grey')

polygon(c(predt.80$income_19, rev(predt.80$income_19)), 
        c(predt.80$upper80_icr2,rev(predt.80$lower80_icr2)),
        col = "gray53", border = NA)
# the mean
#lines(predt.80$income_19, rep(0.396, times =23, col = 'grey30'), lty = 3)
# the predicted values
lines(predt.80$income_19, predt.80$predict_icr2, col = 'black', lty = 1,lwd=2)

legend("bottomright", c("Light grey: 95% CI", "Dark grey: 80% CI"), bty="n", cex =1.3 )
legend("bottom", lty = c(1),lwd=c(2), c("Predicted value"), bty="n", pt.cex=1, cex=1.3)

## For Outdoor demand
## Correction1: PET-PEESE Model 

plot(predt.80$income_19, predt.80$upper80_ocr1,type = 'n', ylim = c(-0.4, 0.80),
     las=1, cex.axis=1.4, cex.lab=1.5, cex.main=1.5, cex=.9,xaxt='n',
     ylab = 'Elasticity Estimate', xlab = "GDP Per Capita",
     main="E. PET-PEESE Model: Outdoor Demand")
axis(side=1, at=axTicks(1), cex.axis=1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

polygon(c(predt.95$income_19, rev(predt.95$income_19)), 
        c(predt.95$upper95_ocr1,rev(predt.95$lower95_ocr1)),
        col = "gray88", border = NA)
lines(predt.95$income_19, predt.95$upper95_ocr1, col = 'grey',lwd=1)
lines(predt.95$income_19, predt.95$lower95_ocr1, col = 'grey',lwd=1)



###############################################################################
# Fill in the 80% CI values

lines(predt.80$income_19, predt.80$upper80_ocr1, col = 'grey')
lines(predt.80$income_19, predt.80$lower80_ocr1, col = 'grey')

polygon(c(predt.80$income_19, rev(predt.80$income_19)), 
        c(predt.80$upper80_ocr1,rev(predt.80$lower80_ocr1)),
        col = "gray53", border = NA)

#the mean
#lines(predt.80$income_19, rep(0.396, times =23, col = 'grey30'), lty = 3)
# the predicted values
lines(predt.80$income_19, predt.80$predict_ocr1, col = 'black', lty = 1, lwd=2)

legend("bottomright", c("Light grey: 95% CI", "Dark grey: 80% CI"), bty="n", cex =1.3 )
legend("bottom", lty = c(1),lwd=c(2), c("Predicted value"), bty="n", pt.cex=1, cex=1.3)

## Correction2: WAAP Model 

plot(predt.80$income_19, predt.80$upper80_ocr2,type = 'n', ylim = c(-0.5, 0.80),
     las=1, cex.axis=1.4, cex.lab=1.5, cex.main=1.5, cex=.9,xaxt='n',
     ylab = 'Elasticity Estimate', xlab = 'GDP Per Capita',
     main="F. WAAP Model: Outdoor Demand")
axis(side=1, at=axTicks(1), cex.axis=1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))
polygon(c(predt.95$income_19, rev(predt.95$income_19)), 
        c(predt.95$upper95_ocr2,rev(predt.95$lower95_ocr2)),
        col = "gray88", border = NA)
lines(predt.95$income_19, predt.95$upper95_ocr2, col = 'grey', lwd=1)
lines(predt.95$income_19, predt.95$lower95_ocr2, col = 'grey',lwd=1)

###############################################################################
#Overlay 80% CI
lines(predt.80$income_19, predt.80$upper80_ocr2, col = 'grey')
lines(predt.80$income_19, predt.80$lower80_ocr2, col = 'grey')

polygon(c(predt.80$income_19, rev(predt.80$income_19)), 
        c(predt.80$upper80_ocr2,rev(predt.80$lower80_ocr2)),
        col = "gray53", border = NA)
# the mean
#lines(predt.80$income_19, rep(0.396, times =23, col = 'grey30'), lty = 3)
# the predicted values
lines(predt.80$income_19, predt.80$predict_ocr2, col = 'black', lty = 1,lwd=2)

legend("bottomright", c("Light grey: 95% CI", "Dark grey: 80% CI"), bty="n", cex =1.3 )
legend("bottom", lty = c(1),lwd=c(2), c("Predicted value"), bty="n", pt.cex=1, cex=1.3)

