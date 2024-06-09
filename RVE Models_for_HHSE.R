
library(robumeta)
library(metafor)
library(PublicationBias)
library(clubSandwich)
library(car)
library(carData)
library(tidyverse)

###################### SOLVING FOR 2021 REAL INCOME DATA ##########################
#### I used this model for the paper ####
hem <- read.csv('HE_data_updated.csv',header=T,sep=",")
head(hem)

head(hem)

endogh<-relevel(factor(hem$endoge),ref = "yes")

formh<-relevel(factor(hem$form),ref ="Double log")

data_typeh<-relevel(factor(hem$data_type),ref = "Panel")

a_s_lh<-relevel(factor(hem$a_s_l),ref = "A")

economyh<-relevel(factor(hem$economy),ref = "high")

tot_in_outh<-relevel(factor(hem$tot_in_out),ref = "T")

incomeh<-relevel(factor(hem$income),ref="no")

climateh<-relevel(factor(hem$rain_tem),ref = "t")

scaleh<-relevel(factor(hem$scale),ref = "macro")

data_collectionh<-relevel(factor(hem$data_collection),ref = "annual")

publicationh<-relevel(factor(hem$publication_type),ref = "grey")

consh<-relevel(factor(hem$consumption),ref = "total")

tarifh<-relevel(factor(hem$tariffadj),ref = "increasing")
summary(as.factor(hem$tariffadj))



#hhs_2022

head(hem)

RVEH5.1 <- robu(formula=est~tot_in_outh+climateh+tarifh+incomeh+
                  endogh+log(income_22)+log(hhs_2022),
              data=hem,studynum = study_id,var.eff.size =var,
              modelweights ="CORR",rho = .8, small = FALSE)

print(RVEH5.1)

#####################

RVEH5.2 <- robu(formula=est~tot_in_outh+climateh+tarifh+incomeh+
                  endogh+log(income_22),
                data=hem,studynum = study_id,var.eff.size =var,
                modelweights ="CORR",rho = .8, small = FALSE)

print(RVEH5.2)

### Model Shrinking ###


RVEH5 <- robu(formula=est~tot_in_outh+climateh+tarifh+incomeh+
                +endogh+log(income_19),
                data=hem,studynum = study_id,var.eff.size =var,
                modelweights ="CORR",rho = .8, small = FALSE)

print(RVEH5)

##Testing the climate variable: non-no-climate data are equal
print(RVEH5)


## Testing with the full model--RVEH1  

print(RVEH5.1)
CD <- rbind(c(0,0,0,-1,1,0,0,0,0,0,0,0,0)   #beta 4 =beta 5
            +c(0,0,0,0,-1,1,0,0,0,0,0,0,0))  #beta 5 =beta 6

Wald_test(RVEH5, constraints = CD, vcov="CR2")

#conclude: found strong evidence for do not rejecting the null that the coefficients are equal
#Test    F      d.f.    p.val
#HTZ    0.123  33.4   0.728

#  create new CD variable for the temperature data included in the model
hem$CD <- ifelse(hem$rain_tem == "t", 0, 1) ##temperature data is 0 the rest is 1
#  Check it has been added
summary(hem$CD)

RVEH5.test1 <- robu(formula=est~tot_in_outh+CD+tarifh+incomeh+log(income_19)+endogh,
                    data=hem,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small = FALSE)
print(RVEH5.test1)

#the model has an effect for temperature data and everything else as a group

## checking the tariff structure

print(RVEH5)
TSH <-rbind(c(0,0,0,0,0,0,-1,1,0,0,0,0,0) #beta 7 and beta 8
            +c(0,0,0,0,0,0,0,-1,1,0,0,0,0))   #beta8 =beta 9
Wald_test(RVEH5, constraints = TSH, vcov="CR2")
# we cannot reject the null that the coefficents are equal
#Test    F    d.f.  p.val
#HTZ    1.66  1.62  0.351

#  create new TS variable for the increasing block
hem$TS <- ifelse(hem$tariffadj == "increasing", 0, 1)

summary(as.factor(hem$tariffadj))

#  Check it has been added
summary(hem$TS)                     


RVEH5.test2 <- robu(formula=est~tot_in_outh+climateh+TS+incomeh+log(income_19)+endogh,
                    data=hem,studynum = study_id,var.eff.size =var,
                    modelweights ="CORR",rho = .8, small = FALSE)
print(RVEH5.test2)

##the model has no an effect for increasing tarif and everything else as a group

#Checking the income data information

print(RVEH5)
NIDH <-rbind(c(0,0,0,0,0,0,0,0,0,-1,1,0,0)) #beta 10 and beta 11

Wald_test(RVEH5, constraints = NIDH, vcov="CR2")
# we cannot reject the null that the coefficients are equal
#Test    F   d.f.  p.val
#HTZ   0.681 10.5  0.428

#  create new NID variable for the "no income data included in the model"
hem$NID <- ifelse(hem$income == "no", 0, 1) # no income data included is the reference

summary(as.factor(hem$income))

# Check it has been added
summary(hem$NID)                     

#run the model
RVEH5.test3 <- robu(formula=est~tot_in_outh+climateh+tarifh+NID+log(income_19)+endogh,
                    data=hem,studynum = study_id,var.eff.size =var,
                    modelweights ="CORR",rho = .8, small = FALSE)
print(RVEH5.test3)

##the model has an effect for no income data and everything else as a group

#tot_in_outh<-relevel(hem$tot_in_out,ref = "T")
#incomec2<-relevel(hem$income,ref="proxy_income")

##pooling the shrinking selection with log GDP per capita income
head(hem)

RVE1.shrink<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+log(income_19),
                   data=hem,studynum = study_id,var.eff.size =var,
                    modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.shrink)

#Publication bias test using SE
RVE1.test<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+log(income_19)+se,
                   data=hem,studynum = study_id,var.eff.size =var,
                   modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.test)

#Publication bias correction using Var
RVE1.corect1<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+log(income_19)+I(se^2),
                 data=hem,studynum = study_id,var.eff.size =var,
                 modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.corect1)

###############################################################################
##############################################################################
### Addressing reviewers' comments 
#The type of data used
RVE1.datatype<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+data_typeh+
                       log(income_19),data=hem,studynum = study_id,var.eff.size =var,
                       modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.datatype)


#Data aggregation--Data scale 
RVE1.dataaggr<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+scaleh+
                       log(income_19),data=hem,studynum = study_id,var.eff.size =var,
                     modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.dataaggr)

summary(as.factor(hem$studyconductedu))
#Data collection frequency 
RVE1.datafreq<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+data_collectionh+
                       log(income_19),data=hem,studynum = study_id,var.eff.size =var,
                     modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.datafreq)

##The average household size
RVE1.HHS<- robu(formula=est~tot_in_outh+CD+tarifh+NID+endogh+log(hhs_2022)+
                       log(income_19),data=hem,studynum = study_id,var.eff.size =var,
                     modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.HHS)

###############################################################################
#Create dummies for the rest of the factor
#install.packages("fastDummies")
library('fastDummies')

# Type of use
hemd <- dummy_cols(hem, select_columns = 'tot_in_out')

head(hemd)

#Tariff

hemd <- dummy_cols(hemd, select_columns = 'tariffadj')
head(hemd)

#  Check it has been added
summary(hemd$tarifh)                     

#endogeneity
hemd <- dummy_cols(hemd, select_columns = 'endoge')

head(hemd)

# getting the data type right--make them all numeric

hemd$tot_in_out_I= as.numeric(hemd$tot_in_out_I)
is.numeric(hemd$tot_in_out_I)
hemd$tot_in_out_O= as.numeric(hemd$tot_in_out_O)
is.numeric(hemd$tot_in_out_O)
hemd$tot_in_out_T=as.numeric(hemd$tot_in_out_T)
is.numeric(hemd$tot_in_out_T)


hemd$CD=as.numeric(hemd$CD)
is.numeric(hemd$CD)

hemd$tariffadj_decreasing= as.numeric(hemd$tariffadj_decreasing)
is.numeric(hemd$tariffadj_decreasing)
hemd$tariffadj_increasing=as.numeric(hemd$tariffadj_increasing)
is.numeric(hemd$tariffadj_increasing)
hemd$tariffadj_others= as.numeric(hemd$tariffadj_others)
is.numeric(hemd$tariffadj_others)
hemd$tariffadj_uniform= as.numeric(hemd$tariffadj_uniform)
is.numeric(hemd$tariffadj_uniform)

hemd$NID= as.numeric(hemd$NID)
is.numeric(hemd$NID)

hemd$endoge_no= as.numeric(hemd$endoge_no)
is.numeric(hemd$endoge_no)
hemd$endoge_yes= as.numeric(hemd$endoge_yes)
is.numeric(hemd$endoge_yes)

hemd$income_19= as.numeric(hemd$income_19)
is.numeric(hemd$income_19)

hemd$est= as.numeric(hemd$est)
is.numeric(hemd$est)
summary(hemd$est)

colnames(hemd)
Nhemd<-hemd %>% 
  select(study,study_id,invers_noestmate,no_estimates,estimate_id,invers_noestmate,hhs_2019,
         hhs_2022,income_19,income_22,est,se,invers_se,var,invers_var,CD,NID,tot_in_out_I,
         tot_in_out_O,tot_in_out_T,tariffadj_decreasing,tariffadj_increasing,tariffadj_others,
         tariffadj_uniform,endoge_no,endoge_yes)

Nhemd = as.data.frame(sapply(Nhemd, as.numeric,na.omit=T))
head(Nhemd)


#the base model
RVE1.rawmodel<- robu(formula=est~tot_in_out_I+tot_in_out_O+CD+tariffadj_decreasing+
                                  tariffadj_uniform+tariffadj_others+NID+endoge_no+
                                  log(income_19),data=Nhemd,studynum = study_id,
                                  var.eff.size =var,modelweights ="CORR",rho = .8,
                                  small = FALSE)
print(RVE1.rawmodel)


## Checking the bias

RVE1.bias<- robu(formula=est~tot_in_out_I+tot_in_out_O+CD+tariffadj_decreasing+
                   tariffadj_uniform+tariffadj_others+NID+endoge_no+log(income_19)+se,
                  data=Nhemd,studynum = study_id,var.eff.size =var,
                  modelweights ="CORR",rho = .8, small = FALSE)
print(RVE1.bias)

## There is strong evidence that there is publication bias
# se  0.8728  0.2656   3.286  62 0.00167  0.34187  1.40364 *** 

## Bias correction using the variance 


RVE1.cor1<- robu(formula=est~tot_in_out_I+tot_in_out_O+CD+tariffadj_decreasing+
                   tariffadj_uniform+tariffadj_others+NID+endoge_no+log(income_19)+I(se^2),
                  data=Nhemd,studynum = study_id,var.eff.size = var,
                  modelweights ="CORR",rho = .8, small = TRUE)
print(RVE1.cor1)



#head(Nhemd)


## Prediction for CI set at 95% ##
print(RVE1.cor1)

## For total demand

predict(object = RVE1.cor1, pred.vector = c(1,0,0,1,0,0,0,1,0,log(1000),0.0597222),level = 0.95)


# Mean value for vars and used for model prediction
head(hem)
var.means <- tapply(hem$var,hem$study_id,mean)

mean(var.means) ## this is used for model prediction
#1643.1
predict(object = RVE1.cor1, pred.vector = c(1,0,0,1,0,0,0,1,0,log(1643.1),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(3031.162),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(4674.261905),0.0597222),level =0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(7705.42381),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(10736.58571),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(13767.74762),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(16798.90952),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(19830.07143),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(22861.23333),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(25892.39524),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(28923.55714),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(31954.71905),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(34985.88095),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(38017.04286),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(41048.204762),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(44079.36667),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(47110.52857),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(50141.69048),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(53172.85238),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(56204.01429),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(59235.17619),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(62266.3381),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(65297.5),0.0597222),level = 0.95)

#########
#########
# For the summary table

predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(1000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(2000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(3500),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(5000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(10000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(15000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(20000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(30000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(50000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(65000),0.0597222),level = 0.95)

##For Indoor demand

RVE19.cor1<- robu(formula=est~tot_in_outh+CD+tarifh+NID+log(income_19)+I(se^2),
                  data=hem,studynum = study_id,var.eff.size =var,
                  modelweights ="CORR",rho = .8, small = FALSE)
print(RVE19.cor1)

predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(1643.1),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(3031.162),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(4674.261905),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(7705.42381),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(10736.58571),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(13767.74762),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(16798.90952),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(19830.07143),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(22861.23333),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(25892.39524),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(28923.55714),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(31954.71905),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(34985.88095),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(38017.04286),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(41048.204762),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(44079.36667),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(47110.52857),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(50141.69048),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(53172.85238),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(56204.01429),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(59235.17619),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(62266.3381),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(65297.5),0.0597222),level = 0.95)

#########
#########
# For the summary table

predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(1000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(2000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(3500),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(5000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(10000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(15000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(20000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(30000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(50000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(65000),0.0597222),level = 0.95)



##For Outdoor demand

RVE19.cor1<- robu(formula=est~tot_in_outh+CD+tarifh+NID+log(income_19)+I(se^2),
                  data=hem,studynum = study_id,var.eff.size =var,
                  modelweights ="CORR",rho = .8, small = FALSE)
print(RVE19.cor1)

predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(1643.1),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(3031.162),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(4674.261905),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(7705.42381),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(10736.58571),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(13767.74762),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(16798.90952),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(19830.07143),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(22861.23333),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(25892.39524),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(28923.55714),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(31954.71905),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(34985.88095),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(38017.04286),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(41048.204762),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(44079.36667),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(47110.52857),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(50141.69048),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(53172.85238),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(56204.01429),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(59235.17619),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(62266.3381),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(65297.5),0.0597222),level = 0.95)

#########
#########
# For the summary table

predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(1000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(2000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(3500),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(5000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(10000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(15000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(20000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(30000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(50000),0.0597222),level = 0.95)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(65000),0.0597222),level = 0.95)


#####################################################################################################
## Prediction for CI set at 80% ##
print(RVE19.cor1)

## For total demand
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(1643.1),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(3031.162),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(4674.261905),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(7705.42381),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(10736.58571),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(13767.74762),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(16798.90952),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(19830.07143),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(22861.23333),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(25892.39524),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(28923.55714),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(31954.71905),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(34985.88095),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(38017.04286),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(41048.204762),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(44079.36667),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(47110.52857),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(50141.69048),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(53172.85238),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(56204.01429),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(59235.17619),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(62266.3381),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,0,1,0,0,0,1,log(65297.5),0.0597222),level = 0.80)


##For Indoor demand

RVE19.cor1<- robu(formula=est~tot_in_outh+CD+tarifh+NID+log(income_19)+I(se^2),
                  data=hem,studynum = study_id,var.eff.size =var,
                  modelweights ="CORR",rho = .8, small = FALSE)
print(RVE19.cor1)

predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(1643.1),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(3031.162),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(4674.261905),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(7705.42381),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(10736.58571),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(13767.74762),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(16798.90952),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(19830.07143),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(22861.23333),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(25892.39524),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(28923.55714),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(31954.71905),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(34985.88095),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(38017.04286),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(41048.204762),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(44079.36667),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(47110.52857),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(50141.69048),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(53172.85238),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(56204.01429),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(59235.17619),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(62266.3381),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,1,0,1,0,0,0,1,log(65297.5),0.0597222),level = 0.80)


##For Outdoor demand

RVE19.cor1<- robu(formula=est~tot_in_outh+CD+tarifh+NID+log(income_19)+I(se^2),
                  data=hem,studynum = study_id,var.eff.size =var,
                  modelweights ="CORR",rho = .8, small = FALSE)
print(RVE19.cor1)

predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(1643.1),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(3031.162),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(4674.261905),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(7705.42381),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(10736.58571),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(13767.74762),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(16798.90952),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(19830.07143),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(22861.23333),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(25892.39524),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(28923.55714),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(31954.71905),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(34985.88095),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(38017.04286),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(41048.204762),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(44079.36667),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(47110.52857),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(50141.69048),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(53172.85238),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(56204.01429),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(59235.17619),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(62266.3381),0.0597222),level = 0.80)
predict(object = RVE19.cor1, pred.vector = c(1,0,1,1,0,0,0,1,log(65297.5),0.0597222),level = 0.80)

#####################################################################################################


### Bias-correction2 ###
############## Bias Correction 2 using WAAP ###############################
0.378 /2.8
cor.2=subset(hem,se<0.135)

cor.2$endoge= as.factor(cor.2$endoge)
endogc2<-relevel(cor.2$endoge,ref = "yes")

cor.2$form= as.factor(cor.2$form)
formhc2<-relevel(cor.2$form,ref ="Double log")

cor.2$data_type= as.factor(cor.2$data_type)
data_typec2<-relevel(cor.2$data_type,ref = "Panel")

cor.2$a_s_l= as.factor(cor.2$a_s_l)
a_s_lc2<-relevel(cor.2$a_s_l,ref = "A")

cor.2$economy=as.factor(cor.2$economy)
economyc2<-relevel(cor.2$economy,ref = "high")

cor.2$tot_in_out=as.factor(cor.2$tot_in_out)
tot_in_outc2<-relevel(cor.2$tot_in_out,ref = "T")

cor.2$income=as.factor(cor.2$income)
incomec2<-relevel(cor.2$income,ref="no")


cor.2$rain_tem=as.factor(cor.2$rain_tem)
climatec2<-relevel(cor.2$rain_tem,ref = "t")

cor.2$scale=as.factor(cor.2$scale)
scalec2<-relevel(cor.2$scale,ref = "macro")

cor.2$data_collection=as.factor(cor.2$data_collection)
data_collectionc2<-relevel(cor.2$data_collection,ref = "daily")


cor.2$publication_type=as.factor(cor.2$publication_type)
publicationc2<-relevel(cor.2$publication_type,ref = "grey")


cor.2$consumption=as.factor(cor.2$consumption)
consc2<-relevel(cor.2$consumption,ref = "total")

cor.2$tariffadj=as.factor(cor.2$tariffadj)
tarifc2<-relevel(cor.2$tariffadj,ref = "increasing")


RVE19.cor2 <- robu(formula=est~tot_in_outc2+climatec2+tarifc2+incomec2+
                     endogc2+log(income_19),
                   data=cor.2,studynum = study_id,var.eff.size =var,
                   modelweights ="CORR",rho = .8, small = FALSE)

print(RVE19.cor2)

## Check climate variables if they can be collapsed into a single group
print(RVE19.cor2)

CD.c2 <- rbind(c(0,0,0,-1,1,0,0,0,0,0,0,0,0)   #beta 4 =beta 5
               +c(0,0,0,0,-1,1,0,0,0,0,0,0,0))  #beta 5 =beta 6

Wald_test(RVE19.cor2, constraints = CD.c2, vcov="CR2")

#conclude: found strong evidence for do not rejecting the null that the coefficents are equal
#Test    F      d.f.    p.val
#HTZ     0.155  25.5   0.697

#  create new CD2 variable for the temperature data included in the model
cor.2$CD2 <- ifelse(cor.2$rain_tem == "t", 0, 1) ##temperature data is 0 the rest is 1
#  Check it has been added
summary(cor.2$CD2)

RVEC2.test1 <- robu(formula=est~tot_in_outc2+CD2+tarifc2+incomec2+log(income_19)+endogc2,
                    data=cor.2,studynum = study_id,var.eff.size =var,modelweights = 
                      "CORR",rho = .8, small = FALSE)
print(RVEC2.test1)

#the model has an effect for temperature data and everything else as a group

## checking the income dummy
# well, income dummies are not statistically significant
# but to have consistency with the previous models it should be shrinked

print(RVE19.cor2)
NIDH.c2 <-rbind(c(0,0,0,0,0,0,0,0,0,-1,1,0,0)) #beta 10 and beta 11

Wald_test(RVE19.cor2, constraints = NIDH.c2, vcov="CR2")

# we cannot reject the null that the coefficents are equal
#Test    F     d.f.  p.val
#HTZ   0.392  6.95   0.551

#  create new NID variable for the "no income data included in the model"
cor.2$NID2 <- ifelse(cor.2$income == "no", 0, 1)

summary(as.factor(cor.2$income))

# Check it has been added
summary(cor.2$NID2)                     

#run the model
RVEC2.test2 <- robu(formula=est~tot_in_outc2+climatec2+tarifc2+NID2+endogc2+log(income_19),
                    data=cor.2,studynum = study_id,var.eff.size =var,
                    modelweights ="CORR",rho = .8, small = FALSE)
print(RVEC2.test2)

##the model has no effect for no income data and everything else as a group


##pooling the shrinking selection with log GDP per capita income

shrink19.cor2 <- robu(formula=est~tot_in_outc2+CD2+tarifc2+NID2+endogc2+log(income_19),
                      data=cor.2,studynum = study_id,var.eff.size =var,
                      modelweights ="CORR",rho = .8, small = FALSE)
print(shrink19.cor2)

## Prediction ##
??clubSandwich

## For total demand
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(1643.1)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(3031.162)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(4674.261905)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(7705.42381)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(10736.58571)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(13767.74762)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(16798.90952)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(19830.07143)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(22861.23333)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(25892.39524)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(28923.55714)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(31954.71905)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(34985.88095)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(38017.04286)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(41048.204762)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(44079.36667)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(47110.52857)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(50141.69048)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(53172.85238)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(56204.01429)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(59235.17619)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(62266.3381)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(65297.5)),level = 0.95)
#######
########
# For the summary table
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(1000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(2000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(3500)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(5000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(10000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(15000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(20000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(30000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(50000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(65000)),level = 0.95)


##For Indoor demand

print(shrink19.cor2)

predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(1643.1)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(3031.162)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(4674.261905)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(7705.42381)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(10736.58571)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(13767.74762)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(16798.90952)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(19830.07143)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(22861.23333)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(25892.39524)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(28923.55714)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(31954.71905)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(34985.88095)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(38017.04286)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(41048.204762)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(44079.36667)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(47110.52857)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(50141.69048)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(53172.85238)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(56204.01429)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(59235.17619)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(62266.3381)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(65297.5)),level = 0.95)

#######
########
# For the summary table
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(1000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(2000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(3500)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(5000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(10000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(15000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(20000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(30000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(50000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(65000)),level = 0.95)


##For Outdoor demand

print(shrink19.cor2)

predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(1643.1)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(3031.162)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(4674.261905)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(7705.42381)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(10736.58571)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(13767.74762)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(16798.90952)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(19830.07143)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(22861.23333)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(25892.39524)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(28923.55714)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(31954.71905)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(34985.88095)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(38017.04286)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(41048.204762)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(44079.36667)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(47110.52857)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(50141.69048)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(53172.85238)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(56204.01429)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(59235.17619)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(62266.3381)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(65297.5)),level = 0.95)

#######
########
# For the summary table
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(1000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(2000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(3500)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(5000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(10000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(15000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(20000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(30000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(50000)),level = 0.95)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(65000)),level = 0.95)


#####################################################################################################

########## 80% CI #########

### Bias-correction2 ###

shrink19.cor2 <- robu(formula=est~tot_in_outc2+CD2+tarifc2+NID2+endogc2+log(income_19),
                      data=cor.2,studynum = study_id,var.eff.size =var,
                      modelweights ="CORR",rho = .8, small = FALSE)
print(shrink19.cor2)
## Prediction ##
## For total demand
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(1643.1)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(3031.162)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(4674.261905)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(7705.42381)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(10736.58571)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(13767.74762)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(16798.90952)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(19830.07143)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(22861.23333)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(25892.39524)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(28923.55714)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(31954.71905)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(34985.88095)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(38017.04286)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(41048.204762)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(44079.36667)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(47110.52857)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(50141.69048)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(53172.85238)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(56204.01429)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(59235.17619)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(62266.3381)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,0,1,0,0,0,1,log(65297.5)),level = 0.80)


##For Indoor demand

print(shrink19.cor2)

predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(1643.1)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(3031.162)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(4674.261905)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(7705.42381)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(10736.58571)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(13767.74762)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(16798.90952)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(19830.07143)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(22861.23333)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(25892.39524)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(28923.55714)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(31954.71905)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(34985.88095)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(38017.04286)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(41048.204762)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(44079.36667)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(47110.52857)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(50141.69048)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(53172.85238)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(56204.01429)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(59235.17619)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(62266.3381)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,1,0,1,0,0,0,1,log(65297.5)),level = 0.80)


##For Outdoor demand

print(shrink19.cor2)

predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(1643.1)),level = 0.80) # not look right
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(3031.162)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(4674.261905)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(7705.42381)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(10736.58571)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(13767.74762)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(16798.90952)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(19830.07143)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(22861.23333)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(25892.39524)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(28923.55714)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(31954.71905)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(34985.88095)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(38017.04286)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(41048.204762)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(44079.36667)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(47110.52857)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(50141.69048)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(53172.85238)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(56204.01429)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(59235.17619)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(62266.3381)),level = 0.80)
predict(object = shrink19.cor2, pred.vector = c(1,0,1,1,0,0,0,1,log(65297.5)),level = 0.80)

####################################################################################################
####################################################################################################
####################################################################################################

### Plotting the Prediction Values For the 2019 Income Data ####

#ploting for 95% CI

predt.95=read.csv("Income2019_Prediction95.csv")
head(predt.95)
dev.off()
## Bias-correction 1
## The Same Y-axis numbering
par(mfrow=c(2,2))
#For total
plot(predt.95$income_19,predt.95$predict_tcr1, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75), cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="PET-PEESE Model: Total Demand")
lines(pred80$income, pred80$predict_tcr1, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



#For Indoor

plot(predt.95$income_19,predt.95$predict_icr1, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="PET-PEESE Model: Indoor Demand")
lines(pred80$income, pred80$predict_icr1, col="red", lwd=4)
legend('bottomleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)


## For Outdoor

plot(predt.95$income_19,predt.95$predict_ocr1, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="PET-PEESE Model: Outdoor Demand")
lines(pred80$income, pred80$predict_ocr1, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



###########################################################################

## Bias-correction 2

dev.off()
## Bias-correction 1
## The Same Y-axis numbering
par(mfrow=c(2,2))
#For total
plot(predt.95$income_19,predt.95$predict_tcr2, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75), cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="WAAP Model: Total Demand")
lines(pred80$income, pred80$predict_tcr2, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



#For Indoor

plot(predt.95$income_19,predt.95$predict_icr2, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="WAAP Model Model: Indoor Demand")
lines(pred80$income, pred80$predict_icr2, col="red", lwd=4)
legend('bottomleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)


## For Outdoor

plot(predt.95$income_19,predt.95$predict_ocr2, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(-0.1,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="WAAP Model Model: Outdoor Demand")
lines(pred80$income, pred80$predict_ocr2, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)

## for 80% CI

predt.80=read.csv("Income2019_Prediction80.csv")
head(predt.80)
dev.off()
## Bias-correction 1
## The Same Y-axis numbering
par(mfrow=c(2,2))
#For total
plot(predt.80$income_19,predt.80$predict_tcr1, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75), cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="PET-PEESE Model: Total Demand")
lines(pred80$income, pred80$predict_tcr1, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



#For Indoor

plot(predt.80$income_19,predt.80$predict_icr1, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="PET-PEESE Model: Indoor Demand")
lines(pred80$income, pred80$predict_icr1, col="red", lwd=4)
legend('bottomleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)


## For Outdoor

plot(predt.80$income_19,predt.80$predict_ocr1, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="PET-PEESE Model: Outdoor Demand")
lines(pred80$income, pred80$predict_ocr1, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



###########################################################################

## Bias-correction 2

dev.off()
## Bias-correction 1
## The Same Y-axis numbering
par(mfrow=c(2,2))
#For total
plot(predt.80$income_19,predt.80$predict_tcr2, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75), cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="WAAP Model: Total Demand")
lines(pred80$income, pred80$predict_tcr2, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



#For Indoor

plot(predt.80$income_19,predt.80$predict_icr2, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(0.05,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="WAAP Model Model: Indoor Demand")
lines(pred80$income, pred80$predict_icr2, col="red", lwd=4)
legend('bottomleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)


## For Outdoor

plot(predt.80$income_19,predt.80$predict_ocr2, type="l", col="blue", lwd=5, las=1,
     cex.axis=1.2, cex.lab=1.4, cex.main=1.4,
     xlim = c(0,65000),ylim = c(-0.1,0.75),cex=.8,
     xlab="GDP Per Capita", ylab="People Elasticity", 
     main="WAAP Model Model: Outdoor Demand")
lines(pred80$income, pred80$predict_ocr2, col="red", lwd=4)
legend('topleft', legend = c('The Whole Sample','2019 Income Data'),col=c(2,4),
       lty=c(1,1),lwd =c(4,5),bty="n",cex=1.3)



###############################################################################
###############################################################################


## Getting indoor-outdoor-total estimates from bias-corrected models

## Bias correction 1 model

tot_in_outho<-relevel(hem$tot_in_out,ref = "O") # for outdoor
tot_in_outhi<-relevel(hem$tot_in_out,ref = "I") # for indoor
tot_in_outht<-relevel(hem$tot_in_out,ref = "T") # for total demand


RVE19.cor1<- robu(formula=est~tot_in_outht+CD+tarifh+NID+log(income_19)+I(se^2),
                  data=hem,studynum = study_id,var.eff.size =var,
                  modelweights ="CORR",rho = .8, small = FALSE)
print(RVE19.cor1)


## Bias correction 2 model
tot_in_outc2o<-relevel(cor.2$tot_in_out,ref = "O") # for outdoor
tot_in_outc2i<-relevel(cor.2$tot_in_out,ref = "I") # for indoor
tot_in_outc2t<-relevel(cor.2$tot_in_out,ref = "T") # for total demand


shrink19.cor2 <- robu(formula=est~tot_in_outc2o+CD2+tarifc2+NID2+log(income_19),
                      data=cor.2,studynum = study_id,var.eff.size =var,
                      modelweights ="CORR",rho = .8, small = FALSE)
print(shrink19.cor2)
