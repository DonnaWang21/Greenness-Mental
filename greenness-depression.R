
library(survival)
library(survminer)

analysis <- read.table("C:\\D\\wang\\data\\analysis.csv",header = T,sep = ",",check.names=F)

## depression ##

## ndvi300
# crude
ndvi300_depression_crude <- coxph(Surv(followup,depression)~ ndvi300mean_match_dIQR,
                                  id = eid, data = analysis)
summary(ndvi300_depression_crude)
## model 1
ndvi300_depression_model1 <- coxph(Surv(followup,depression)~ndvi300mean_match_dIQR
                                   +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                   id = eid, data = analysis)
summary(ndvi300_depression_model1)

## ndvi500
# crude
ndvi500_depression_crude <- coxph(Surv(followup,depression)~ ndvi500mean_match_dIQR,
                                  id = eid, data = analysis)
summary(ndvi500_depression_crude)
## model 1
ndvi500_depression_model1 <- coxph(Surv(followup,depression)~ndvi500mean_match_dIQR
                                   +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                   id = eid, data = analysis)
summary(ndvi500_depression_model1)

## ndvi1000
# crude
ndvi1000_depression_crude <- coxph(Surv(followup,depression)~ ndvi1000mean_match_dIQR,
                                  id = eid, data = analysis)
summary(ndvi1000_depression_crude)
## model 1
ndvi1000_depression_model1 <- coxph(Surv(followup,depression)~ndvi1000mean_match_dIQR
                                   +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                   id = eid, data = analysis)
summary(ndvi1000_depression_model1)

## ndvi1500
# crude
ndvi1500_depression_crude <- coxph(Surv(followup,depression)~ ndvi1500mean_match_dIQR,
                                   id = eid, data = analysis)
summary(ndvi1500_depression_crude)
## model 1
ndvi1500_depression_model1 <- coxph(Surv(followup,depression)~ndvi1500mean_match_dIQR
                                    +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                    id = eid, data = analysis)
summary(ndvi1500_depression_model1)
