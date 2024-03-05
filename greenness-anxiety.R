
library(survival)
library(survminer)

analysis <- read.table("C:\\D\\wang\\data\\analysis.csv",header = T,sep = ",",check.names=F)

## anxiety ##

## ndvi300
# crude
ndvi300_anxiety_crude <- coxph(Surv(followup,anxiety)~ ndvi300mean_match_dIQR,
                                  id = eid, data = analysis)
summary(ndvi300_anxiety_crude)
## model 1
ndvi300_anxiety_model1 <- coxph(Surv(followup,anxiety)~ndvi300mean_match_dIQR
                                   +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                   id = eid, data = analysis)
summary(ndvi300_anxiety_model1)

## ndvi500
# crude
ndvi500_anxiety_crude <- coxph(Surv(followup,anxiety)~ ndvi500mean_match_dIQR,
                                  id = eid, data = analysis)
summary(ndvi500_anxiety_crude)
## model 1
ndvi500_anxiety_model1 <- coxph(Surv(followup,anxiety)~ndvi500mean_match_dIQR
                                   +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                   id = eid, data = analysis)
summary(ndvi500_anxiety_model1)

## ndvi1000
# crude
ndvi1000_anxiety_crude <- coxph(Surv(followup,anxiety)~ ndvi1000mean_match_dIQR,
                                   id = eid, data = analysis)
summary(ndvi1000_anxiety_crude)
## model 1
ndvi1000_anxiety_model1 <- coxph(Surv(followup,anxiety)~ndvi1000mean_match_dIQR
                                    +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                    id = eid, data = analysis)
summary(ndvi1000_anxiety_model1)

## ndvi1500
# crude
ndvi1500_anxiety_crude <- coxph(Surv(followup,anxiety)~ ndvi1500mean_match_dIQR,
                                   id = eid, data = analysis)
summary(ndvi1500_anxiety_crude)
## model 1
ndvi1500_anxiety_model1 <- coxph(Surv(followup,anxiety)~ndvi1500mean_match_dIQR
                                    +age+sex+eth_6cat+edu+income+urban_rural_2cat+drink+BMI+smo,
                                    id = eid, data = analysis)
summary(ndvi1500_anxiety_model1)
