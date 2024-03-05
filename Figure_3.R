library(smoothHR)
library(survival)
library(rms)
library(Hmisc)
library(ggplot2)
library(gridExtra)

RCS<-read.csv("C:\\D\\wang\\data\\RCS_data.csv",header = TRUE,sep = ",")

## anxiety ##

## ndvi300
ddist<-datadist(RCS)
options(datadist="ddist") 
refvalue<-min(RCS$ndvi300mean_match)
ddist$limits$ndvi300mean_match[2]<-refvalue

fit <- cph(Surv(followup,anxiety)~rcs(ndvi300mean_match,nk=3)+age+sex+eth_6cat+edu+income+urban_rural_2cat+smo+drink+BMI, id=eid, data=RCS, x=TRUE,y=TRUE)
anova(fit)

p <-round(anova(fit)[,3],3)
HR<-Predict(fit, ndvi300mean_match,fun=exp,ref.zero = TRUE) 
p_overall <- "P overall < 0.001" # read results
p_nonlinear <- "P non-linearity < 0.001" # read results

range(RCS$ndvi300mean_match)
p5<-ggplot()+
  xlab("NDVI, 300-m buffer")+
  ylab("HR (95% CI)")+
  geom_hline(yintercept = 1,colour="darkgrey",linetype="dashed",size = 1.2)+
  geom_ribbon(data=HR,aes(x=ndvi300mean_match, y=yhat,ymin=lower, ymax=upper),fill="#e18283", alpha=0.25)+
  geom_line(data=HR,aes(x=ndvi300mean_match, y=yhat),linetype="solid",size=1.2,colour="#e18283")+
  scale_x_continuous(breaks = seq(0,0.8,0.2),limits = c(0,0.9))+
  scale_y_continuous(breaks = seq(0.4,1.4,0.2),limits = c(0.4,1.4))+
  annotate("text",x=0.2,y=0.55,size=7,label=c(paste(p_overall,p_nonlinear,sep = "\n")))+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.title = element_text(size=20,colour = "black"),
        axis.text = element_text(size=20,colour = "black"),
        axis.line =element_line(colour = "black",size = 1.2),
        plot.margin = margin(t = 1,  
                             r = 1,  
                             b = 1,  
                             l = 1,  
                             unit = "cm"))+ 
  theme(legend.position = "None")
p5


## ndvi500
ddist<-datadist(RCS)
options(datadist="ddist") 
refvalue<-min(RCS$ndvi500mean_match)
ddist$limits$ndvi500mean_match[2]<-refvalue

fit <- cph(Surv(followup,anxiety)~rcs(ndvi500mean_match,nk=3)+age+sex+eth_6cat+edu+income+urban_rural_2cat+smo+drink+BMI, id=eid, data=RCS, x=TRUE,y=TRUE)
anova(fit)

p <-round(anova(fit)[,3],3)
HR<-Predict(fit, ndvi500mean_match,fun=exp,ref.zero = TRUE) 
p_overall <- "P overall < 0.001"
p_nonlinear <- "P non-linearity < 0.001"

range(RCS$ndvi500mean_match)
p6<-ggplot()+
  xlab("NDVI, 500-m buffer")+
  ylab("HR (95% CI)")+
  geom_hline(yintercept = 1,colour="darkgrey",linetype="dashed",size = 1.2)+
  geom_ribbon(data=HR,aes(x=ndvi500mean_match, y=yhat,ymin=lower, ymax=upper),fill="#e18283", alpha=0.25)+
  geom_line(data=HR,aes(x=ndvi500mean_match, y=yhat),linetype="solid",size=1.2,colour="#e18283")+
  scale_x_continuous(breaks = seq(0,0.8,0.2),limits = c(0,0.9))+
  scale_y_continuous(breaks = seq(0.4,1.4,0.2),limits = c(0.4,1.4))+
  annotate("text",x=0.2,y=0.55,size=7,label=c(paste(p_overall,p_nonlinear,sep = "\n")))+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.title = element_text(size=20,colour = "black"),
        axis.text = element_text(size=20,colour = "black"),
        axis.line =element_line(colour = "black",size = 1.2),
        plot.margin = margin(t = 1,  
                             r = 1,  
                             b = 1,  
                             l = 1,  
                             unit = "cm"))+ 
  theme(legend.position = "None")
p6


## ndvi1000
ddist<-datadist(RCS)
options(datadist="ddist") 
refvalue<-min(RCS$ndvi1000mean_match)
ddist$limits$ndvi1000mean_match[2]<-refvalue

fit <- cph(Surv(followup,anxiety)~rcs(ndvi1000mean_match,nk=3)+age+sex+eth_6cat+edu+income+urban_rural_2cat+smo+drink+BMI, id=eid, data=RCS, x=TRUE,y=TRUE)
anova(fit)

p <-round(anova(fit)[,3],3)
HR<-Predict(fit, ndvi1000mean_match,fun=exp,ref.zero = TRUE) 
p_overall <- "P overall < 0.001"
p_nonlinear <- "P non-linearity < 0.001"

range(RCS$ndvi1000mean_match)
p7<-ggplot()+
  xlab("NDVI, 1000-m buffer")+
  ylab("HR (95% CI)")+
  geom_hline(yintercept = 1,colour="darkgrey",linetype="dashed",size = 1.2)+
  geom_ribbon(data=HR,aes(x=ndvi1000mean_match, y=yhat,ymin=lower, ymax=upper),fill="#e18283", alpha=0.25)+
  geom_line(data=HR,aes(x=ndvi1000mean_match, y=yhat),linetype="solid",size=1.2,colour="#e18283")+
  scale_x_continuous(breaks = seq(0,0.8,0.2),limits = c(0,0.9))+
  scale_y_continuous(breaks = seq(0.4,1.4,0.2),limits = c(0.4,1.4))+
  annotate("text",x=0.2,y=0.55,size=7,label=c(paste(p_overall,p_nonlinear,sep = "\n")))+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.title = element_text(size=20,colour = "black"),
        axis.text = element_text(size=20,colour = "black"),
        axis.line =element_line(colour = "black",size = 1.2),
        plot.margin = margin(t = 1,  
                             r = 1,  
                             b = 1,  
                             l = 1,  
                             unit = "cm"))+ 
  theme(legend.position = "None")
p7


## ndvi1500
ddist<-datadist(RCS)
options(datadist="ddist") 
refvalue<-min(RCS$ndvi1500mean_match)
ddist$limits$ndvi1500mean_match[2]<-refvalue

fit <- cph(Surv(followup,anxiety)~rcs(ndvi1500mean_match,nk=3)+age+sex+eth_6cat+edu+income+urban_rural_2cat+smo+drink+BMI, id=eid, data=RCS, x=TRUE,y=TRUE)
anova(fit)

p <-round(anova(fit)[,3],3)
HR<-Predict(fit, ndvi1500mean_match,fun=exp,ref.zero = TRUE) 
p_overall <- "P overall < 0.001"
p_nonlinear <- "P non-linearity < 0.001"

range(RCS$ndvi1500mean_match)
p8<-ggplot()+
  xlab("NDVI, 1500-m buffer")+
  ylab("HR (95% CI)")+
  geom_hline(yintercept = 1,colour="darkgrey",linetype="dashed",size = 1.2)+
  geom_ribbon(data=HR,aes(x=ndvi1500mean_match, y=yhat,ymin=lower, ymax=upper),fill="#e18283", alpha=0.25)+
  geom_line(data=HR,aes(x=ndvi1500mean_match, y=yhat),linetype="solid",size=1.2,colour="#e18283")+
  scale_x_continuous(breaks = seq(0,0.8,0.2),limits = c(0,0.9))+
  scale_y_continuous(breaks = seq(0.4,1.4,0.2),limits = c(0.4,1.4))+
  annotate("text",x=0.2,y=0.55,size=7,label=c(paste(p_overall,p_nonlinear,sep = "\n")))+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.title = element_text(size=20,colour = "black"),
        axis.text = element_text(size=20,colour = "black"),
        axis.line =element_line(colour = "black",size = 1.2),
        plot.margin = margin(t = 1,  
                             r = 1,  
                             b = 1,  
                             l = 1,  
                             unit = "cm"))+ 
  theme(legend.position = "None")
p8

##
pdf(file="C:/D/wang/results/Figure3.pdf",
    width = 17,
    height = 17)
grid.arrange(p5, p6, p7, p8, ncol=2, nrow = 2)
dev.off()
