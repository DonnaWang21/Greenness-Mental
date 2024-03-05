
library(ggplot2)
library(tidyr)
library(dplyr)
library(openxlsx)

## depression ##
depression <- read.xlsx("C:\\D\\wang\\data\\Figure1_depression.xlsx")
names(depression)

depression <- depression %>% 
  mutate( 
    Quartile=factor(Quartile,levels = c("Q1", "Q2", "Q3", "Q4")),
    NDVI = factor(NDVI,levels = c("NDVI, 300-m", "NDVI, 500-m","NDVI, 1000-m","NDVI, 1500-m")))

pdf(file = "C:\\D\\wang\\results\\Figure\\Figure1\\Q_depression.pdf",
    width = 10,
    height = 7.5,
    bg="white"
)

forest <- ggplot(depression, aes(NDVI, mean, ymin = lower, ymax = upper, col = Quartile)) + 
  
  geom_hline(yintercept = 1 ,color="darkgrey", linetype="dashed", lwd=.8)+
  
  geom_pointrange(position = position_dodge(width = 0.75), 
                  size = 0.9) + 
  
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.75),
                lwd=0.8, width = 0.3)+
  
  labs(y="HR (95% CI)", x= "NDVI")+
  
  scale_fill_manual(values=c("#666666", "#DFC286","#608595", "#ABC56E"))+
  scale_color_manual(values=c("#666666", "#DFC286","#608595", "#ABC56E"))+
  
  scale_y_continuous(breaks = seq(0.7,1.1,0.1),limit= c(0.7,1.15),expand = c(0,0))+
  
  ggtitle("A. Depression")+
  
  theme_classic()+
  theme(axis.text.x = element_text(size=15,colour = "black",margin = margin(0.3,0,0,0,'cm')),
        axis.text.y = element_text(size=15,colour = "black",margin = margin(0,0.3,0,0,'cm')),
        
        axis.line =element_line(colour = "black",size = 1.0),
        
        legend.position = c(0.75,0.9),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(size=15),
        legend.background = element_rect(color = "black",size = 1),
        
        axis.title.y = element_text(size=15, margin = margin(0,0.3,0,0,'cm'), face="bold"), 
        axis.title.x = element_text(size=15, margin = margin(0,0,0.3,0,'cm'), face="bold"),
        
        plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 15),
        
        axis.ticks.y=element_line(colour="black", size=1, linetype=1, lineend=1), 
        axis.ticks.x=element_line(colour="black", size=1, linetype=1, lineend=1),
        axis.ticks.length.y=unit(.5,"lines"),
        axis.ticks.length.x=unit(.5,"lines"),

        plot.margin = margin(t = 1,  
                             r = 1,  
                             b = 1,  
                             l = 1,  
                             unit = "cm")) 

forest
dev.off()


## anxiety ##

anxiety <- read.xlsx("C:\\D\\wang\\data\\Figure1_anxiety.xlsx")
names(anxiety)

anxiety <- anxiety %>% 
  mutate( 
    Quartile=factor(Quartile,levels = c("Q1", "Q2", "Q3", "Q4")),
    NDVI = factor(NDVI,levels = c("NDVI, 300-m", "NDVI, 500-m","NDVI, 1000-m","NDVI, 1500-m")))

pdf(file = "C:\\D\\wang\\results\\Figure\\Figure1\\Q_anxiety.pdf",
    width = 10,
    height = 7.5,
    bg="white"
)

forest <- ggplot(anxiety, aes(NDVI, mean, ymin = lower, ymax = upper, col = Quartile)) + 
  
  geom_hline(yintercept = 1 ,color="darkgrey", linetype="dashed", lwd=.8)+
  
  geom_pointrange(position = position_dodge(width = 0.75), 
                  size = 0.9) + 
  
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.75),
                lwd=0.8, width = 0.3)+
  
  labs(y="HR (95% CI)", x= "NDVI")+
  
  scale_fill_manual(values=c("#666666", "#DFC286","#608595", "#ABC56E"))+
  scale_color_manual(values=c("#666666", "#DFC286","#608595", "#ABC56E"))+
  
  scale_y_continuous(breaks = seq(0.7,1.1,0.1),limit= c(0.7,1.15),expand = c(0,0))+
  
  ggtitle("B. Anxiety")+
  
  theme_classic()+
  theme(axis.text.x = element_text(size=15,colour = "black",margin = margin(0.3,0,0,0,'cm')),
        axis.text.y = element_text(size=15,colour = "black",margin = margin(0,0.3,0,0,'cm')),
        
        axis.line =element_line(colour = "black",size = 1.0),
        
        legend.position = c(0.75,0.9),
        legend.direction = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(size=15),
        legend.background = element_rect(color = "black",size = 1),
        
        axis.title.y = element_text(size=15, margin = margin(0,0.3,0,0,'cm'), face="bold"), 
        axis.title.x = element_text(size=15, margin = margin(0,0,0.3,0,'cm'), face="bold"),
        
        plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 15),
        
        axis.ticks.y=element_line(colour="black", size=1, linetype=1, lineend=1), 
        axis.ticks.x=element_line(colour="black", size=1, linetype=1, lineend=1),
        axis.ticks.length.y=unit(.5,"lines"),
        axis.ticks.length.x=unit(.5,"lines"),

        plot.margin = margin(t = 1,  
                             r = 1,  
                             b = 1,  
                             l = 1,  
                             unit = "cm")) 

forest
dev.off()
