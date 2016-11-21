
library(dplyr)
library(reshape)
library(ggplot2)

library(plyr)

Northeast

NE.states<-Northeast

NE.Procedures<-data.frame()
NE.Procedures<-NE.states[,c("No_Exercise","Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes")]
NE.Procedures.Matrix<-as.matrix(NE.Procedures)
NE.Cor<-cor(NE.Procedures)
NE.Melt<-melt(data=NE.Cor,varnames = c("x","y"))
NE.Melt <- NE.Melt[order(NE.Melt$value),]
mean(NE.Melt$value)
summary(NE.Melt)
NE.Melt<-NE.Melt[(!NE.Melt$value==1),]
NE.MeltMean<-mean(NE.Melt$value)
grep(pattern = "green",x = colors(),value = T)


# Heat map - using blues
plt_heat_blue <- ggplot(data = NE.Melt, aes(x=x, y = y)) +
  theme(panel.background = element_rect(fill = "snow2")) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "lightcyan", mid = "lightblue", high="midnightblue", 
                       midpoint = NE.MeltMean,
                       limits = c(0, 0.61), name = "Correlations") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  labs(x=NULL, y=NULL) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  ggtitle("Heat map of correlations in Risk Factors data") 
plt_heat_blue 

