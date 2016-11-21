library(dplyr)
library(reshape)
library(ggplot2)
library(plyr)

West

WE.states<-West

WE.Procedures<-data.frame()
WE.Procedures<-WE.states[,c("No_Exercise","Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes")]
WE.Procedures.Matrix<-as.matrix(WE.Procedures)
WE.Cor<-cor(WE.Procedures)
WE.Melt<-melt(data=WE.Cor,varnames = c("x","y"))
WE.Melt <- WE.Melt[order(WE.Melt$value),]
mean(WE.Melt$value)
summary(WE.Melt)
WE.Melt<-WE.Melt[(!WE.Melt$value==1),]
WE.MeltMean<-mean(WE.Melt$value)
grep(pattern = "pink",x = colors(),value = T)


# Heat map - using pink
plt_heat_pink <- ggplot(data = WE.Melt, aes(x=x, y = y)) +
  theme(panel.background = element_rect(fill = "snow2")) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "lightpink",mid = "deeppink1", high="deeppink4", 
                       midpoint = WE.MeltMean,
                       limits = c(0, 1), name = "Correlations") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  labs(x=NULL, y=NULL) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  ggtitle("Heat map of correlations in Risk Factors data : Region 4") 
plt_heat_pink