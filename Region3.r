library(dplyr)
library(reshape)
library(ggplot2)
library(plyr)

South

SO.states<-South

SO.Procedures<-data.frame()
SO.Procedures<-SO.states[,c("No_Exercise","Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes")]
SO.Procedures.Matrix<-as.matrix(SO.Procedures)
SO.Cor<-cor(SO.Procedures)
SO.Melt<-melt(data=SO.Cor,varnames = c("x","y"))
SO.Melt <- SO.Melt[order(SO.Melt$value),]
mean(SO.Melt$value)
summary(SO.Melt)
SO.Melt<-SO.Melt[(!SO.Melt$value==1),]
SO.MeltMean<-mean(SO.Melt$value)
grep(pattern = "orange",x = colors(),value = T)


# Heat map - using Orange
plt_heat_orange <- ggplot(data = SO.Melt, aes(x=x, y = y)) +
  theme(panel.background = element_rect(fill = "snow2")) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "yellow",mid = "orange", high="red", 
                       midpoint = SO.MeltMean,
                       limits = c(0, 1), name = "Correlations") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  labs(x=NULL, y=NULL) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  ggtitle("Heat map of correlations in Risk Factors data :  Region 3") 
plt_heat_orange 