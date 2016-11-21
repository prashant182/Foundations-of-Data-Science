library(dplyr)
library(reshape)
library(ggplot2)
library(plyr)

Midwest

MW.states<-Midwest

MW.Procedures<-data.frame()
MW.Procedures<-MW.states[,c("No_Exercise","Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes")]
MW.Procedures.Matrix<-as.matrix(MW.Procedures)
MW.Cor<-cor(MW.Procedures)
MW.Melt<-melt(data=MW.Cor,varnames = c("x","y"))
MW.Melt <- MW.Melt[order(MW.Melt$value),]
mean(MW.Melt$value)
summary(MW.Melt)
MW.Melt<-MW.Melt[(!MW.Melt$value==1),]
MW.MeltMean<-mean(MW.Melt$value)
grep(pattern = "green",x = colors(),value = T)


# Heat map - using Green
plt_heat_green <- ggplot(data = MW.Melt, aes(x=x, y = y)) +
  theme(panel.background = element_rect(fill = "snow2")) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "yellow",mid = "green", high="dark green", 
                       midpoint = MW.MeltMean,
                       limits = c(0, 1), name = "Correlations") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  labs(x=NULL, y=NULL) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  ggtitle("Heat map of correlations in Risk Factors data : Region 2") 
plt_heat_green 