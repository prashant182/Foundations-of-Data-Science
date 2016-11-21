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
grep(pattern = "blue",x = colors(),value = T)


# Heat map - using blues
plt_heat_blue <- ggplot(data = NE.Melt, aes(x=x, y = y)) +
  theme(panel.background = element_rect(fill = "snow2")) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "lightcyan",mid = "lightblue", high="royalblue4", 
                       midpoint = NE.MeltMean,
                       limits = c(0, 1), name = "Correlations") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  labs(x=NULL, y=NULL) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  ggtitle("Heat map of correlations in Risk Factors data : Region 1") 
plt_heat_blue 

NE.Procedures$No_Exercise = ifelse(NE.Procedures$No_Exercise >0,NE.Procedures$No_Exercise , 0)
NE.Procedures$Few_Fruit_Veg = ifelse(NE.Procedures$Few_Fruit_Veg >0,NE.Procedures$Few_Fruit_Veg , 0)
NE.Procedures$Obesity = ifelse(NE.Procedures$Obesity >0,NE.Procedures$Obesity , 0)
NE.Procedures$High_Blood_Pres = ifelse(NE.Procedures$High_Blood_Pres >0,NE.Procedures$High_Blood_Pres , 0)
NE.Procedures$Smoker = ifelse(NE.Procedures$Smoker >0,NE.Procedures$Smoker , 0)
NE.Procedures$Diabetes = ifelse(NE.Procedures$Diabetes >0,NE.Procedures$Diabetes , 0)

percent <- c("10%","20%","30%","40%","50%")
plt_obevsNoEx_NE <- ggplot(data = NE.Procedures, aes(x = (NE.Procedures$No_Exercise), y = (NE.Procedures$Obesity), 
                                            color = factor(signif(NE.Procedures$Obesity, 0)))) + 
  geom_point() +  
  scale_color_discrete(name="% Obesity") +
  scale_x_continuous(labels = percent, name = 'No Exercise %s') +
  scale_y_continuous(labels = percent, name = 'Obesity %s') +
  theme_classic(base_size = 12, base_family = 'Verdana') +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of No Exercise") 
plt_obevsNoEx_NE

plt_obevsfru_NE <- ggplot(data = NE.Procedures, aes(x = (NE.Procedures$No_Exercise), y = (NE.Procedures$Few_Fruit_Veg), 
                                                     color = factor(signif(NE.Procedures$Few_Fruit_Veg, 0)))) + 
  geom_point() +  
  scale_color_discrete(name="% Few Fruits and Vegetables") +
  scale_x_continuous(labels = percent, name = 'No Exercise %s') +
  scale_y_continuous(labels = percent, name = 'Few Fruits and Vegetables %s') +
  theme_classic(base_size = 12, base_family = 'Verdana') +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of Few Fruits and Vegetables") 
plt_obevsfru_NE

plt_obevsblood_NE <- ggplot(data = NE.Procedures, aes(x = (NE.Procedures$No_Exercise), y = (NE.Procedures$High_Blood_Pres), 
                                                    color = factor(signif(NE.Procedures$High_Blood_Pres, 0)))) + 
  geom_point() +  
  scale_color_discrete(name="% High Blood Pressure") +
  scale_x_continuous(labels = percent, name = 'No Exercise %s') +
  scale_y_continuous(labels = percent, name = 'High Blood Pressure %s') +
  theme_classic(base_size = 12, base_family = 'Verdana') +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of High Blood Pressure") 
plt_obevsblood_NE

plt_obevsSmoker_NE <- ggplot(data = NE.Procedures, aes(x = (NE.Procedures$No_Exercise), y = (NE.Procedures$Smoker), 
                                                      color = factor(signif(NE.Procedures$Smoker, 0)))) + 
  geom_point() +  
  scale_color_discrete(name="% Smoker") +
  scale_x_continuous(labels = percent, name = 'No Exercise %s') +
  scale_y_continuous(labels = percent, name = 'Smoker %s') +
  theme_classic(base_size = 12, base_family = 'Verdana') +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of Smoker") 
plt_obevsSmoker_NE

percent <- c("10%","20%","30%","40%")
breaks <- c(10,20,30,40)
plt_obevsdia_NE <- ggplot(data = NE.Procedures, aes(x = (NE.Procedures$No_Exercise), y = (NE.Procedures$Diabetes), 
                                                       color = factor(signif(NE.Procedures$Diabetes, 0)))) + 
  geom_point() +  
  scale_color_discrete(name="% Diabetes") +
  scale_x_continuous(labels = percent,breaks = breaks, name = 'No Exercise %s') +
  scale_y_continuous(labels = percent,breaks = breaks, name = 'Diabetes %s') +
  theme_classic(base_size = 12, base_family = 'Verdana') +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of Diabetes") 
plt_obevsdia_NE



g1_noExFruit <- ggplot(data = risk_dat, aes(x=(no.exercise/100), 
                                            y = (few.fruit/100), 
                                            size = no.exercise, 
                                            color = state.abbr, 
                                            group = state.abbr)) +
  theme_stata(base_size = 10, base_family = 'Verdana') + 
  geom_point(na.rm = T) + 
  scale_color_discrete() +  
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  xlab(label = "No Exercise") +
  ylab(label = "Eating few fruits") + 
  theme(legend.position = 'None') + 
  facet_wrap(~state.abbr) +
  ggtitle(label = "Percentage of People Reporting Not Exercising and Eating Few fruits by State")
g1_noExFruit

par(mar=c(0,0,0,1))

pairs(~NE.Procedures$No_Exercise+NE.Procedures$Few_Fruit_Veg+NE.Procedures$Obesity+NE.Procedures$High_Blood_Pres+NE.Procedures$Smoker+NE.Procedures$Diabetes,data=NE.Procedures,main="Multivariate Scatterplot")
