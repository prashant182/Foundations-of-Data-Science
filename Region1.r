
library(dplyr)
library(reshape)
library(reshape2)
library(plyr)

Northeast


NE.states<-ddply(Northeast,"CHSI_State_Name",numcolwise(sum))
NE.states$State_FIPS_Code<-NULL
NE.states$Population_Density<-NULL

NE.Procedures<-data.frame()
NE.Procedures<-NE.states[,c("All_Death","Major_Depression","Recent_Drug_Use","No_Exercise","Few_Fruit_Veg","Obesity","High_Blood_Pres","Smoker","Diabetes")]
NE.Procedures.Matrix<-as.matrix(NE.Procedures)

NE.Melt<-melt(NE.Procedures.Matrix)
NE.Melt<-ddply(NE.Melt$X2,.(variable), transform,  rescale = rescale(value),na.action=na.omit(()))
NE.heatmap<-heatmap(NE.Melt, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))



