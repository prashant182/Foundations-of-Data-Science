data<-read.csv("E:/NYU/1/Foundation of Data Science/Projects/Foundations-of-Data-Science/USAdata_Original.csv",stringsAsFactors = FALSE,skipNul = TRUE)
data[,"region"] <- NA
data$X<-NULL
data$X.1<-NULL
data$region[data$CHSI_State_Name=="Connecticut" | data$CHSI_State_Name=="Maine" | data$CHSI_State_Name=="Massachusetts" | data$CHSI_State_Name=="New Hampshire" |data$CHSI_State_Name=="Massachusetts"|data$CHSI_State_Name=="Rhode Island"|data$CHSI_State_Name=="Vermont"|data$CHSI_State_Name=="New Jersey" | data$CHSI_State_Name=="New York" | data$CHSI_State_Name=="Pennsylvania"]<-1
data$region[data$CHSI_State_Name=="Illinois" | data$CHSI_State_Name=="Indiana" | data$CHSI_State_Name=="Michigan" | data$CHSI_State_Name=="Ohio" |data$CHSI_State_Name=="Wisconsin"|data$CHSI_State_Name=="Iowa"|data$CHSI_State_Name=="Kansas"|data$CHSI_State_Name=="Minnesota" | data$CHSI_State_Name=="Nebraska" | data$CHSI_State_Name=="North Dakota"| data$CHSI_State_Name=="South Dakota"| data$CHSI_State_Name=="Missouri"]<-2
data$region[data$CHSI_State_Name=="Delaware" | data$CHSI_State_Name=="Florida" | data$CHSI_State_Name=="Georgia" | data$CHSI_State_Name=="Maryland" |data$CHSI_State_Name=="North Carolina"|data$CHSI_State_Name=="South Carolina"|data$CHSI_State_Name=="Virginia"|data$CHSI_State_Name=="District of Columbia" | data$CHSI_State_Name=="West Virginia" | data$CHSI_State_Name=="Alabama"| data$CHSI_State_Name=="Kentucky" | data$CHSI_State_Name=="Mississippi"| data$CHSI_State_Name=="Tennessee"| data$CHSI_State_Name=="Arkansas"| data$CHSI_State_Name=="Louisiana"| data$CHSI_State_Name=="Oklahoma"|data$CHSI_State_Name=="Texas"]<-3
data$region[data$CHSI_State_Name=="Arizona" | data$CHSI_State_Name=="Colorado" | data$CHSI_State_Name=="Idaho" | data$CHSI_State_Name=="Montana" |data$CHSI_State_Name=="Nevada"|data$CHSI_State_Name=="New Mexico"|data$CHSI_State_Name=="Utah"|data$CHSI_State_Name=="Wyoming" | data$CHSI_State_Name=="Alaska" | data$CHSI_State_Name=="California"| data$CHSI_State_Name=="Hawaii"|data$CHSI_State_Name=="Oregon"|data$CHSI_State_Name=="Washington"]<-4
Northeast<- data[data$region==1,]
Midwest<-data[data$region==2,]
South<-data[data$region==3,]
West<-data[data$region==4,]

correlation<-cor(data[, c(19:26)],use="complete")





