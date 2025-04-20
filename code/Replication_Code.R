#install.packages("CausalImpact","readxl","ggplot2","scales") 

set.seed(1988)
library(CausalImpact)
library(readxl)
library(ggplot2)
library(scales)

#Read the data
data <- read_excel("Remesas.xlsx", 
                          col_types = c("numeric","numeric","text", "text", "numeric", 
                                         "numeric", "numeric","numeric"))

#Create two objects with the time series
remesas<-data$Remesas #Remittances 
reservas<-data$Reservas #Reserves

#Create new dataset with both time series
data2 <- cbind(remesas, reservas)

#Visualization for Figure 1
A<-ggplot(data=data, aes(x=MonthID, y=Remesas,colour=2)) +
  geom_line()+geom_vline(xintercept = 219,color="red")+
  theme_bw()+
  labs(title = "Remittances",
       y = "Remittances (in millions dollars)",
       x="Year")+
  scale_y_continuous(labels = unit_format(unit = "M"))+
  scale_x_continuous(breaks=c(0,25,49,73,97,121,
                              145,169,192,217,
                              241,265),
                     labels=c(2002,2004,2006,2008,
                              2010,2012,2014,2016,
                              2018,2020,2022,2024))+
  theme(legend.position="none")

  
B<-ggplot(data=data, aes(x=MonthID, y=reservas,colour=2)) +
  geom_line()+geom_vline(xintercept = 219,size=1.0)+
  theme_bw()+
  labs(title = "Banking Reserves",
       y = "Reserves (in millions Quetzals)",
       x="Year")+
  scale_y_continuous(labels = unit_format(unit = "M"))+
  scale_x_continuous(breaks=c(0,25,49,73,97,121,
                              145,169,192,217,
                              241,265),
                     labels=c(2002,2004,2006,2008,
                              2010,2012,2014,2016,
                              2018,2020,2022,2024))+
  theme(legend.position="none")

library(ggpubr) #Needed to join figures in one figure

#Creates figure with 2 panels, with appropriate labels
C<-ggarrange(A,B,ncol = 2,labels = c("A","B"))

#Shows Figure 1
C

# Here, we determine the pre- and post- periods
# This is needed for the impact analysis
pre.period <- c(1, 218)
post.period <- c(219, 276)

#If working with dates, not recommended
#pre.period <- c("Jan-02", "Jan-20")
#post.period <- c("Feb-20", "Nov-24")

#Here, we run the model with simulations and accounting for month-to-month variation

impact1 <- CausalImpact(data2, pre.period, post.period,
                       model.args=list(niter=5000,nseasons=12))

D<-plot(impact1,c("original"))+
  theme_bw()+
  labs(title = "Remittances received in Guatemala, 2002-2024",
       subtitle="Counterfactual built using banking reserves",
       y = "Remittances (in millions dollars)")+
  scale_y_continuous(labels = unit_format(unit = "M"))+
  scale_x_continuous(breaks=c(0,37,73,109,145,181,217,253),
                     labels=c(2002,2005,2008,2011,2014,2017,2020,2023))

D #Shows time series and the counterfactual

E<-plot(impact1,"pointwise")+
  theme_bw()+
  labs(title = "Difference between remittances and counterfactual, 2002-2024",
      subtitle = "No overlap with 0 M indicates a credible causal effect",
      y="Remittances (in million dollars)")+
  scale_y_continuous(labels = unit_format(unit = "M"))+
  scale_x_continuous(breaks=c(0,37,73,109,145,181,217,253),
                     labels=c(2002,2005,2008,2011,2014,2017,2020,2023))

E #Shows deviation from counterfactual

A1<-ggarrange(D) #Figure 2
B1<-ggarrange(E) #Figure 3

#Shows the summary of the Causal Impact Model
#Includes description of results
summary(impact1) #Components of Table 1

summary(impact1,"report") #This shows a report with all the explanations

#Data needed to calculate % GDP in 2022
series_data <- as.data.frame(impact1$series)
series_data$ID <- 1:nrow(series_data)
data_sub<-subset(series_data,ID>240 & ID<253)
predicted_values <- data_sub[["point.pred"]]
predicted_values2 <- data_sub[["point.pred.lower"]]
predicted_values3 <- data_sub[["point.pred.upper"]]

#Create the central estimate, and CIs
sum(predicted_values)  #Central
sum(predicted_values2) #Lower Limit
sum(predicted_values3) #Upper Limit