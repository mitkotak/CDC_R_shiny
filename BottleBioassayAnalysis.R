library(tidyverse)

data=read.csv("BottleBioassayEx.csv")

#data cleaning
mortality=data
for(c in 2:ncol(data)){mortality[,c]=data[,c]/data[nrow(data),c]} #calculate %mortality based on final count of live and dead

#data check
#enter warning here if control mortality >10%
if(mortality$Control[(nrow(data)-1)]>0.1){print("Warning: mortality in control bottle is too high. Please repeat bioassay.")}
#Abbot's
if(mortality$Control[(nrow(data)-1)]>0.03){ #use Abbot's
for(c in 2:(ncol(data)-1)){
  mortality[,c]=(mortality[,c]-mortality[,ncol(data)])/(1-mortality[,ncol(data)])
}}

#Visualization
diag_time=60 #look up from table

mortality$Median=apply(mortality[,2:(ncol(data)-1)],1,median,na.rm=T)
mortality_long=pivot_longer(mortality[,1:(ncol(data)-1)],cols = 2:(ncol(data)-1),names_to = "Replicate",values_to = "Mortality")
color_breaks <- data.frame(start = c(0, 0.9, 0.97),  # Create data with breaks
                          end = c(0.9, 0.97, 1),
                          colors = c("#FF0000","#FF9900","#FFFF99"))
ggplot()+
#plotting the guides
  geom_rect(aes(xmin=0,xmax=Inf,ymin=0,ymax=0.9),alpha=0.5,fill="red")+
  geom_rect(aes(xmin=0,xmax=Inf,ymin=0.9,ymax=0.97),alpha=0.5,fill="orange")+
  geom_rect(aes(xmin=0,xmax=Inf,ymin=0.97,ymax=1),alpha=0.5,fill="yellow")+
  geom_vline(xintercept = diag_time,color="blue",linetype="dotted")+ #diagnostic time line
#plotting the observations
  #leaves off the final row to avoid including the live ones
  #individual replicates
  geom_line(data=mortality_long[which(mortality_long$Time<max(data$Time)),], 
            aes(x=Time,y=Mortality,group=Replicate),color="white",linetype="dashed")+
  #median of all replicates
  geom_line(data=mortality[1:(nrow(data)-1),], 
          aes(x=Time,y=Median))+
  #labels
  ylab("Percent Mortality")+xlab("Time (minutes)")+
  theme_minimal()

#messaging
if(mortality$Median[min(which(mortality$Time>=diag_time))]>=0.97){print("Susceptible message")}else{
if(mortality$Median[min(which(mortality$Time>=diag_time))]>=0.90){print("Developing Resistance message")}else{
print("Resistant message")}}
