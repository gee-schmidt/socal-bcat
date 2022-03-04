####Script to format effort data for trap opp, eventually. From cam set dates provided by Megan 

setwd("D:/Socal Bobcat Reproducible Research Folder")


library(lubridate)
library(dplyr)

#read in the data 
camset<-read.csv("./Processed Data/socalbcat_camactivity.csv")

#convert to lubridate format
camset$set_date<-ymd(camset$Int_Start)
camset$end_date<-ymd(camset$Int_End)

#intervals in which cams were active 
camset$cam_int<-interval(camset$set_date,camset$end_date)

#split the min of cam set and max of cam set into weeks
break.df<-data.frame(breaks=seq(min(camset$set_date), max(camset$end_date), by="week"))

#convert those into intervals 
break.df$week.int<-interval(break.df$breaks, break.df$breaks+days(6))

break.df$occasion <- 1:63

#testing overlaps 
# int_overlaps(break.df$week.int[1],break.df$week.int[2])

#now make dataframe of stations, with true/false whether they were active during a given week
x<-lapply(camset$cam_int, function(x) int_overlaps(break.df$week.int, x))
df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=T))
df$Station<-camset$Station

##aggregate to station level, get sum of the activity for a week (0 = inactive, > 0, active )
weekly.trap.opp<-aggregate(.~Station, df,sum)

#then just convert everything over 0 to 1, for inactive/active station during a given week
#def not most efficient way to do this but it gets the job done... 
weekly.trap.opp[2:64] <- lapply(weekly.trap.opp[2:64], function(x) ifelse(x > 0, 1, 0))

write.csv(weekly.trap.opp, file = "./Processed Data/socalbcat_weeklytrapopp.csv")

write.csv(break.df, file = "./Processed Data/socalbcat_weeklyoccasions.csv")
#and you have weekly trap operation for 36 cam stations across 63 weeks of monitoring 

