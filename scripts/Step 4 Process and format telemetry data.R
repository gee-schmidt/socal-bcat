#Step 4 Process and format telemetry data 

######Telemetry processing script

#load what you need 
library(oSCR)
library(lubridate)
library(tidyverse)
library(dplyr)
library(here)

#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

#load the ssDF bc needed for the telem data 
load("./../../../Processed Data/socalbobcat_basicssDF.RDA")
load("./../../../Processed Data/socalbobcat_scrframebuildingblocks.RDA")

###INFO ON THE CATS INCLUDED 
###Also caught on cam: M03 (66), M14 (71), M15 OR 16 (100)?
###<- position in LEFT edf: 37,40,60 respectively
###<- position in RIGHT edf: 36,39,52 respectively

unique(sort(bcat_R$id))
unique(sort(bcat_L$id))

###NEEDS to be the POSITION in the EDF in cap.tel, NOT id#!!!!
# ##load in the raw telemetry
telem.data <- read.csv("./../../../Processed Data/socalbobcat_telemdata.csv")


telem.data <- telem.data%>%mutate(Date_Format = make_date(Year,Month,Day))

names(telem.data)

###filter for precision/accuracy (according to lewis paper)
filter(telem.data, Satellites >=3 & HDOP <= 5)

total.locs <- telem.data %>% 
  filter(Satellites >=3 & HDOP <= 5) %>%
  group_by(Animal,Date_Format)

m03.locs <- subset(total.locs, Animal == "M03")

telem.data.1 <- telem.data %>% 
  filter(Satellites >=3 & HDOP <= 5) %>%
  group_by(Animal,Date_Format) %>%
  sample_n(1)

telem.data.2 <- telem.data.1 %>% 
  # transform to date format with lubridate
  group_by(Animal) %>% 
  # find years min and max 
  summarise(min = min(Date_Format),
            max = max(Date_Format))


xxtelem.data.1.F04<-subset(telem.data.1, Animal == "F04")
xxtelem.data.1.M01<-subset(telem.data.1, Animal == "M01")
aatelem.data.1.M03<-subset(telem.data.1, Animal == "M03")
xxtelem.data.1.M06<-subset(telem.data.1, Animal == "M06")
xxtelem.data.1.M08<-subset(telem.data.1, Animal == "M08")
bbtelem.data.1.M14<-subset(telem.data.1, Animal == "M14")
xxtelem.data.1.M15<-subset(telem.data.1, Animal == "M15")
cctelem.data.1.M16<-subset(telem.data.1, Animal == "M16")

#thin random sample M01 and M03 (the ones that fail)
nrow(xxtelem.data.1.M01)
thinby<-round(0.1*nrow(xxtelem.data.1.M01))
xxtelem.data.1.M01.thin<-xxtelem.data.1.M01[sample(nrow(xxtelem.data.1.M01), thinby), ]


thinby<-round(0.1*nrow(aatelem.data.1.M03))
aatelem.data.1.M03.thin<-aatelem.data.1.M03[sample(nrow(aatelem.data.1.M03), thinby), ]
###testing even more thinned 


####put them all back together 

###REMOVING M03 -- fucks up sigma and not reflective of his HR in 2012
aatelem.data.1.M03.thin$Animal<-"aaM03"
bbtelem.data.1.M14$Animal<-"bbM14"
cctelem.data.1.M16$Animal<-"ccM16"

catlist<-list(bbtelem.data.1.M14,cctelem.data.1.M16,####caught on cam
              xxtelem.data.1.F04,xxtelem.data.1.M01.thin,xxtelem.data.1.M06,xxtelem.data.1.M08,xxtelem.data.1.M15)
###Also caught on cam: M03 (66), M14 (71), M15 OR 16 (100)? based on homeranges, calling 100 M16
aatelem.data.1.M03.thin$Animal<-"aaM03"
bbtelem.data.1.M14$Animal<-"bbM14"
cctelem.data.1.M16$Animal<-"ccM16"

# cap.tel.L<-c(37,40,60)
# cap.tel.R<-c(36,39,52)
###Remove M03


####give the positions in the scrFrame
cap.tel.L<-c(40,60)
cap.tel.R<-c(39,52)


#put all thinned telem data in a df properly ordered 
telem.data.all.thinned<-do.call(rbind, catlist)

#just get the ID and UTM columns 
telem.data.all.thinned.1<-telem.data.all.thinned[c(2,11,12)]
telem.data.M01.only <- xxtelem.data.1.M01[c(2,11,12)]

#rename 
names(telem.data.all.thinned.1)<- c("ind","X","Y")
names(telem.data.M01.only) <- c("ind","X","Y")
#make ind a character, I think that's important 
telem.data.all.thinned.1$ind<-as.character(telem.data.all.thinned.1$ind)
telem.data.M01.only$ind<-as.character(telem.data.M01.only$ind)

#convert from m to km (to match the ssDF and scrFrame)
telem.data.all.thinned.1[,c("X","Y")] <- telem.data.all.thinned.1[,c("X","Y")]/1e3
telem.data.M01.only[,c("X","Y")] <- telem.data.M01.only[,c("X","Y")]/1e3


###format as df
telem.data.all.thinned.1<-as.data.frame(telem.data.all.thinned.1)
telem.data.M01.only<-as.data.frame(telem.data.M01.only)

##put thru oscr function to get in correct format for scrFrame 
bcat.nfix.thinned <- telemetry.processor(list(basic.ssDF[[1]]),
                                         list(telem.data.all.thinned.1))$nfreq
bcat.nfix.M01 <- telemetry.processor(list(basic.ssDF[[1]]),
                                     list(telem.data.M01.only))$nfreq
###make one specially for left and right datasets for where those collared cats are in those edfs 
bcat.telem.oscr.thinned.L<-list(fixfreq=bcat.nfix.thinned,cap.tel=list(cap.tel.L))
bcat.telem.oscr.thinned.R<-list(fixfreq=bcat.nfix.thinned,cap.tel=list(cap.tel.R))
bcat.telem.oscr.ERROR <- list(fixfreq=bcat.nfix.M01)

###save data file 
save(bcat.telem.oscr.thinned.L,
     bcat.telem.oscr.thinned.R,
     bcat.telem.oscr.ERROR,
     file= "./../../../Processed Data/socalbobcat_processed_telem_data.RDA")


