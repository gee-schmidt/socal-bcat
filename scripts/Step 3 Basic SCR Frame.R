##Step 3 Basic SCR Frame setup (No Telem yet because need a basic ssDF to format telem data)

###This basic scrframe can be used to test buffer sizes for the ssDF and pois/binom on the det function

rm(list=ls())

setwd("C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Data_Inputs")
# rm(list=ls())
library(dplyr)
library(car)
library(oSCR)
library(reshape2)
library(raster)
library(rgdal)
library(maptools)
library(ggplot2)
library(sp)
library(rgeos)
library(tibble)
library(lubridate)
library(BiocManager)
library(fuzzyjoin)
library(IRanges)
library(spatialEco)

##raw capture data still with dates of cap, not occasion
bcat<-read.csv("C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Data_Inputs/SCR_master_11-2-18_2020_1028_idsheet_gms_QC_2021_0602.csv")
bcat$occ<-ymd(bcat$occ)
bcat<-bcat[,c(2:6)]

#raw trap loc data, includes collared cat cap locs
traps<-read.csv("detectors_all_new.csv")
trap.metadata <- read.csv("2020_0112_detcovs.csv", stringsAsFactors = FALSE)

###reduce size of the coords so that things will run faster 
traps[,c(2:3)]<-traps[,c(2:3)]/1000

#remove the capture locations, retain detectors only 
traps.nocaps<-traps[c(1:36),]

####changing to ones and zeroes here
#Site Type
### Core = 1, Linkage = 0
#Rec Level
### Low = 1, Medium = 1, High = 0
#Cam Type 
### Cuddeback = 0, LTLAcorn = 1
#Season
### Wet = 1, Dry = 0
trap.metadata$Core_Linkage<-as.numeric(ifelse(trap.metadata$Core_Linkage == "Core", 1, 0))
trap.metadata$Recreation<-as.numeric(ifelse(trap.metadata$Recreation == "H", 0,1))
trap.metadata$Camera<-as.numeric(ifelse(trap.metadata$Camera == "LTLAcorn", 1, 0))


trap.covs<-cbind(trap.metadata, replicate(28,trap.metadata$Core_Linkage))
trap.covs<-cbind(trap.covs,replicate(28,trap.metadata$Recreation))
trap.covs<-cbind(trap.covs,replicate(28,trap.metadata$Camera))
names(trap.covs)

###put in the wet/rainy season here 1 through 11 == wet, 12 through 22 == dry, 23 - 28 ==wet
season<-c(replicate(11, 1),replicate(11,0),replicate(6,1))

season.frame<-as.data.frame(do.call("rbind",replicate(36,season,simplify = FALSE)))
names(season.frame)<-c(1:28)

trap.covs<-cbind(trap.covs,season.frame)


names(trap.covs)[c(7:34)]<-paste("site_type",names(trap.covs)[c(7:34)],sep=".")
names(trap.covs)[c(35:62)]<-paste("rec_level",names(trap.covs)[c(35:62)],sep=".")
names(trap.covs)[c(63:90)]<-paste("cam_type",names(trap.covs)[c(63:90)],sep=".")
names(trap.covs)[c(91:118)]<-paste("season",names(trap.covs)[c(91:118)],sep = ".")



trap.covs.add<-trap.covs[,-c(2:6)]
names(trap.covs.add)[c(1)]<-"Detector"



names(trap.covs.add)
###brief look at trap locs 
plot(traps.nocaps$X,traps.nocaps$Y)


###want to add trap opp
trap.opp<-read.csv("C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/Cam_Activity/2020_1102_socalbcat_weeklytrapopp.csv")

##append to trap opp df
traps.nocaps.opp<-cbind(traps.nocaps,trap.opp[3:65])

###now need to bin the bobcat detections into 1:63 occasions
###mess with some date data to get the occs as weekly intervals 
camset<-read.csv("C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/Cam_Activity/2020_1102_socalbcat_camactivity.csv")
camset$set_date<-ymd(camset$Int_Start)
camset$end_date<-ymd(camset$Int_End)
camset$cam_int<-interval(camset$set_date,camset$end_date)
break.df<-data.frame(breaks=seq(min(camset$set_date), max(camset$end_date), by="week"))
break.df$week.int<-interval(break.df$breaks, break.df$breaks+days(6))
break.df$occasion <- 1:63

##make merge-able date columns 
break.df$start<-int_start(break.df$week.int)
break.df$end<-int_end(break.df$week.int)
break.df$start<-ymd(break.df$start)
break.df$end<-ymd(break.df$end)

bcat$start<-bcat$occ
bcat$end<-bcat$occ

#confirm that the date classes match before merging 
dput(bcat$start[1])
dput(bcat$end[1])
dput(break.df$start[1])
dput(break.df$end[1])


###this should get it to the correct occasions for each detection 
bcat.occ<-interval_inner_join(bcat,break.df,by=c("start","end"))
bcat.occ$session<-1

new.occs<-(rep(1:30,each=2))
new.occs<-append(new.occs,c(99,99,99))
occ.shrink<-data.frame(occasion = c(1:63), occ.shrink = new.occs)

bcat.occ.shrink<-merge(bcat.occ,occ.shrink, by = "occasion")
traps.nocaps.opp.shrink<-traps.nocaps.opp[,c(4:63)]

collapse.effort<-traps.nocaps.opp.shrink[c(TRUE,FALSE)] + traps.nocaps.opp.shrink[c(FALSE,TRUE)]
collapse.effort<-ifelse(collapse.effort[,c(1:30)] >0,1,0)

opp.shrink.tdf<-cbind(traps.nocaps.opp[,c(1:3)],collapse.effort)
names(opp.shrink.tdf)[c(4:33)]<-1:30



####only 28 occasions -- up to Dec 25, 2012 
bcat.occ.shrink.28occs<-subset(bcat.occ.shrink, occasion <= 56)
opp.shrink.tdf.28occs<-opp.shrink.tdf[,c(1:31)]

opp.shrink.tdf.28occs["sep"]<-"/"

opp.shrink.tdf.28occs.metadata<-merge(opp.shrink.tdf.28occs,trap.covs.add,by="Detector")
names(opp.shrink.tdf.28occs.metadata)
# tdf_w_opp<-opp.shrink.tdf
###subset into lefts and rights (both in both), the ors_both column has this information 
bcat_L<-subset(bcat.occ.shrink.28occs, ors_both == "Left" | ors_both == "Both")
bcat_R<-subset(bcat.occ.shrink.28occs, ors_both == "Right"| ors_both == "Both")


save(bcat_L,bcat_R,opp.shrink.tdf.28occs.metadata,
     file = "C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Code_and_Data/2021_0602_scrframebuildingblocks.RDA")

load("C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Data_Inputs/2020_0602_processed_telem_data.RDA")
####NOW to make the scrframe ... one for left and both, one for right and both
names(opp.shrink.tdf.28occs.metadata)
names(bcat_L)
bobcat_data.L.bin <- data2oscr(bcat_L,##edf
                               tdf = list(opp.shrink.tdf.28occs.metadata), ##tdf w/ trap effort 
                               sess.col = 13,
                               id.col = 2,
                               occ.col = 14, 
                               trap.col = 4,
                               K = c(28), ##sessions w/ occasions 
                               ntraps = c(36),##traps
                               telemetry = bcat.telem.oscr.thinned.L,
                               trapcov.names = c("site_type","rec_level","cam_type","season"),
                               tdf.sep = "/")##if have trapcovs 


bobcat_data.R.bin <- data2oscr(bcat_R,##edf
                               tdf = list(opp.shrink.tdf.28occs.metadata), ##tdf w/ trap effort 
                               sess.col = 13,
                               id.col = 2,
                               occ.col = 14, 
                               trap.col = 4,
                               K = c(28), ##sessions w/ occasions 
                               ntraps = c(36),##traps
                               telemetry = bcat.telem.oscr.thinned.R,
                               trapcov.names = c("site_type","rec_level","cam_type","season"),
                               tdf.sep = "/")



bobcat_data.L.pois <- data2oscr(bcat_L,##edf
                                tdf = list(opp.shrink.tdf.28occs.metadata), ##tdf w/ trap effort 
                                sess.col = 13,
                                id.col = 2,
                                occ.col = 14, 
                                trap.col = 4,
                                K = c(28), ##sessions w/ occasions 
                                ntraps = c(36),##traps
                                telemetry = bcat.telem.oscr.thinned.L,
                                trapcov.names = c("site_type","rec_level","cam_type","season"),
                                tdf.sep = "/",
                                remove.extracaps = FALSE)##if have trapcovs 


bobcat_data.R.pois <- data2oscr(bcat_R,##edf
                                tdf = list(opp.shrink.tdf.28occs.metadata), ##tdf w/ trap effort 
                                sess.col = 13,
                                id.col = 2,
                                occ.col = 14, 
                                trap.col = 4,
                                K = c(28), ##sessions w/ occasions 
                                ntraps = c(36),##traps
                                telemetry = bcat.telem.oscr.thinned.R,
                                trapcov.names = c("site_type","rec_level","cam_type","season"),
                                tdf.sep = "/",
                                remove.extracaps = FALSE)##if have trapcovs 

plot(bobcat_data.L.pois$scrFrame, jit=20)
plot(bobcat_data.R.pois$scrFrame)

# printframe.L.bin<-print.scrFrame.new(bobcat_data.L.bin$scrFrame)
# hist(printframe[[2]][[1]])
#ssdf w/ buffers of 8km and 500mx500m  res
bcat.L.ssDF.pois<-make.ssDF(bobcat_data.L.pois$scrFrame, buffer = 8, res = 0.5)
bcat.R.ssDF.pois<-make.ssDF(bobcat_data.R.pois$scrFrame, buffer = 8, res = 0.5)

bcat.L.ssDF.bin<-make.ssDF(bobcat_data.L.bin$scrFrame, buffer = 8, res = 0.5)
bcat.R.ssDF.bin<-make.ssDF(bobcat_data.R.bin$scrFrame, buffer = 8, res = 0.5)

plot(bcat.L.ssDF.pois)

####make the masks here 

ocean <- readOGR(dsn = "C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/2021_0112_From_Megan/ocean", layer = "ocean")

plot(ocean)
ssdf_tomask.L<- bcat.L.ssDF.pois[[1]]
ssdf_tomask.R<- bcat.R.ssDF.pois[[1]]
ssdf_tomask.L.XY<-ssdf_tomask.L[,c(1,2)]*1000
ssdf_tomask.R.XY<-ssdf_tomask.R[,c(1,2)]*1000
L.spdf <- SpatialPointsDataFrame(coords = ssdf_tomask.L.XY,data = ssdf_tomask.L.XY,
                                 proj4string = CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
points(L.spdf)
L.masked <- erase.point(L.spdf,ocean)
plot(L.masked)

L.masked_df <- as.data.frame(L.masked)
L.masked_df_reduced<-L.masked_df[,c(1:2)]/1000
bcat.L.ssDF.pois[[1]]<-L.masked_df_reduced
plot(bcat.L.ssDF.pois)

R.spdf <- SpatialPointsDataFrame(coords = ssdf_tomask.R.XY,data = ssdf_tomask.R.XY,
                                 proj4string = CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
points(R.spdf)
R.masked <- erase.point(R.spdf,ocean)
plot(R.masked)

R.masked_df <- as.data.frame(R.masked)
R.masked_df_reduced<-R.masked_df[,c(1:2)]/1000
bcat.R.ssDF.pois[[1]]<-R.masked_df_reduced
plot(bcat.R.ssDF.pois)

# save(bobcat_data.L.bin,bobcat_data.R.bin,bcat.L.ssDF.bin,bcat.R.ssDF.bin, file = "C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Code_and_Data/2021_0313_socalbcat_nullmod_scrframe_ssdf_28occshrink_fixedtdf_binary.RDA")
# save(bobcat_data.L.pois,bobcat_data.R.pois,bcat.L.ssDF.pois,bcat.R.ssDF.pois, file = "C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Code_and_Data/2021_0313_socalbcat_nullmod_scrframe_ssdf_28occshrink_fixedtdf_poisson.RDA")


######ALLLL THIS IS DONE WHEN YOU LOAD WHAT IS BELOW!! (already masked) 
