###Step 5 make the scrframes that will be used for model runs 

library(oSCR)
library(raster)
library(sp)
library(here)

#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

load("./../../../Processed Data/socalbobcat_basicssDF.RDA")
load("./../../../Processed Data/socalbobcat_scrframebuildingblocks.RDA")
load("./../../../Processed Data/socalbobcat_processed_telem_data.RDA")
load("./../../../Processed Data/Spatial Data/socalbcat_ssDFcovrasters.RDA")

###make scrframes with added telemetry!! 
bobcat_data.L <- data2oscr(bcat_L,##edf
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
###there will be warning messages but ignore them, want pois and have sorted collared cats right 

bobcat_data.R <- data2oscr(bcat_R,##edf
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


#####plot to make sure both looking good 
plot(bobcat_data.L$scrFrame, jit=20)
plot(bobcat_data.R$scrFrame)



#####NOW add spatial covs to basic ssDF
#you need this for some reason, i think
.rs.unloadPackage("tidyr")
ssDF.water<-extract.rast(basic.ssDF, water_dist_500.scale, mult = 1000, cov.name = "water_dist")
ssDF.elev<-extract.rast(ssDF.water, elev_500.scale, mult = 1000, cov.name = "elev")
ssDF.road465<-extract.rast(ssDF.elev, road465.1.scale, mult = 1000, cov.name = "road465")
ssDF.road1000<-extract.rast(ssDF.road465, road1000.1.scale, mult = 1000, cov.name = "road1000")
ssDF.imperv<-extract.rast(ssDF.road1000, imperv_500.scale, mult = 1000, cov.name = "imperv")


###Then need to make sure that spatial covs reflected in trap covs and trap covs on ssDF 

#do a left one 
rbs.sf<-bobcat_data.L[["scrFrame"]]
caphist <- rbs.sf$caphist
traps <- rbs.sf$traps
trapCovs <- rbs.sf$trapCovs
sf <- make.scrFrame(caphist = caphist, traps = traps, trapCovs = trapCovs, rsfDF = ssDF.imperv, type = "scr")
bcat.sf.L<-bobcat_data.L$scrFrame
bcat.sf.L$trapCovs<-sf[["trapCovs"]]


#then do a right one 
rbs.sf<-bobcat_data.R[["scrFrame"]]
caphist <- rbs.sf$caphist
traps <- rbs.sf$traps
trapCovs <- rbs.sf$trapCovs
sf <- make.scrFrame(caphist = caphist, traps = traps, trapCovs = trapCovs, rsfDF = ssDF.imperv, type = "scr")
bcat.sf.R<-bobcat_data.R$scrFrame
bcat.sf.R$trapCovs<-sf[["trapCovs"]]


#then, i think this is fine, make one ssDF for both left and right

bcat.ssDF<-ssDF.imperv
bcat.ssDF[[1]]$rec_level<-0
bcat.ssDF[[1]]$site_type<-0
bcat.ssDF[[1]]$cam_type<-0
bcat.ssDF[[1]]$season<-0


###Then save your frames and ssDF, and should be able to move on to the model running process from here... 
save(bcat.sf.L,bcat.sf.R,bcat.ssDF,file = "./../../../Processed Data/socalbobcat_sfssdf_LR_telemandcovs.rda")



