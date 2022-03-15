#Step 8 Density model runs 

#setwd first
setwd("E:/Socal Bobcat Reproducible Research Folder")

#load oscr package 
library(oSCR)

#load scrframe and ssdf
load("./Processed Data/socalbobcat_sfssdf_LR_telemandcovs.rda")

###Left density models 

L.dens.road465 <- oSCR.fit(model=list(D~road465,p0~elev + rec_level + imperv,sig~1),
                           scrFrame=bcat.sf.L,
                           ssDF=bcat.ssDF,
                           encmod = "P",
                           rsfDF = bcat.ssDF,
                           RSF = T,
                           telemetry.type = "dep")

L.dens.road1000 <- oSCR.fit(model=list(D~road1000,p0~elev + rec_level + imperv,sig~1),
                            scrFrame=bcat.sf.L,
                            ssDF=bcat.ssDF,
                            encmod = "P",
                            rsfDF = bcat.ssDF,
                            RSF = T,
                            telemetry.type = "dep")

L.dens.elev <- oSCR.fit(model=list(D~elev,p0~elev + rec_level + imperv,sig~1),
                        scrFrame=bcat.sf.L,
                        ssDF=bcat.ssDF,
                        encmod = "P",
                        rsfDF = bcat.ssDF,
                        RSF = T,
                        telemetry.type = "dep")


L.dens.water <- oSCR.fit(model=list(D~water_dist,p0~elev + rec_level + imperv,sig~1),
                         scrFrame=bcat.sf.L,
                         ssDF=bcat.ssDF,
                         encmod = "P",
                         rsfDF = bcat.ssDF,
                         RSF = T,
                         telemetry.type = "dep")

L.dens.imperv <- oSCR.fit(model=list(D~imperv,p0~elev + rec_level + imperv,sig~1),
                          scrFrame=bcat.sf.L,
                          ssDF=bcat.ssDF,
                          encmod = "P",
                          rsfDF = bcat.ssDF,
                          RSF = T,
                          telemetry.type = "dep")

###Right density models 
R.dens.road465 <- oSCR.fit(model=list(D~road465,p0~elev + rec_level + imperv,sig~1),
                           scrFrame=bcat.sf.R,
                           ssDF=bcat.ssDF,
                           encmod = "P",
                           rsfDF = bcat.ssDF,
                           RSF = T,
                           telemetry.type = "dep")

R.dens.road1000 <- oSCR.fit(model=list(D~road1000,p0~elev + rec_level + imperv,sig~1),
                            scrFrame=bcat.sf.R,
                            ssDF=bcat.ssDF,
                            encmod = "P",
                            rsfDF = bcat.ssDF,
                            RSF = T,
                            telemetry.type = "dep")

R.dens.elev <- oSCR.fit(model=list(D~elev,p0~elev + rec_level + imperv,sig~1),
                        scrFrame=bcat.sf.R,
                        ssDF=bcat.ssDF,
                        encmod = "P",
                        rsfDF = bcat.ssDF,
                        RSF = T,
                        telemetry.type = "dep")

R.dens.water <- oSCR.fit(model=list(D~water_dist,p0~elev + rec_level + imperv,sig~1),
                         scrFrame=bcat.sf.R,
                         ssDF=bcat.ssDF,
                         encmod = "P",
                         rsfDF = bcat.ssDF,
                         RSF = T,
                         telemetry.type = "dep")

R.dens.imperv <- oSCR.fit(model=list(D~imperv,p0~elev + rec_level + imperv,sig~1),
                          scrFrame=bcat.sf.R,
                          ssDF=bcat.ssDF,
                          encmod = "P",
                          rsfDF = bcat.ssDF,
                          RSF = T,
                          telemetry.type = "dep")

save(L.dens.imperv,L.dens.water,L.dens.elev,L.dens.road1000,L.dens.road465,
     R.dens.imperv,R.dens.water,R.dens.elev,R.dens.road1000,R.dens.road465,
     file = "./Results/Model Outputs/Density Mods/socalbobcat_densitymodels_LR.rda")