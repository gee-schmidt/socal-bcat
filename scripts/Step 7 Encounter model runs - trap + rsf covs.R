##Step 7 RSF incorporated into detection model 

#load oscr package 
library(oSCR)
library(here)

#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

#load scrframe and ssdf
load("./../../../Processed Data/socalbobcat_sfssdf_LR_telemandcovs.rda")

#setwd again for saving model outputs 
setwd("./../../../Results/Model Outputs/RSF Mods")

#####Lefts
L.rsf.water <- oSCR.fit(model=list(D~1,p0~water_dist + rec_level,sig~1),
                        scrFrame=bcat.sf.L,
                        ssDF=bcat.ssDF,
                        encmod = "P",
                        rsfDF = bcat.ssDF,
                        RSF = T,
                        telemetry.type = "dep")
save(L.rsf.water,
     file = "L_rsf_water.RDA")


L.rsf.elev <- oSCR.fit(model=list(D~1,p0~elev + rec_level,sig~1),
                       scrFrame=bcat.sf.L,
                       ssDF=bcat.ssDF,
                       encmod = "P",
                       rsfDF = bcat.ssDF,
                       RSF = T,
                       telemetry.type = "dep")

save(L.rsf.elev,
     file = "L_rsf_elev.RDA")

L.rsf.imperv <- oSCR.fit(model=list(D~1,p0~imperv + rec_level,sig~1),
                         scrFrame=bcat.sf.L,
                         ssDF=bcat.ssDF,
                         encmod = "P",
                         rsfDF = bcat.ssDF,
                         RSF = T,
                         telemetry.type = "dep")

save(L.rsf.imperv,
     file = "L_rsf_imperv.RDA")


L.rsf.road465 <- oSCR.fit(model=list(D~1,p0~road465 + rec_level,sig~1),
                          scrFrame=bcat.sf.L,
                          ssDF=bcat.ssDF,
                          encmod = "P",
                          rsfDF = bcat.ssDF,
                          RSF = T,
                          telemetry.type = "dep") 

save(L.rsf.road465,
     file = "L_rsf_road465.RDA")

L.rsf.road1000 <- oSCR.fit(model=list(D~1,p0~road1000 + rec_level,sig~1),
                          scrFrame=bcat.sf.L,
                          ssDF=bcat.ssDF,
                          encmod = "P",
                          rsfDF = bcat.ssDF,
                          RSF = T,
                          telemetry.type = "dep") 

save(L.rsf.road1000,
     file = "L_rsf_road1000.RDA")

L.rsf.water.road465 <- oSCR.fit(model=list(D~1,p0~water_dist + road465 + rec_level,sig~1),
                                scrFrame=bcat.sf.L,
                                ssDF=bcat.ssDF,
                                encmod = "P",
                                rsfDF = bcat.ssDF,
                                RSF = T,
                                telemetry.type = "dep") 

save(L.rsf.water.road465,
     file = "L_rsf_water_road465.RDA")

L.rsf.water.road1000 <- oSCR.fit(model=list(D~1,p0~water_dist + road1000 + rec_level,sig~1),
                                 scrFrame=bcat.sf.L,
                                 ssDF=bcat.ssDF,
                                 encmod = "P",
                                 rsfDF = bcat.ssDF,
                                 RSF = T,
                                 telemetry.type = "dep") 

save(L.rsf.water.road1000,
     file = "L_rsf_water_road1000.RDA")

L.rsf.water.elev <- oSCR.fit(model=list(D~1,p0~water_dist + elev + rec_level,sig~1),
                             scrFrame=bcat.sf.L,
                             ssDF=bcat.ssDF,
                             encmod = "P",
                             rsfDF = bcat.ssDF,
                             RSF = T,
                             telemetry.type = "dep") 

save(L.rsf.water.elev,
     file = "L_rsf_water_elev.RDA")

L.rsf.water.imperv <- oSCR.fit(model=list(D~1,p0~water_dist + imperv + rec_level,sig~1),
                               scrFrame=bcat.sf.L,
                               ssDF=bcat.ssDF,
                               encmod = "P",
                               rsfDF = bcat.ssDF,
                               RSF = T,
                               telemetry.type = "dep") 

save(L.rsf.water.imperv,
     file = "L_rsf_water_imperv.RDA")

L.rsf.elev.road465 <- oSCR.fit(model=list(D~1,p0~elev + road465 + rec_level,sig~1),
                               scrFrame=bcat.sf.L,
                               ssDF=bcat.ssDF,
                               encmod = "P",
                               rsfDF = bcat.ssDF,
                               RSF = T,
                               telemetry.type = "dep") 

save(L.rsf.elev.road465,
     file = "L_rsf_elev_road465.RDA")

L.rsf.elev.road1000 <- oSCR.fit(model=list(D~1,p0~ elev + road1000 + rec_level,sig~1),
                                scrFrame=bcat.sf.L,
                                ssDF=bcat.ssDF,
                                encmod = "P",
                                rsfDF = bcat.ssDF,
                                RSF = T,
                                telemetry.type = "dep") 

save(L.rsf.elev.road1000,
     file = "L_rsf_elev_road1000.RDA")

L.rsf.elev.imperv <- oSCR.fit(model=list(D~1,p0~elev + imperv + rec_level,sig~1),
                              scrFrame=bcat.sf.L,
                              ssDF=bcat.ssDF,
                              encmod = "P",
                              rsfDF = bcat.ssDF,
                              RSF = T,
                              telemetry.type = "dep") 

save(L.rsf.elev.imperv,
     file = "L_rsf_elev_imperv.RDA")

####Rights

R.rsf.water <- oSCR.fit(model=list(D~1,p0~water_dist + rec_level,sig~1),
                        scrFrame=bcat.sf.R,
                        ssDF=bcat.ssDF,
                        encmod = "P",
                        rsfDF = bcat.ssDF,
                        RSF = T,
                        telemetry.type = "dep")
save(R.rsf.water,
     file = "R_rsf_water.RDA")


R.rsf.elev <- oSCR.fit(model=list(D~1,p0~elev + rec_level,sig~1),
                       scrFrame=bcat.sf.R,
                       ssDF=bcat.ssDF,
                       encmod = "P",
                       rsfDF = bcat.ssDF,
                       RSF = T,
                       telemetry.type = "dep")

save(R.rsf.elev,
     file = "R_rsf_elev.RDA")

R.rsf.imperv <- oSCR.fit(model=list(D~1,p0~imperv + rec_level,sig~1),
                         scrFrame=bcat.sf.R,
                         ssDF=bcat.ssDF,
                         encmod = "P",
                         rsfDF = bcat.ssDF,
                         RSF = T,
                         telemetry.type = "dep")

save(R.rsf.imperv,
     file = "R_rsf_imperv.RDA")


R.rsf.road465 <- oSCR.fit(model=list(D~1,p0~road465 + rec_level,sig~1),
                          scrFrame=bcat.sf.R,
                          ssDF=bcat.ssDF,
                          encmod = "P",
                          rsfDF = bcat.ssDF,
                          RSF = T,
                          telemetry.type = "dep") 

save(R.rsf.road465,
     file = "R_rsf_road465.RDA")

R.rsf.road1000 <- oSCR.fit(model=list(D~1,p0~road1000 + rec_level,sig~1),
                           scrFrame=bcat.sf.R,
                           ssDF=bcat.ssDF,
                           encmod = "P",
                           rsfDF = bcat.ssDF,
                           RSF = T,
                           telemetry.type = "dep") 

save(R.rsf.road1000,
     file = "R_rsf_road1000.RDA")

R.rsf.water.road465 <- oSCR.fit(model=list(D~1,p0~water_dist + road465 + rec_level,sig~1),
                                scrFrame=bcat.sf.R,
                                ssDF=bcat.ssDF,
                                encmod = "P",
                                rsfDF = bcat.ssDF,
                                RSF = T,
                                telemetry.type = "dep") 

save(R.rsf.water.road465,
     file = "R_rsf_water_road465.RDA")

R.rsf.water.road1000 <- oSCR.fit(model=list(D~1,p0~water_dist + road1000 + rec_level,sig~1),
                                 scrFrame=bcat.sf.R,
                                 ssDF=bcat.ssDF,
                                 encmod = "P",
                                 rsfDF = bcat.ssDF,
                                 RSF = T,
                                 telemetry.type = "dep") 

save(R.rsf.water.road1000,
     file = "R_rsf_water_road1000.RDA")

R.rsf.water.elev <- oSCR.fit(model=list(D~1,p0~water_dist + elev + rec_level,sig~1),
                             scrFrame=bcat.sf.R,
                             ssDF=bcat.ssDF,
                             encmod = "P",
                             rsfDF = bcat.ssDF,
                             RSF = T,
                             telemetry.type = "dep") 

save(R.rsf.water.elev,
     file = "R_rsf_water_elev.RDA")

R.rsf.water.imperv <- oSCR.fit(model=list(D~1,p0~water_dist + imperv + rec_level,sig~1),
                               scrFrame=bcat.sf.R,
                               ssDF=bcat.ssDF,
                               encmod = "P",
                               rsfDF = bcat.ssDF,
                               RSF = T,
                               telemetry.type = "dep") 

save(R.rsf.water.imperv,
     file = "R_rsf_water_imperv.RDA")

R.rsf.elev.road465 <- oSCR.fit(model=list(D~1,p0~elev + road465 + rec_level,sig~1),
                               scrFrame=bcat.sf.R,
                               ssDF=bcat.ssDF,
                               encmod = "P",
                               rsfDF = bcat.ssDF,
                               RSF = T,
                               telemetry.type = "dep") 

save(R.rsf.elev.road465,
     file = "R_rsf_elev_road465.RDA")

R.rsf.elev.road1000 <- oSCR.fit(model=list(D~1,p0~ elev + road1000 + rec_level,sig~1),
                                scrFrame=bcat.sf.R,
                                ssDF=bcat.ssDF,
                                encmod = "P",
                                rsfDF = bcat.ssDF,
                                RSF = T,
                                telemetry.type = "dep") 

save(R.rsf.elev.road1000,
     file = "R_rsf_elev_road1000.RDA")

R.rsf.elev.imperv <- oSCR.fit(model=list(D~1,p0~elev + imperv + rec_level,sig~1),
                              scrFrame=bcat.sf.R,
                              ssDF=bcat.ssDF,
                              encmod = "P",
                              rsfDF = bcat.ssDF,
                              RSF = T,
                              telemetry.type = "dep") 

save(R.rsf.elev.imperv,
     file = "R_rsf_elev_imperv.RDA")

setwd(here())


