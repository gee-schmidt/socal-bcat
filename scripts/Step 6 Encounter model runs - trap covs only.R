### Step 6 Detection Models - trapcovs only sel first 

#load oscr package 
library(oSCR)
library(here)

#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

#load scrframe and ssdf
load("./../../../Processed Data/socalbobcat_sfssdf_LR_telemandcovs.rda")

##nulls
m0.L<- oSCR.fit(model=list(D~1,p0~1,sig~1), 
                           scrFrame=bcat.sf.L, ssDF=bcat.ssDF, encmod = "P",
                           RSF = F, telemetry.type = "dep")

m0.R<- oSCR.fit(model=list(D~1,p0~1,sig~1), 
                           scrFrame=bcat.sf.R, ssDF=bcat.ssDF, encmod = "P",
                           RSF = F, telemetry.type = "dep")


#site_type
mp0site.L <- oSCR.fit(model=list(D~1,p0~site_type,sig~1), 
                                 scrFrame=bcat.sf.L, ssDF=bcat.ssDF, encmod = "P",
                                 RSF = F, telemetry.type = "dep")

mp0site.R <- oSCR.fit(model=list(D~1,p0~site_type,sig~1), 
                                 scrFrame=bcat.sf.R, ssDF=bcat.ssDF, encmod = "P",
                                 RSF = F, telemetry.type = "dep")

#rec_level
mp0rec.L <- oSCR.fit(model=list(D~1,p0~rec_level,sig~1), 
                                scrFrame=bcat.sf.L, ssDF=bcat.ssDF, encmod = "P",
                                RSF = F, telemetry.type = "dep")

mp0rec.R <- oSCR.fit(model=list(D~1,p0~rec_level,sig~1), 
                                scrFrame=bcat.sf.R, ssDF=bcat.ssDF, encmod = "P",
                                RSF = F, telemetry.type = "dep")

#cam_type
mp0cam.L <- oSCR.fit(model=list(D~1,p0~cam_type,sig~1), 
                                scrFrame=bcat.sf.L, ssDF=bcat.ssDF, encmod = "P",
                                RSF = F, telemetry.type = "dep")


mp0cam.R <- oSCR.fit(model=list(D~1,p0~cam_type,sig~1), 
                                scrFrame=bcat.sf.R, ssDF=bcat.ssDF, encmod = "P",
                                RSF = F, telemetry.type = "dep")

#season
mp0season.L <- oSCR.fit(model=list(D~1,p0~season,sig~1), 
                                   scrFrame=bcat.sf.L, ssDF=bcat.ssDF, encmod = "P",
                                   RSF = F, telemetry.type = "dep")


mp0season.R <- oSCR.fit(model=list(D~1,p0~season,sig~1), 
                                   scrFrame=bcat.sf.R, ssDF=bcat.ssDF, encmod = "P",
                                   RSF = F, telemetry.type = "dep")


save(m0.R,m0.L,
     mp0season.R,mp0season.L,
     mp0cam.R,mp0cam.L,
     mp0rec.R,mp0rec.L,
     mp0site.R,mp0site.L,
     file = "./../../../Results/Model Outputs/socalbobcat_trapdet_models_LR.RDA")



