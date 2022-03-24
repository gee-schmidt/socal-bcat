#Step 6b Encounter model selection - trap covs only

#setwd first
setwd("E:/Socal Bobcat Reproducible Research Folder")

#load oscr package 
library(oSCR)


###load mods 
##trapcov mods (LR)
load("./Results/Model Outputs/socalbobcat_trapdet_models_LR.RDA")

##rsf mods (LR) 
file_names=list.files("./Results/Model Outputs/RSF Mods",pattern="RDA",full.names = TRUE)

lapply(file_names, load, environment())

######Model Selection############
fl.L <- fitList.oSCR(list(m0.L,
                          mp0cam.L,
                          mp0season.L,
                          mp0rec.L,
                          mp0site.L,
                          L.rsf.elev,
                          L.rsf.water,
                          L.rsf.imperv,
                          L.rsf.road465,
                          L.rsf.road1000,
                          L.rsf.water.elev,
                          L.rsf.water.imperv,
                          L.rsf.water.road465,
                          L.rsf.water.road1000,
                          L.rsf.elev.imperv,
                          L.rsf.elev.road465,
                          L.rsf.elev.road1000), rename=TRUE) #rename=T adds sensible model names
ms.L <- modSel.oSCR(fl.L)
ms.L


fl.R <- fitList.oSCR(list(m0.R,
                          mp0cam.R,
                          mp0season.R,
                          mp0rec.R,
                          mp0site.R,
                          R.rsf.elev,
                          R.rsf.water,
                          R.rsf.imperv,
                          R.rsf.road465,
                          R.rsf.road1000,
                          R.rsf.water.elev,
                          R.rsf.water.imperv,
                          R.rsf.water.road465,
                          R.rsf.water.road1000,
                          R.rsf.elev.imperv,
                          R.rsf.elev.road465,
                          R.rsf.elev.road1000), rename=TRUE) #rename=T adds sensible model names
ms.R <- modSel.oSCR(fl.R)
ms.R


####WILL NEED TO EVAL SOME MODS HERE --- esp for the right. Add later. 