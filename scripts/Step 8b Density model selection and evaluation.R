#Step 8b Density model selection


#setwd first
setwd("E:/Socal Bobcat Reproducible Research Folder")

#load oscr package 
library(oSCR)

#load models 
load("./Results/Model Outputs/Density Mods/socalbobcat_densitymodels_LR.rda")

##density model selection

fl.L <- fitList.oSCR(list(L.dens.imperv,
                          L.dens.water,
                          L.dens.elev,
                          L.dens.road1000,
                          L.dens.road465), rename = TRUE)

ms.L <- modSel.oSCR(fl.L)
ms.L

fl.R <- fitList.oSCR(list(R.dens.imperv,
                          R.dens.water,
                          R.dens.elev,
                          R.dens.road1000,
                          R.dens.road465), rename = TRUE)
ms.R <- modSel.oSCR(fl.R)
ms.R


###Need to incorp model evals here 