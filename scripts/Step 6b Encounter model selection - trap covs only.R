#Step 6b Encounter model selection - trap covs only


#setwd first
setwd("E:/Socal Bobcat Reproducible Research Folder")

#load oscr package 
library(oSCR)

load("./Results/Model Outputs/socalbobcat_trapdet_models_LR.RDA")

############Model Selection######################



fl.L <- fitList.oSCR(list(m0.L,
                          mp0cam.L,
                          mp0season.L,
                          mp0rec.L,
                          mp0site.L), rename=TRUE) #rename=T adds sensible model names
ms.L <- modSel.oSCR(fl.L)
ms.L


fl.R <- fitList.oSCR(list(m0.R,
                          mp0cam.R,
                          mp0season.R,
                          mp0rec.R,
                          mp0site.R), rename=TRUE) #rename=T adds sensible model names
ms.R <- modSel.oSCR(fl.R)
ms.R
