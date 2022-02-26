#Step 4 testing poisson & binomial det funcs 

library(oSCR)

#load binpois scrFrames
load("E:/Socal Bobcat Reproducible Research Folder/Processed Data/socalbobcat_binpoisframes.RDA")

#load basic ssDF 
load("E:/Socal Bobcat Reproducible Research Folder/Processed Data/socalbobcat_basicssDF.RDA")

###Run null models

#left bin
m0.L.28occ.bin <- oSCR.fit(model=list(D~1,p0~1,sig~1), 
                           scrFrame=bobcat_data.L.bin$scrFrame, ssDF=basic.ssDF, encmod = "B")
#right bin
m0.R.28occ.bin <- oSCR.fit(model=list(D~1,p0~1,sig~1), 
                           scrFrame=bobcat_data.R.bin$scrFrame, ssDF=basic.ssDF, encmod = "B")

#left pois
m0.L.28occ.pois <- oSCR.fit(model=list(D~1,p0~1,sig~1), 
                            scrFrame=bobcat_data.L.pois$scrFrame, ssDF=basic.ssDF, encmod = "P")

#right pois
m0.R.28occ.pois <- oSCR.fit(model=list(D~1,p0~1,sig~1), 
                            scrFrame=bobcat_data.R.pois$scrFrame, ssDF=basic.ssDF, encmod = "P")


#and save them but you can also evaluate in a model selection below 
save(m0.L.28occ.bin,m0.R.28occ.bin,m0.L.28occ.pois,m0.R.28occ.pois,
     file = "E:/Socal Bobcat Reproducible Research Folder/Results/Model Outputs/socalbobcat_leftright_binpois_nullmod_notelem.RDA")


fl <- fitList.oSCR(list(m0.R.28occ.pois,m0.R.28occ.bin), rename = T)
m0.R.28occ.bin
m0.R.28occ.pois
ms <- modSel.oSCR(fl)

ms
