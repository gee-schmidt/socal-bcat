#Step 8b Density model selection

#load oscr package and other packages
library(oSCR)
library(ggplot2)
library(raster)
library(scales)
library(ggthemes)
library(rgdal)
library(maptools)
library(here)

#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

#load models 
load("./../../../Results/Model Outputs/Density Mods/socalbobcat_densitymodels_LR.rda")

##density model selection

fl.L <- fitList.oSCR(list(L.dens.imperv,
                          L.dens.water,
                          L.dens.elev,
                          L.dens.road1000,
                          L.dens.road465), rename = TRUE)

ms.L <- modSel.oSCR(fl.L)
ms.L

##Top L model 
L.dens.imperv

fl.R <- fitList.oSCR(list(R.dens.imperv,
                          R.dens.water,
                          R.dens.elev,
                          R.dens.road1000,
                          R.dens.road465), rename = TRUE)
ms.R <- modSel.oSCR(fl.R)
ms.R

##Top R model 

###LEFT model evaluation 

######TOP MODEL

#per pixel density 

pixdens <- get.real(L.dens.imperv, type = "dens")

#mean pixel density

mean(pixdens[[1]]$estimate*400)

#range pixel density

hist(pixdens[[1]]$estimate*400)

range(pixdens[[1]]$estimate)*400

#bobcats across study area

sum(pixdens[[1]]$estimate)

#getreal sigma

#getreal detection

newdata.det = data.frame(session=factor(1),
                         imperv = 0,
                         elev = 0 ,
                         rec_level = 0)
det.df <- get.real(model = L.dens.imperv, type = "det", newdata = newdata.det)


#getreal density 


L.dens.imperv

write.csv(L.dens.imperv[["outStats"]], file = "2022_0105_bestmodelparams.csv")








ggplot(pixdens[[1]], aes(X*1000, Y*1000, color = estimate)) +
  coord_quickmap() + geom_point() + scale_color_viridis_c()

topmod.preds <- predict.oSCR(L.dens.imperv)
sum(topmod.preds[["ssN"]][[1]])
mean((topmod.preds[["ssN"]][[1]]))*400

L.dens.imperv
ssDF.top <- topmod.preds[["ssDF"]][[1]]
sig.df <- get.real(model = L.dens.imperv, type = "sig")

newdata.det = data.frame(session=factor(1),
                         imperv = 0,
                         elev = 0 ,
                         rec_level = 0)
det.df <- get.real(model = L.dens.imperv, type = "det", newdata = newdata.det)

####things for the equation
sigma = sig.df[1,3]
alpha2elev = L.dens.imperv[["coef.mle"]][3,2]
alpha2imperv = L.dens.imperv[["coef.mle"]][5,2]
distance = 0.5
d.0 = L.dens.imperv[["coef.mle"]][6,2]
d.imperv = L.dens.imperv[["coef.mle"]][7,2]
d.imperv.lwr = (L.dens.imperv[["outStats"]][7,2] - 1.96 * L.dens.imperv[["outStats"]][7,3])
d.imperv.upr = (L.dens.imperv[["outStats"]][7,2] + 1.96 * L.dens.imperv[["outStats"]][7,3])
d.0.lwr = (L.dens.imperv[["outStats"]][6,2] - 1.96 * L.dens.imperv[["outStats"]][6,3])
d.0.upr = (L.dens.imperv[["outStats"]][6,2] + 1.96 * L.dens.imperv[["outStats"]][6,3])

ssDF.top$topeq.all <- exp(-(1/(2*sigma*sigma))*distance + alpha2elev*(ssDF.top$elev) + alpha2imperv*(ssDF.top$imperv))
ssDF.top$rpu.all <- ssDF.top$topeq.all/sum(ssDF.top$topeq.all)

ssDF.top$topeq.elev <- exp(-(1/(2*sigma*sigma))*distance + alpha2elev*(ssDF.top$elev) + alpha2imperv*mean(ssDF.top$imperv))
ssDF.top$rpu.elev <- ssDF.top$topeq.elev/sum(ssDF.top$topeq.elev)

ssDF.top$topeq.imperv <- exp(-(1/(2*sigma*sigma))*distance + alpha2elev*mean(ssDF.top$elev) + alpha2imperv*(ssDF.top$imperv))
ssDF.top$rpu.imperv <- ssDF.top$topeq.imperv/sum(ssDF.top$topeq.imperv)

ssDF.top$ED <- exp(d.0 + d.imperv * ssDF.top$imperv)

ssDF.top$ED.lwr <- exp(d.0.lwr + d.imperv.lwr * ssDF.top$imperv)

ssDF.top$ED.upr <- exp(d.0.upr + d.imperv.upr * ssDF.top$imperv)

mean(ssDF.top$ED)*400
mean(ssDF.top$ED.lwr)*400
mean(ssDF.top$ED.upr)*400

# traps <- data.frame(L.dens.imperv[["scrFrame"]][["traps"]][[1]])
# 
# test <- rasterFromXYZ(ssDF.top[c("X","Y","rpu")])
# plot(test)
# 
# elev.plot <- ggplot() +  
#   geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=elev), alpha=0.8) +
#   geom_point(data = traps, mapping = aes(x = X, y = Y)) +
#   # scale_fill_viridis_c(name = "Elevation /n (m)") +
#   scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",name = "Elevation /n (m)", direction = -1)+
#   coord_equal() +
#   theme_map() +
#   theme(legend.position="right",legend.title=element_text(size=12), legend.text = element_text(size=10)) 
# #+ theme(legend.text=element_text(size=X))
# elev.plot
# 
# imperv.plot <- ggplot() +  
#   geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=imperv), alpha=0.8) +
#   geom_point(data = traps, mapping = aes(x = X, y = Y)) +
#   scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",name = "% Impervious  /n Surface", direction = -1)+
#   # scale_fill_viridis_c(name = "% Impervious  /n Surface") +
#   coord_equal() +
#   theme_map() + 
#   theme(legend.position="right",legend.title=element_text(size=12), legend.text = element_text(size=10)) 
# imperv.plot
# 
# elev.rpu.plot <- ggplot() +  
#   geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.elev*1000), alpha=0.8) +
#   scale_fill_viridis_c(name = "RPU /n Elevation", labels = comma) +
#   coord_equal() +
#   theme_map() +
#   theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
#         legend.key.size = unit(.3, 'cm')) 
# elev.rpu.plot
# imperv.rpu.plot <- ggplot() +  
#   geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.imperv*1000), alpha=0.8) +
#   scale_fill_viridis_c("RPU /n % Impervious /n Surface", labels = comma) +
#   coord_equal() +
#   theme_map() +
#   theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
#         legend.key.size = unit(.3, 'cm')) 
# imperv.rpu.plot
# 
# all.rpu.plot <- ggplot() +  
#   geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.all*1000), alpha=0.8) +
#   scale_fill_viridis_c(name = "Full RPU", labels = comma) +
#   coord_equal() +
#   theme_map() +
#   theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
#         legend.key.size = unit(.3, 'cm')) 
# all.rpu.plot
# 
# ed.plot <- ggplot() +  
#   geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=ED*4), alpha=0.8) +
#   scale_fill_viridis_c(option = "magma", name = bquote("E("~hat(D)~")"), labels = comma) +
#   coord_equal() +
#   theme_map() +
#   theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
#         legend.key.size = unit(.3, 'cm')) 
# 
# ed.plot
# 
# 
# all.maps <- ggarrange(elev.rpu.plot,imperv.rpu.plot,all.rpu.plot,ed.plot,nrow = 2, ncol = 2, labels = "auto",
#                       align = c("hv"), vjust = 5)
# 
# all.maps
# 
# getwd()
# ggsave("2022_0119_RPU_ED_plot.png", width = 6, height = 5, dpi = 800)
# tiff(filename="C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Code_and_Data/2022_0119_RPU_ED_plot.tiff",
#      units="in",width = 6, height = 5, res = 800,compression="lzw")
# all.maps
# dev.off()
# 
# 
# all.maps.2 <- ggarrange(elev.rpu.plot,imperv.rpu.plot,all.rpu.plot,ed.plot,nrow = 2, ncol = 2, labels = "auto",
#                         align = c("hv"), vjust = 5)
# 
# all.maps

# points(traps)

coordinates(traps) <- 1:2

L.rsf.imperv

exp(-5.098)

###


######SERIOUSLY NEEDS CLEANING BUT THIS IS FOR GETTING THE STUDY AREA SPECIFIC DENSITY ESTIMATES
ssDF.top[c(1,2)] <- ssDF.top[c(1,2)]*1000

ssDF.top$ED.4 <- ssDF.top$ED*4

hist(ssDF.top$ED.4)

ED.df <- ssDF.top[c(1,2,16)]  

ED.rast <- rasterFromXYZ(ED.df, crs = "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

plot(ED.rast)


plot(topmod.preds[[pb]])


plot(topmod.preds[["pbar"]][[1]])
plot(topmod.preds[["r"]][[1]])

pq_trapbuff <- st_read
getwd()
pq_trapbuff <- readOGR(dsn = "./Processed Data/Spatial Data/Shapefiles", layer = "PenasquitosTraps_3kmbuff")
plot(pq_trapbuff, add = TRUE)

WUI_trapbuff <- readOGR(dsn = "./Processed Data/Spatial Data/Shapefiles", layer = "WUITraps_3kmbuff")
plot(WUI_trapbuff, add = TRUE)

wildland_trapbuff <- readOGR(dsn = "./Processed Data/Spatial Data/Shapefiles", layer = "WildlandTraps_3kmbuff")
plot(wildland_trapbuff, add = TRUE)


pq_crop <- crop(ED.rast, extent(pq_trapbuff))
pq_mask <- mask(pq_crop, pq_trapbuff)
plot(pq_mask)
plot(pq_trapbuff,add=TRUE, lwd = 2)


WUI_crop <- crop(ED.rast, extent(WUI_trapbuff))
WUI_mask <- mask(WUI_crop, WUI_trapbuff)
plot(WUI_mask)
plot(WUI_trapbuff,add=TRUE, lwd = 2)

wildland_crop <- crop(ED.rast, extent(wildland_trapbuff))
wildland_mask <- mask(wildland_crop, wildland_trapbuff)
plot(wildland_mask)
plot(wildland_trapbuff,add=TRUE, lwd = 2)


cellStats(pq_mask, stat="mean")*100
cellStats(pq_mask, stat="sd")*100





cellStats(WUI_mask, stat="mean")*100
cellStats(WUI_mask, stat="sd")*100


cellStats(wildland_mask, stat="mean")*100
cellStats(wildland_mask, stat="sd")*100


hist(pq_mask)

hist(test)
test <- data.frame(rasterToPoints(pq_mask))
mean(test$ED.4)*100
sd(test$ED.4)*100





###RIGHT model evaluation

topmod.preds <- predict.oSCR(R.dens.imperv)
sum(topmod.preds[["ssN"]][[1]])
mean((topmod.preds[["ssN"]][[1]]))*400

R.dens.imperv
ssDF.top <- R.dens.imperv[["ssDF"]][[1]]
sig.df <- get.real(model = R.dens.imperv, type = "sig")

newdata.det = data.frame(session=factor(1),
                         imperv = 0,
                         elev = 0 ,
                         rec_level = 0)
det.df <- get.real(model = R.dens.imperv, type = "det", newdata = newdata.det)

dens.df <- get.real(model = R.dens.imperv, type = "dens")

dens.df[[1]]
#mean pixel density

mean(dens.df[[1]]$estimate*400)

#range pixel density

hist(dens.df[[1]]$estimate*400)

range(dens.df[[1]]$estimate)

#bobcats across study area

sum(dens.df[[1]]$estimate)


####things for the equation
sigma = sig.df[1,3]
alpha2elev = R.dens.imperv[["coef.mle"]][3,2]
alpha2imperv = R.dens.imperv[["coef.mle"]][5,2]
distance = 0.5
d.0 = R.dens.imperv[["coef.mle"]][6,2]
d.imperv = R.dens.imperv[["coef.mle"]][7,2]
d.imperv.lwr = (R.dens.imperv[["outStats"]][7,2] - 1.96 * R.dens.imperv[["outStats"]][7,3])
d.imperv.upr = (R.dens.imperv[["outStats"]][7,2] + 1.96 * R.dens.imperv[["outStats"]][7,3])
d.0.lwr = (R.dens.imperv[["outStats"]][6,2] - 1.96 * R.dens.imperv[["outStats"]][6,3])
d.0.upr = (R.dens.imperv[["outStats"]][6,2] + 1.96 * R.dens.imperv[["outStats"]][6,3])

ssDF.top$topeq.all <- exp(-(1/(2*sigma*sigma))*distance + alpha2elev*(ssDF.top$elev) + alpha2imperv*(ssDF.top$imperv))
ssDF.top$rpu.all <- ssDF.top$topeq.all/sum(ssDF.top$topeq.all)

ssDF.top$topeq.elev <- exp(-(1/(2*sigma*sigma))*distance + alpha2elev*(ssDF.top$elev) + alpha2imperv*mean(ssDF.top$imperv))
ssDF.top$rpu.elev <- ssDF.top$topeq.elev/sum(ssDF.top$topeq.elev)

ssDF.top$topeq.imperv <- exp(-(1/(2*sigma*sigma))*distance + alpha2elev*mean(ssDF.top$elev) + alpha2imperv*(ssDF.top$imperv))
ssDF.top$rpu.imperv <- ssDF.top$topeq.imperv/sum(ssDF.top$topeq.imperv)

ssDF.top$ED <- exp(d.0 + d.imperv * ssDF.top$imperv)

ssDF.top$ED.lwr <- exp(d.0.lwr + d.imperv.lwr * ssDF.top$imperv)

ssDF.top$ED.upr <- exp(d.0.upr + d.imperv.upr * ssDF.top$imperv)

mean(ssDF.top$ED)*400
mean(ssDF.top$ED.lwr)*400
mean(ssDF.top$ED.upr)*400


####Right model region-specific densities

ED.df <- ssDF.top[c(1,2,19)]  

ED.df[c(1,2)] <- ED.df[c(1,2)]*1000

ED.rast <- rasterFromXYZ(ED.df, crs = "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

plot(ED.rast)


plot(topmod.preds[[pb]])


plot(topmod.preds[["pbar"]][[1]])
plot(topmod.preds[["r"]][[1]])


pq_trapbuff <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "PenasquitosTraps_3kmbuff")
plot(pq_trapbuff, add = TRUE)

WUI_trapbuff <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "WUITraps_3kmbuff")
plot(WUI_trapbuff, add = TRUE)

wildland_trapbuff <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "WildlandTraps_3kmbuff")
plot(wildland_trapbuff, add = TRUE)


pq_crop <- crop(ED.rast, extent(pq_trapbuff))
pq_mask <- mask(pq_crop, pq_trapbuff)
plot(pq_mask)
plot(pq_trapbuff,add=TRUE, lwd = 2)


WUI_crop <- crop(ED.rast, extent(WUI_trapbuff))
WUI_mask <- mask(WUI_crop, WUI_trapbuff)
plot(WUI_mask)
plot(WUI_trapbuff,add=TRUE, lwd = 2)

wildland_crop <- crop(ED.rast, extent(wildland_trapbuff))
wildland_mask <- mask(wildland_crop, wildland_trapbuff)
plot(wildland_mask)
plot(wildland_trapbuff,add=TRUE, lwd = 2)


cellStats(pq_mask, stat="mean")*400
cellStats(pq_mask, stat="sd")*400





cellStats(WUI_mask, stat="mean")*400
cellStats(WUI_mask, stat="sd")*400


cellStats(wildland_mask, stat="mean")*400
cellStats(wildland_mask, stat="sd")*400


hist(pq_mask)

hist(test)
test <- data.frame(rasterToPoints(pq_mask))
mean(test$ED.4)*100
sd(test$ED.4)*100





