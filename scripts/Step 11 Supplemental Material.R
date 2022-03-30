#Step 11 Supplemental Material 

#load oscr package and other packages - agh so many loaded probably not all used :/ 
library(oSCR)
library(ggplot2)
library(raster)
library(scales)
library(ggthemes)
library(rgdal)
library(maptools)
library(paletteer)
library(colortools)
library(ggpubr)
library(here)
library(sp)
library(rgeos)
library(sf)
library(rasterVis)
library(RColorBrewer)
library(viridis)
#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

#load models 
load("./../../../Results/Model Outputs/Density Mods/socalbobcat_densitymodels_LR.rda")

##Right sided results 

#Model selection table

#Model coefficient table 

#Region-specific ests 



##Figures for Supplemental (region specific density ests)
######SERIOUSLY NEEDS CLEANING BUT THIS IS FOR GETTING THE STUDY AREA SPECIFIC DENSITY ESTIMATES -- FIGURES 
topmod.preds <- predict.oSCR(L.dens.imperv)
sum(topmod.preds[["ssN"]][[1]])
mean((topmod.preds[["ssN"]][[1]]))*400

L.dens.imperv
ssDF.top <- topmod.preds[["ssDF"]][[1]]
ssDF.top[c(1,2)] <- ssDF.top[c(1,2)]*1000

ssDF.top$ED.4 <- dens.df$estimate

hist(ssDF.top$ED.4)

ED.df <- ssDF.top[c(1,2,13)]  

ED.rast <- rasterFromXYZ(ED.df, crs = "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")




plot(topmod.preds[[pb]])


plot(topmod.preds[["pbar"]][[1]])
plot(topmod.preds[["r"]][[1]])

pq_trapbuff <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "PenasquitosTraps_3kmbuff")
plot(pq_trapbuff, add = TRUE)

WUI_trapbuff <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "WUITraps_3kmbuff")
plot(WUI_trapbuff, add = TRUE)

wildland_trapbuff <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "WildlandTraps_3kmbuff")
plot(wildland_trapbuff, add = TRUE)

wildland_traps <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "WildlandTraps")
plot(wildland_traps, add = TRUE, pch = 15)

WUI_traps <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "WUITraps")
plot(WUI_traps, add = TRUE, pch = 17)

urban_traps <- readOGR(dsn = "./../../../Processed Data/Spatial Data/Shapefiles", layer = "PenasquitosTraps")
plot(urban_traps, add = TRUE, pch = 16)




png("./../../../Tables and Figures/Figures/region_density_plot.png", width = 5, height = 5,unit = "in", res = 600)
plot(ED.rast, col = viridis::magma(10), bty = "n", box = FALSE)
plot(pq_trapbuff, add = TRUE, border = "white", lwd = 2)
plot(WUI_trapbuff, add = TRUE, border = "white", lwd = 2)
plot(wildland_trapbuff, add = TRUE, border = "white", lwd = 2)
plot(wildland_traps, add = TRUE, pch = 15, col = "white")
plot(WUI_traps, add = TRUE, pch = 17, col = "white")
plot(urban_traps, add = TRUE, pch = 16, col = "white")
# Close device
dev.off()


ggplot() +
  geom_raster(data = ED.rast , 
              aes(x = x, y = y, 
                  fill = ED.4)) +   
  scale_fill_viridis_c() +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()

ggplot() +
  geom_sf(data = WUI_trapbuff) +
  geom_sf(data = pq_trapbuff) +
  geom_sf(data = wildland_trapbuff) +
  geom_sf(data = WUI_traps) +
  geom_sf(data = pq_traps) +
  geom_sf(data = wildland_traps) +
  coord_sf()



pq_crop <- crop(ED.rast, extent(pq_trapbuff))
pq_mask <- mask(pq_crop, pq_trapbuff)
plot(pq_mask)
plot(pq_trapbuff,add=TRUE, lwd = 2, )


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

tiff(filename="./../../../Tables and Figures/Figures/region_plot.tiff",
     units="in",width = 5, height = 5, res = 800,compression="lzw")
all.maps
dev.off()


####Other Supp Figures

####LEFT#####


####gotta fix this for the workflow but fine for now I guess 
load("./../../../Processed Data/Spatial Data/unscaled_imperv.rda")
plot(imperv_500)

ssDF.imperv.unscaled <- extract.rast(L.dens.imperv[["ssDF"]],imperv_500, mult = 1000, cov.name = "imperv")

newdata = data.frame(session=factor(1),
                     imperv = L.dens.imperv[["ssDF"]][[1]]$imperv)
dens.df <- get.real(L.dens.imperv, type = "dens", d.factor = 4, newdata = newdata)

dens.df$unsc.imperv <-  ssDF.imperv.unscaled[[1]][["imperv"]]


newdata = data.frame(session=factor(1),
                     imperv = L.dens.imperv[["ssDF"]][[1]]$imperv,
                     elev = mean(L.dens.imperv[["ssDF"]][[1]]$elev),
                     rec_level = 0)

det.df <- get.real(L.dens.imperv, type = "det", newdata = newdata)

det.df$unsc.imperv <-  ssDF.imperv.unscaled[[1]][["imperv"]]


complementary("seagreen2")

dens.plot <- ggplot(dens.df, aes(x=unsc.imperv, y = estimate, ymin = lwr, ymax = upr))+
  geom_point(alpha = 0)+
  geom_ribbon(fill = "#4EEE94" ,alpha = 0.4) +
  geom_line(size = 1.5)+
  labs(x="% Impervious Surface", y = expression ("Density (bobcats/"~km^2~")"))+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18))


dens.plot

det.plot <- ggplot(det.df, aes(x=unsc.imperv, y = estimate, ymin = lwr, ymax = upr))+
  geom_ribbon(fill = "#EE4EA8",alpha = 0.2) +
  geom_line(size = 1.5)+
  geom_point(alpha = 0)+
  labs(x="% Impervious Surface", y = "Probability of Use")+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18))


det.plot


range(dens.df$estimate)


densdetplot.L <- ggarrange(det.plot, dens.plot, nrow = 1, ncol = 2,  labels = "auto",
                     align = c("hv"), vjust = 5)
densdetplot.L

ggsave("./../../../Tables and Figures/Figures/densdetimperv_plot_L.png",plot = densdetplot.L, width = 14, height = 6, dpi = 800)

tiff(filename="./../../../Tables and Figures/Figures/RPU_ED_plot.tiff",
     units="in",width = 14, height = 6, res = 800,compression="lzw")
densdetplot.L
dev.off()


#############
####RIGHT####
#############

###RPU ED fig for supp


#get sigma
sig.df <- get.real(model = R.dens.imperv, type = "sig")


#get detection
newdata.det = data.frame(session=factor(1),
                         imperv = 0,
                         elev = 0 ,
                         rec_level = 0)
det.df <- get.real(model = R.dens.imperv, type = "det", newdata = newdata.det)


###pull out ssDF
ssDF.top <- R.dens.imperv[["ssDF"]][[1]]

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

traps <- data.frame(R.dens.imperv[["scrFrame"]][["traps"]][[1]])

# test <- rasterFromXYZ(ssDF.top[c("X","Y","rpu")])
# plot(test)

elev.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=elev), alpha=0.8) +
  geom_point(data = traps, mapping = aes(x = X, y = Y)) +
  # scale_fill_viridis_c(name = "Elevation \n (m)") +
  scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",name = "Elevation \n (m)", direction = -1)+
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=12), legend.text = element_text(size=10)) 
#+ theme(legend.text=element_text(size=X))
elev.plot

imperv.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=imperv), alpha=0.8) +
  geom_point(data = traps, mapping = aes(x = X, y = Y)) +
  scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",name = "% Impervious  \n Surface", direction = -1)+
  # scale_fill_viridis_c(name = "% Impervious  \n Surface") +
  coord_equal() +
  theme_map() + 
  theme(legend.position="right",legend.title=element_text(size=12), legend.text = element_text(size=10)) 
imperv.plot

elev.rpu.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.elev*1000), alpha=0.8) +
  scale_fill_viridis_c(name = "RPU \n Elevation", labels = comma) +
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
        legend.key.size = unit(.3, 'cm')) 
elev.rpu.plot
imperv.rpu.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.imperv*1000), alpha=0.8) +
  scale_fill_viridis_c("RPU \n % Impervious \n Surface", labels = comma) +
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
        legend.key.size = unit(.3, 'cm')) 
imperv.rpu.plot

all.rpu.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.all*1000), alpha=0.8) +
  scale_fill_viridis_c(name = "Full RPU", labels = comma) +
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
        legend.key.size = unit(.3, 'cm')) 
all.rpu.plot

ed.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=ED), alpha=0.8) +
  scale_fill_viridis_c(option = "magma", name = bquote("E("~hat(D)~")"), labels = comma) +
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
        legend.key.size = unit(.3, 'cm')) 

ed.plot


all.maps <- ggarrange(elev.rpu.plot,imperv.rpu.plot,all.rpu.plot,ed.plot,nrow = 2, ncol = 2, labels = "auto",
                      align = c("hv"), vjust = 5)

all.maps

ggsave("./../../../Tables and Figures/Figures/RPU_ED_plot_rightsupp.png", width = 6, height = 5, dpi = 800)
tiff(filename="./../../../Tables and Figures/Figures/RPU_ED_plot_rightsupp.tiff",
     units="in",width = 6, height = 5, res = 800,compression="lzw")
all.maps
dev.off()


####Right Density /prob use / imperv relationship figure 
newdata = data.frame(session=factor(1),
                     imperv = R.dens.imperv[["ssDF"]][[1]]$imperv)
dens.df <- get.real(R.dens.imperv, type = "dens", d.factor = 4, newdata = newdata)

dens.df$unsc.imperv <-  ssDF.imperv.unscaled[[1]][["imperv"]]


newdata = data.frame(session=factor(1),
                     imperv = R.dens.imperv[["ssDF"]][[1]]$imperv,
                     elev = mean(R.dens.imperv[["ssDF"]][[1]]$elev),
                     rec_level = 0)

det.df <- get.real(R.dens.imperv, type = "det", newdata = newdata)

det.df$unsc.imperv <-  ssDF.imperv.unscaled[[1]][["imperv"]]


complementary("thistle")

dens.plot <- ggplot(dens.df, aes(x=unsc.imperv, y = estimate, ymin = lwr, ymax = upr))+
  geom_point(alpha = 0)+
  geom_ribbon(fill = "#D8BFD8" ,alpha = 0.4) +
  geom_line(size = 1.5)+
  labs(x="% Impervious Surface", y = expression ("Density (bobcats/"~km^2~")"))+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18))


dens.plot

det.plot <- ggplot(det.df, aes(x=unsc.imperv, y = estimate, ymin = lwr, ymax = upr))+
  geom_ribbon(fill = "#BFD8BF",alpha = 0.2) +
  geom_line(size = 1.5)+
  geom_point(alpha = 0)+
  labs(x="% Impervious Surface", y = "Probability of Use")+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18))


det.plot


range(dens.df$estimate)


densdetplot.R <- ggarrange(det.plot, dens.plot, nrow = 1, ncol = 2,  labels = "auto",
                           align = c("hv"), vjust = 5)
densdetplot.R

ggsave("./../../../Tables and Figures/Figures/densdetimperv_plot_R.png",plot = densdetplot.R, width = 14, height = 6, dpi = 800)
getwd()

