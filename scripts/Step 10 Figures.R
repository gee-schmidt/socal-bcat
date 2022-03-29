#Step 10 figures for MS

#load oscr package and other packages
library(oSCR)
library(ggplot2)
library(raster)
library(scales)
library(ggthemes)
library(rgdal)
library(maptools)
library(paletteer)
library(ggpubr)
library(here)


#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

#load models 
load("./../../../Results/Model Outputs/Density Mods/socalbobcat_densitymodels_LR.rda")

##map figures for MS made in GIS 

###############################
#State space RPU figure for MS#
###############################

#get sigma
sig.df <- get.real(model = L.dens.imperv, type = "sig")


#get detection
newdata.det = data.frame(session=factor(1),
                         imperv = 0,
                         elev = 0 ,
                         rec_level = 0)
det.df <- get.real(model = L.dens.imperv, type = "det", newdata = newdata.det)


###pull out ssDF
ssDF.top <- L.dens.imperv[["ssDF"]][[1]]

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

traps <- data.frame(L.dens.imperv[["scrFrame"]][["traps"]][[1]])

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
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=ED*4), alpha=0.8) +
  scale_fill_viridis_c(option = "magma", name = bquote("E("~hat(D)~")"), labels = comma) +
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
        legend.key.size = unit(.3, 'cm')) 

ed.plot


all.maps <- ggarrange(elev.rpu.plot,imperv.rpu.plot,all.rpu.plot,ed.plot,nrow = 2, ncol = 2, labels = "auto",
                      align = c("hv"), vjust = 5)

all.maps

ggsave("./../../../Tables and Figures/Figures/RPU_ED_plot.png", width = 6, height = 5, dpi = 800)
tiff(filename="./../../../Tables and Figures/Figures/RPU_ED_plot.tiff",
     units="in",width = 6, height = 5, res = 800,compression="lzw")
all.maps
dev.off()


###et voila a figure 
