#Step 8b Density model selection


#setwd first
setwd("E:/Socal Bobcat Reproducible Research Folder")

#load oscr package and other packages
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

traps <- data.frame(L.dens.imperv[["scrFrame"]][["traps"]][[1]])

test <- rasterFromXYZ(ssDF.top[c("X","Y","rpu")])
plot(test)

elev.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=elev), alpha=0.8) +
  geom_point(data = traps, mapping = aes(x = X, y = Y)) +
  # scale_fill_viridis_c(name = "Elevation /n (m)") +
  scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",name = "Elevation /n (m)", direction = -1)+
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=12), legend.text = element_text(size=10)) 
#+ theme(legend.text=element_text(size=X))
elev.plot

imperv.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=imperv), alpha=0.8) +
  geom_point(data = traps, mapping = aes(x = X, y = Y)) +
  scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",name = "% Impervious  /n Surface", direction = -1)+
  # scale_fill_viridis_c(name = "% Impervious  /n Surface") +
  coord_equal() +
  theme_map() + 
  theme(legend.position="right",legend.title=element_text(size=12), legend.text = element_text(size=10)) 
imperv.plot

elev.rpu.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.elev*1000), alpha=0.8) +
  scale_fill_viridis_c(name = "RPU /n Elevation", labels = comma) +
  coord_equal() +
  theme_map() +
  theme(legend.position="right",legend.title=element_text(size=6), legend.text = element_text(size=6),
        legend.key.size = unit(.3, 'cm')) 
elev.rpu.plot
imperv.rpu.plot <- ggplot() +  
  geom_tile(data=ssDF.top, aes(x=X, y=Y, fill=rpu.imperv*1000), alpha=0.8) +
  scale_fill_viridis_c("RPU /n % Impervious /n Surface", labels = comma) +
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

getwd()
ggsave("2022_0119_RPU_ED_plot.png", width = 6, height = 5, dpi = 800)
tiff(filename="C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Code_and_Data/2022_0119_RPU_ED_plot.tiff",
     units="in",width = 6, height = 5, res = 800,compression="lzw")
all.maps
dev.off()


all.maps.2 <- ggarrange(elev.rpu.plot,imperv.rpu.plot,all.rpu.plot,ed.plot,nrow = 2, ncol = 2, labels = "auto",
                        align = c("hv"), vjust = 5)

all.maps

# points(traps)

coordinates(traps) <- 1:2

L.rsf.imperv

exp(-5.098)

###


load("C:/Users/Greta/Desktop/SDSU Fall 2020/Julia_Bobcat_Paper/R_Data_Inputs/2022_0203_imperv_500m_unscaled.rda")


ssDF.imperv.unscaled <- extract.rast(L.dens.imperv[["ssDF"]],imperv_500, mult = 1000, cov.name = "imperv")

newdata = data.frame(session=factor(1),
                     imperv = ssDF.top$imperv)
dens.df <- get.real(L.dens.imperv, type = "dens", d.factor = 4, newdata = newdata)

dens.df$unsc.imperv <-  ssDF.imperv.unscaled[[1]][["imperv"]]


newdata = data.frame(session=factor(1),
                     imperv = ssDF.top$imperv,
                     elev = mean(ssDF.top$elev),
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
dens.plot.2 <- ggMarginal(dens.plot,
                          type = "histogram", 
                          xparams = list(  bins=10),
                          yparams = list(bins = 10),
                          position = "identity",
                          theme(axis.line = element_blank()))
dens.plot.2
ggsave("2022_0107_densimperv_plot.png",plot = dens.plot.2, width = 10, height = 9, dpi = 800)
getwd()

range(dens.df$estimate)


all.maps.2 <- ggarrange(det.plot, dens.plot, elev.rpu.plot,imperv.rpu.plot,all.rpu.plot,ed.plot,nrow = 3, ncol = 2, labels = "auto",
                        align = c("hv"))

all.maps.2

plots.2 <- ggarrange(det.plot, dens.plot, nrow = 1, ncol = 2,  labels = "auto",
                     align = c("hv"), vjust = 5)
plots.2

ggsave("2022_0203_densdetimperv_plot.png",plot = plots.2, width = 14, height = 6, dpi = 800)
getwd()






###RIGHT model evaluation