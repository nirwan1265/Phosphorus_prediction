library(purrr)
library(tidyverse)
library(randomForest)
library(ranger)
library(plotbiomes)
#remotes::install_github("valentinitnelav/plotbiomes")
library(ggpubr)
library(oce)
library(gridBase)
library(grid)
library(maps)
library(mapdata)
library(ochRe)
library(scales)
library(ggsci)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(maptools)
library(maps)
library(rworldmap)
library(countrycode)

#read database in a .csv file
stp.dat <- read.csv("data/raw.data.csv")
colnames(stp.dat)

# Adding country and filter by continent
stpcountry <- data.frame( 
  Total_P = stp.dat$TOTAL_P,
  LATITUDE = stp.dat$LATITUDE,
  LONGITUDE = stp.dat$LONGITUDE,
  MAT = stp.dat$MAT,
  MAP= stp.dat$MAP,
  AI= stp.dat$AI,
  ELEVATION = stp.dat$ELEVATION,
  VEGETATION = stp.dat$VEGETATION,
  DEPTH = stp.dat$DEPTH,
  CLAY = stp.dat$CLAY,
  SILT = stp.dat$SILT,
  SAND = stp.dat$SAND,
  BDENSITY = stp.dat$BDENSITY,
  PH = stp.dat$PH,
  CEC = stp.dat$CEC,
  SOC = stp.dat$SOC,
  TN = stp.dat$TN,
  C_N = stp.dat$C_N,
  mapname = map.where(database="world", 
                      stp.dat$LONGITUDE, stp.dat$LATITUDE)) %>%
  inner_join(iso3166) %>%
  inner_join(countryRegions, by =c(a3="ISO3")) #%>% 
  #dplyr::select(LATITUDE,LONGITUDE,Total_P, a2,a3,ISOname, GEO3major,REGION, continent, GEO3)



table(stpcountry$GEO3major)
stp.dat <- stpcountry[which(stpcountry$continent == "Africa"), ]



#Figure 1 in main text: site-level data distribution
stp.point.distribution.plot <- 
  ggplot()+
  geom_polygon(data=map_data("world"),
               aes(x=long, y=lat, group=group),
               fill="gray50",colour="white",size=0.1)+
  ylim(-66, 90)+
  theme(panel.background = element_rect(fill="white"))+
  theme(axis.text=element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank())+
  geom_point(data=stp.dat,
             aes(x=LONGITUDE,y=LATITUDE , color=Total_P),
             size=3, alpha=0.7)+
  scale_color_distiller(palette="Reds",
                        name = expression(paste("Soil total P (mg kg"^-1,")")),
                        direction = 1, 
                        breaks=c(10, 250, 500, 1000, 1500, 2000),
                        label=c("10", "250", "500", "1000", 
                                "1500", ">2000"),
                        limits=c(0, 2100))+
  theme(legend.position=c(0.15,0.26),
        plot.margin = unit(c(0,0,2,0), "mm"),
        text = element_text(size = 10),
        legend.box.background = element_rect(fill = NA, 
                                             colour = NA),
        legend.key = element_rect(fill = NA, colour = NA))


plot(stp.point.distribution.plot)

#hist
stp.dat <- filter(stp.dat, DEPTH<100)
stp.dat$DEPTH.BIN <- stp.dat$DEPTH
stp.dat$DEPTH.BIN[stp.dat$DEPTH<=30] <- "0-30 cm"
stp.dat$DEPTH.BIN[stp.dat$DEPTH > 30] <- "30-100 cm"
stp.dat$DEPTH.BIN <- as.factor(stp.dat$DEPTH.BIN)
tp.his.plot <- 
  ggplot(stp.dat, aes(x=Total_P, fill=DEPTH.BIN))+
  geom_histogram(colour="black", binwidth = 120)+
  geom_vline(aes(xintercept=mean(Total_P)),
             color="firebrick", linetype="dashed", size=1.2)+
  labs(x=expression(paste("Soil total P (mg kg"^-1,")")),
       y="Count")+
  scale_x_continuous(breaks = c(0, 570,3000, 
                                5000, 7000, 9000),
                     label = c("", "570",  "3000",
                               "5000","7000", "9000"))+
  scale_y_continuous(breaks = c(1, 50,   150,  400, 700),
                     label = c("1", "50",  "150",
                               "400", "700"))+
  theme_test()+
  ggsci::scale_fill_jco()+
  theme(legend.position=c(0.75,0.8),
        plot.margin = unit(c(1.6, .3, .5, 0), "cm"),
        text = element_text(size = 11))+
  guides(fill=guide_legend(title="Soil depth"))

plot(tp.his.plot)

world.hist.plots <-  
  ggarrange(stp.point.distribution.plot, 
            tp.his.plot,
            nrow = 1, widths = c(3,1.2),
            labels = c("A","B")) 


plot(world.hist.plots)

point_biomes <-  
  whittaker_base_plot()+
  scale_fill_jco()+
  geom_point(data = stp.dat, aes(x=MAT,  y=MAP/10),
             size = 2, color="black", fill="tomato2", shape=21 )+
  theme_test()+
  theme(legend.background = element_blank(),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        legend.position=c(0.25,0.71))

plot(point_biomes)


stp.dat.2 <- stp.dat %>% group_by(SOIL_ORDER) %>% 
  dplyr::mutate(SOIL_ORDER_count=n())

soil.order.count.plot <-  
  stp.dat.2 %>% filter(SOIL_ORDER != "Others") %>%
  ggplot(aes(x=reorder(SOIL_ORDER, -SOIL_ORDER_count), fill=DEPTH.BIN))+
  geom_bar()+
  xlab("Soil order")+ylab("Count")+
  scale_fill_jco()+
  theme_test()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 12))



stp.dat.2 <- stp.dat %>% group_by(BEDROCK) %>% 
  dplyr::mutate(count.bedrock=n())

bedrock.count.plot <- 
  stp.dat.2 %>% filter(BEDROCK != "ND" & BEDROCK != "IG" & BEDROCK != "WB") %>%
  ggplot(aes(x=reorder(BEDROCK, -count.bedrock), fill=DEPTH.BIN))+
  geom_bar()+
  xlab("Parental material")+ylab("Count")+
  scale_fill_jco()+
  theme_test()+
  theme(legend.position="none",
        text = element_text(size = 12))




count.plots <-  
  ggarrange(soil.order.count.plot, 
            bedrock.count.plot,
            nrow = 2, heights = c(1, .9),
            labels = c("D", "E")) 

point.count.plots <- 
  ggarrange(point_biomes, 
            count.plots,
            nrow = 1, 
            labels = c("C")) 


global.biomes.point.count.plots <-  
  ggarrange(world.hist.plots, 
            point.count.plots,
            nrow = 2, heights = c(1, 1.15),
            labels = c("")) 



ggsave("global.biomes.point.count.plots.tiff", 
       global.biomes.point.count.plots,
       width = 280, height = 250, units = "mm",
       dpi =  300)





#Figure 2 Soil total P concentration in relation to 
# parent material, biome, and soil weathering extent.

stp.dat$weather.stage <- stp.dat$SOIL_ORDER
stp.dat$weather.stage <- as.character(stp.dat$weather.stage)
stp.dat <- stp.dat[!(stp.dat$weather.stage == "Others"), ]
stp.dat$weather.stage[stp.dat$weather.stage == 'Geilsols'] <- 'Slightly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Histosols'] <- 'Slightly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Spodosols'] <- 'Strongly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Andisols'] <- 'Slightly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Oxisols'] <- 'Strongly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Vertisols'] <- 'Intermediately'
stp.dat$weather.stage[stp.dat$weather.stage == 'Aridisols'] <- 'Intermediately'
stp.dat$weather.stage[stp.dat$weather.stage == 'Ultisols'] <- 'Strongly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Mollisols'] <- 'Intermediately'
stp.dat$weather.stage[stp.dat$weather.stage == 'Alfisols'] <- 'Intermediately'
stp.dat$weather.stage[stp.dat$weather.stage == 'Inceptisols'] <- 'Slightly'
stp.dat$weather.stage[stp.dat$weather.stage == 'Entisols'] <- 'Slightly'

soil.stage.violin <- 
  ggplot(stp.dat, aes(x = reorder(weather.stage, -Total_P ), 
                     y = Total_P , fill=weather.stage)) + 
  geom_violin(trim = F, width=.8, col="black",
              show.legend=F, position = "dodge",
              scale = "area")+
  ggpubr::fill_palette("npg")+
  geom_boxplot(width=0.2, color="grey90", alpha=.5)+
  theme_pubclean()+
  labs(x="Soil weathered extent",
       y=expression(paste("Soil total P (mg kg"^-1,")")))+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(3,3,3,3), "mm"),
        legend.position = "none",
        axis.ticks.margin=unit(-1, "cm"))+
  scale_y_continuous(limits=c(0, 1500), 
                     expand = c(0, 20),
                     breaks = seq(0, 1500, 250)) 


biome.violin <- 
  ggplot(stp.dat, aes(x = reorder(BIOMES, -Total_P), 
                     y = Total_P, fill=BIOMES)) + 
  geom_violin(trim = F, width=.8, col="black",
              show.legend=F, position = "dodge",
              scale = "area")+
  ggpubr::fill_palette("aaas")+
  geom_boxplot(width=0.2, color="grey90", alpha=.5)+
  theme_pubclean()+
  labs(x="Biomes",
       y=expression(paste("Soil total P (mg kg"^-1,")")))+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(3,3,3,3), "mm"),
        legend.position = "none",
        axis.ticks.margin=unit(-1, "cm"))+
  scale_y_continuous(limits=c(0, 1500), 
                     expand = c(0, 20),
                     breaks = seq(0, 1500, 250)) 



bedrock.violin <-  
  stp.dat %>% filter(BEDROCK  != "ND" & BEDROCK  != "IG" & BEDROCK  != "WB" &
                       BEDROCK  != "PB" & BEDROCK  != "PI" & BEDROCK  != "PY" &
                       BEDROCK  != "VA" & BEDROCK  != "VI"& BEDROCK  != "EV"& BEDROCK  != "OTHERS") %>% 
  dplyr::mutate(BEDROCK =factor(BEDROCK , levels = c("SC", "VB", "SM", "SU", "SS", 
                                                   "PA", "MT"))) %>% 
  ggplot( aes(x = BEDROCK , 
              y = Total_P, fill=BEDROCK )) + 
  geom_violin(trim = F, width=.8, col="black",
              show.legend=F, position = "dodge",
              scale = "area")+
  ggpubr::fill_palette("jco")+
  geom_boxplot(width=0.2, color="grey90", alpha=.5)+
  theme_pubclean()+
  labs(x="Parental material",
       y=expression(paste("Soil total P (mg kg"^-1,")")))+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(3,3,3,3), "mm"),
        legend.position = "none",
        axis.ticks.margin=unit(-1, "cm"))+
  scale_y_continuous(limits=c(0, 1500), 
                     expand = c(0, 20),
                     breaks = seq(0, 1500, 250)) 


violin.plots <- 
  cowplot::plot_grid(bedrock.violin, biome.violin, 
                     soil.stage.violin, 
            labels = c("A", "B", "C"),
            label_size=12,
            nrow= 3)


ggsave("violin.plots.tiff", 
       violin.plots,
       width = 200, height = 300, units = "mm",
       dpi =  300)


#Figure 3 Results of the random forest model predicting soil total P concentration.

##random forest model
rf.dat <- read.csv("data/rf.dat.csv")

rf.dat$BEDROCK <- as.factor(rf.dat$BEDROCK)
rf.dat$SOIL.TYPE <- as.factor(rf.dat$SOIL.TYPE)
rf.dat$BIOMES <- as.factor(rf.dat$BIOMES)
rf.dat <- rf.dat[complete.cases(rf.dat),]
colnames(rf.dat)
set.seed(111)
output.rf<- randomForest(Total_P ~ .,  
                         ntree=500, keep.forest=T,
                         importance=T, mtry=3, 
                         data = rf.dat[,-c(1,2,3)])
output.rf
imp.dat <- randomForest::importance(output.rf, scale=T)
predict.train <- predict(output.rf, data = rf.dat)
predicted.dat <- data.frame(predict.train, rf.dat)

#importance plot
imp.dat <- as.data.frame(imp.dat)
imp.dat$Predictors <- c("SOC","Sand", "Clay", "Depth",
                        "MAT","MAP","PH","NPP","SLOPE",
                        "ELEVATION", "BIOMES", "BEDROCK",
                        "SOIL.TYPE", "WRB.SOIL.TYPE"
                        )
names(imp.dat) <- c("IncMSE", "IncNodePurity", "Predictors")
imp.dat <- mutate(imp.dat, Predictor.type=Predictors)

imp.dat$Predictor.type[imp.dat$Predictor.type == "Parent material"] <- "Parent material"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Soil order"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Depth"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Biome"] <- "Climate"
imp.dat$Predictor.type[imp.dat$Predictor.type == "MAT"] <- "Climate"
imp.dat$Predictor.type[imp.dat$Predictor.type == "MAP"] <- "Climate"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Clay"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Sand"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "pH"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "SOC"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "NPP"] <- "Vegetation"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Slope"] <- "Topography"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Elevation"] <- "Topography"

imp.dat$Predictor.type <- factor(imp.dat$Predictor.type, 
                                 levels = c("Soil", 
                                            "Parent material",
                                            "Climate",
                                            "Topography",
                                            "Vegetation"))

p.imp <-  
  ggplot()+
  geom_bar(data = imp.dat, 
           aes(x= reorder(Predictors, IncMSE), 
               y=IncMSE, fill=Predictor.type),
           stat = "identity")+
  scale_fill_manual(values=c("#3C5488FF", "#F39B7FFF", "#4DBBD5FF",
                             "#8491B4FF","#00A087FF"))+
  labs(x="Predictors",
       y="%IncMSE",
       title = "") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105),
                     breaks = c(0, 25, 50, 75, 100))+
  theme_test()+
  coord_flip()+
  theme(plot.margin = unit(c(0,.5,.2,0), "cm"),
        legend.position = c(0.75, 0.2),
        text = element_text(size = 14),
        axis.text.y = element_text(size = 12))+
  guides(fill=guide_legend(title="Predictor category"))



#predict vs observed plot
get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

prec.obs.plot <- predicted.dat %>% 
  select(predict.train, Total_P) %>% 
  mutate(Density = get_density(predict.train, Total_P, n = 200))

prec.obs.plot <- 
  ggplot(data=predicted.dat, aes(x=predict.train, y=Total_P))+
  geom_hex(binwidth=c(50,50), na.rm = T, alpha=.7, show.legend=T)+
  scale_fill_gradient(low = "red", high = "yellow")+
  geom_smooth(method = 'lm', se = T)+
  geom_abline(aes(slope=1, intercept=0), 
              color="grey50", size=1, linetype="longdash")+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 4150),
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 4150),
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000))+
  labs(x=expression(paste("Predicted soil total P (mg kg"^-1,")")), 
       y=expression(paste("Observed soil total P (mg kg"^-1,")")))+
  theme_test()+
  theme(plot.margin = unit(c(0.67,0,0,0), "cm"),
        text = element_text(size = 14),
        legend.position = c(0.85,0.25))+
  ggpubr::stat_cor(aes(label=paste(..rr.label..,sep="~','~")),
                   label.x = 300, label.y = 3700, size=5)

library(cowplot)
importance.accuracy.plots <-  
  plot_grid(p.imp, 
            prec.obs.plot,
            labels = c("A", "B"),label_size=12,
            nrow= 1)

ggsave("importance.accuracy.plots.tiff", importance.accuracy.plots, 
       width = 28, height = 13, units = "cm", scale = 1, dpi = 300)

#Fig. 4 Partial dependence plots showing the dependence of soil total P concentration on predictors.
library(tidyverse)
library(mlbench)
library(randomForest)
library(caret)
library(edarf)

# Add predicted values to data frame
df_rf <- rf.dat %>% 
  mutate(predicted = predict(output.rf))
# Get performance measures
postResample(df_rf$predicted, df_rf$Total_P)
# Get variable importance measures
imp_df <- data.frame(randomForest::importance(output.rf, 
                                              scale = FALSE, type = 1))
# Tidy up and sort the data frame
imp_df <- imp_df %>% 
  mutate(names = rownames(imp_df)) %>% 
  arrange(desc( X.IncMSE))
# Plot mean decreased accuracy
imp_df %>% 
  top_n(10,  X.IncMSE) %>% 
  ggplot(aes(x = reorder(names,  X.IncMSE),
             y =  X.IncMSE)) +
  geom_col() +
  coord_flip() +
  labs(title = "Variable Importance, Sonar Dataset",
       subtitle = "Random Forests (N = 500)",
       x= "",
       y= "Mean Decrease in Accuracy",
       caption = "sethdobson.netlify.com") +
  theme(plot.caption = element_text(face = "italic"))
# Save top predictor names as character vector
nm <- as.character(imp_df$names)
# Get partial depedence values for top predictors
pd_df <- partial_dependence(fit = output.rf.total_p.1,
                            vars = nm,
                            data = df_rf,
                            n = c(100, 200))
# Plot partial dependence using edarf
plot_pd(pd_df)
glimpse(pd_df)

pdp.mat <-  
  ggplot(data=pd_df, aes(x=MAT, y=Total_P))+
  geom_point(size=3)+
  geom_line()+
  ggpubr::color_palette("npg")+
  labs(x="MAT (°C)",
       y="")+
  theme_minimal()+
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(3,3,3,3), "mm"),
        legend.position = "none")



#Fig. 5 Global maps of total P concentration in the 0-30 cm and 30-100 cm of soils. 

covstack.dat <- read.csv("covstack.dat.csv")
covstack.dat$BEDROCK <- as.factor(covstack.dat$BEDROCK)
covstack.dat$SOIL.TYPE <- as.factor(covstack.dat$SOIL.TYPE)
covstack.dat$BIOMES <- as.factor(covstack.dat$BIOMES)
covstack.dat <- na.omit(covstack.dat)
library(ranger)
output.ranger <- ranger(Total_P ~ ., keep.inbag = T, mtry = 3,
                             num.trees = 500, 
                             importance = "permutation",
                             data = rf.dat[,-c(1,2)])


names(covstack.dat) <- c("x", "y","SOC", 
                         "MAP", "MAT", 
                         "SAND", "CLAY", "PH",
                         "BEDROCK", "SOIL.TYPE","DEPTH",
                         "NPP", "SLOPE", "ELEVATION",
                         "BIOMES")
covstack.dat$BEDROCK <- as.factor(covstack.dat$BEDROCK)
covstack.dat$SOIL.TYPE <- as.factor(covstack.dat$SOIL.TYPE)
covstack.dat$BIOMES <- as.factor(covstack.dat$BIOMES)

covstack.dat$DEPTH <- 5
map.RF.stp.5 <- predict(output.ranger, covstack.dat)

stp.prediction.5.dat <- data.frame(covstack.dat[, c(1:2)], 
                                   map.RF.stp.5$predictions)


covstack.dat$DEPTH <- 15
map.RF.stp.15 <- predict(output.ranger, covstack.dat)

stp.prediction.15.dat <- data.frame(covstack.dat[, c(1:2)], 
                                    map.RF.stp.15$predictions)

covstack.dat$DEPTH <- 25
map.RF.stp.25 <- predict(output.ranger, covstack.dat)

stp.prediction.25.dat <- data.frame(covstack.dat[, c(1:2)], 
                                    map.RF.stp.25$predictions)

covstack.dat$DEPTH <- 65
map.RF.stp.65 <- predict(output.ranger, covstack.dat)

stp.prediction.65.dat <- data.frame(covstack.dat[, c(1:2)], 
                                    map.RF.stp.65$predictions)


stp.prediction.0_30.dat <- (stp.prediction.5.dat * 10+
                            stp.prediction.15.dat * 10+
                            stp.prediction.25.dat *10)/30

# plot global soil total P concentration in topsoil (0-30 cm) and subsoil (30-100 cm)
stp.030.plot <- 
  ggplot() +
  geom_raster(data=stp.prediction.0_30.dat, 
              aes(x=x, y=y,fill=map.RF.stp.5.predictions)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("Soil total P (mg kg"^-1,")")),
                       limits=c(100, 1400),
                       breaks=c(100,  800, 1400),
                       label=c("100", "800",  "1400")) +
  labs(title = expression(paste("Soil total P concentration in 0~30 cm")),
       x=" ",   y=" ",
       fill = "stp.av15") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40,
                                60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA)) 




stp.30100.plot <- 
  ggplot() +
  geom_raster(data=stp.prediction.65.dat, 
              aes(x=x, y=y,fill=map.RF.stp.65.predictions)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey80",
                       name = expression(paste("Soil total P (mg kg"^-1,")")),
                       limits=c(100, 1400),
                       breaks=c(100,  800, 1400),
                       label=c("100", "800","1400")) +
  labs(x=" ",  y=" ",
       title = expression(paste("Soil total P concentration in 30~100 cm")),
       fill = "tp.raster.65") +
  scale_y_continuous(expand = c(0,0), limits = c(-65,85),
                     breaks = c(-60, -40, -20, 0, 20, 40, 60, 80),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", "60°N", "80°N")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(-120, -60,  0,  60, 120),
                     label = c("120°W", "60°W",  
                               "0", "60°E", "120°E"))+
  theme(text = element_text(size = 16),
        legend.title = element_text(size=12),
        legend.key.size = unit(.7, "cm"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = c(0.13,0.3),
        plot.margin = unit(c(0,2,1,1), "mm"),
        legend.background = element_rect(fill=NA)) 


latitudial.stp.030 <-  
  ggplot(stp.prediction.0_30.dat ,aes(x=y, 
                                      y=map.RF.stp.5.predictions))+
  geom_hex(size=.1, na.rm = T, 
           alpha=.8, show.legend=FALSE)+
  geom_smooth(method = "loess",  se=F, color = "red")+
  labs(title = '', 
       x = '', 
       y = expression(paste("Soil total P (mg kg"^-1,")")))+
  scale_x_continuous(breaks = c(-60, -40, -20, 0, 20, 
                                40, 60, 80),
                     limits = c(-65,85),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N", 
                               "60°N", "80°N")) + 
  scale_y_continuous(breaks = c(500, 1000),
                     limits = c(100,1400),
                     label = c("500", "1000")) + 
  theme_test()+
  theme(text = element_text(size = 18),
        axis.title = element_text(size = 14),
        plot.margin = unit(c(0,1,0,0), "mm"))+
  annotate(geom="text", x=85, y=1200, 
           label="0~30 cm", size=4.5,
           color="black")+
  coord_flip()





latitudial.stp.30100 <-  
  ggplot(stp.prediction.65.dat ,aes(x=y, y=map.RF.stp.65.predictions))+
  geom_hex(size=.1, na.rm = T, alpha=.8, show.legend=FALSE)+
  geom_smooth(method = "loess", se=F, color = "red")+
  labs(title = '', 
       x = '', 
       y = expression(paste("Soil total P (mg kg"^-1,")")))+
  scale_x_continuous(breaks = c(-60, -40, -20, 0, 20, 40, 60, 80),
                     limits = c(-65,85),
                     label = c("60°S", "40°S", "20°S", 
                               "0", "20°N", "40°N",
                               "60°N", "80°N")) + 
  scale_y_continuous(breaks = c(500, 1000),
                     limits = c(100,1400),
                     label = c("500", "1000")) + 
  theme_test()+
  theme(text = element_text(size = 18),
        axis.title = element_text(size = 14),
        plot.margin = unit(c(0,1,0,0), "mm"))+
  annotate(geom="text", x=85, y=1200, 
           label="30~100 cm", size=4.5,
           color="black")+
  
  coord_flip()

stp.030.global.latitudinal <- 
  ggarrange(stp.030.plot, latitudial.stp.030,
            nrow = 1, widths = c(3, 1), 
            labels = c("A","B"),
            font.label = list(size = 14)) 

stp.30100.global.latitudinal <-  
  ggarrange(stp.30100.plot, latitudial.stp.30100,
            nrow = 1, widths = c(3, 1),
            labels = c("C","D"),
            font.label = list(size = 14)) 


stp.global.latitudianl.plots <-   
  plot_grid(stp.030.global.latitudinal, 
            stp.30100.global.latitudinal,
            ncol=1)

ggsave("stp.global.latitudianl.plots.tiff", 
       stp.global.latitudianl.plots, 
       width = 37, height = 26, units = "cm",
       scale = 1, dpi = 100)

