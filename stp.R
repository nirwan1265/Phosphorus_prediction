#Figure 3 Results of the random forest model predicting soil total P concentration.

##random forest model
rf.dat <- read.csv("data/rf.dat.csv")
# with soil errosion
rf.dat <- read.csv("data/Total_P_errosion.csv")
rf.dat <- rf.dat[complete.cases(rf.dat), ]
colnames(rf.dat)
str(rf.dat)


# Adding country and filter by continent
stpcountry <- data.frame( 
  LONGITUDE = rf.dat$LONGITUDE,
  LATITUDE = rf.dat$LATITUDE,
  Total_P = rf.dat$Total_P,
  SOC = rf.dat$SOC,
  SAND = rf.dat$SAND,
  CLAY = rf.dat$CLAY,
  DEPTH = rf.dat$DEPTH,
  MAT = rf.dat$MAT,
  MAP= rf.dat$MAP,
  PH = rf.dat$PH,
  NPP = rf.dat$NPP,
  SLOPE = rf.dat$SLOPE,
  ELEVATION = rf.dat$ELEVATION,
  BIOMES = rf.dat$BIOMES,
  BEDROCK = rf.dat$BEDROCK,
  SOIL_TYPE = rf.dat$SOIL_TYPE,
  WRB_SOIL_TYPE = rf.dat$WRB_SOIL_TYPE,
  ERROSION = rf.dat$Errosion,
  mapname = map.where(database="world", 
                      rf.dat$LONGITUDE, rf.dat$LATITUDE)) %>%
  inner_join(iso3166) %>%
  inner_join(countryRegions, by =c("a3"="ISO3"))

table(stpcountry$GEO3major)
# 1:17 without errosion and 1:18 with errosion data
stpcountry <- stpcountry[which(stpcountry$GEO3major %in% c("Africa", "Latin America and the Caribbean")), ] %>% dplyr::select(1:18)

rf.dat <- stpcountry
colnames(rf.dat)
str(rf.dat)
rf.dat$BEDROCK <- as.factor(rf.dat$BEDROCK)
rf.dat$SOIL_TYPE <- as.factor(rf.dat$SOIL_TYPE)
rf.dat$BIOMES <- as.factor(rf.dat$BIOMES)
rf.dat$WRB_SOIL_TYPE <- as.factor(rf.dat$WRB_SOIL_TYPE)

rf.dat <- rf.dat[complete.cases(rf.dat),]


set.seed(111)
output.rf<- randomForest(Total_P ~ .,  
                         ntree=500, keep.forest=T,
                         importance=T, mtry=3, 
                         data = rf.dat[,-c(1,2)])
output.rf
imp.dat <- randomForest::importance(output.rf, scale=T)
predict.train <- predict(output.rf, data = rf.dat)
predicted.dat <- data.frame(predict.train, rf.dat)

#importance plot
imp.dat <- as.data.frame(imp.dat)

            
imp.dat$Predictors <- c("SOC","Sand", "Clay", "Depth",
                        "MAT","MAP","pH","NPP","Slope",
                        "Elevation", "Biomes", "Bedrock",
                        "Soil type", "WRB soil type","Errosion"
)
names(imp.dat) <- c("IncMSE", "IncNodePurity", "Predictors")
imp.dat <- mutate(imp.dat, Predictor.type=Predictors)

imp.dat$Predictor.type[imp.dat$Predictor.type == "Parent material"] <- "Parent material"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Soil order"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Soil type"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "WRB soil type"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Depth"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Biomes"] <- "Climate"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Bedrock"] <- "Topography"
imp.dat$Predictor.type[imp.dat$Predictor.type == "MAT"] <- "Climate"
imp.dat$Predictor.type[imp.dat$Predictor.type == "MAP"] <- "Climate"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Clay"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Sand"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "pH"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "SOC"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "NPP"] <- "Vegetation"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Slope"] <- "Topography"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Elevation"] <- "Topography"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Errosion"] <- "Soil"

imp.dat$Predictor.type <- factor(imp.dat$Predictor.type, 
                                 levels = c("Soil", 
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
  dplyr::select(predict.train, Total_P) %>% 
  dplyr::mutate(Density = get_density(predict.train, Total_P, n = 200))

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

ggsave("Stp_Africa_LatinAmerica_Errosion.tiff", importance.accuracy.plots, 
       width = 28, height = 13, units = "cm", scale = 1, dpi = 300)
