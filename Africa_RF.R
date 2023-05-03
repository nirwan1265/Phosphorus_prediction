#Figure 3 Results of the random forest model predicting soil total P concentration.

##random forest model
rf.dat <- read.csv("data/rf.dat.csv")
rf.dat <- rf.dat[complete.cases(rf.dat), ]

stpcountry <- data.frame( 
  NO = rf.dat$NO,
  LONGITUDE = rf.dat$LONGITUDE,
  LATITUDE = rf.dat$LATITUDE,
  Total_P = rf.dat$TOTAL_P,
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
  SOIL.TYPE = rf.dat$SOIL.TYPE,
  WRB.SOIL.TYPE = rf.dat$WRB.SOIL.TYPE,
  mapname = map.where(database="world", 
                      rf.dat$LONGITUDE, rf.dat$LATITUDE)) %>%
  inner_join(iso3166) %>%
  inner_join(countryRegions, by =c(a3="ISO3"))



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


