#Figure Results of the random forest model predicting soil total P concentration.

##random forest model
rf.dat <- read.csv("data/P_Olsen_mcdowell2023_predictors_he2022_LAC_AFR.csv")
rf.dat <- rf.dat[complete.cases(rf.dat), ]
#rf.dat <- rf.dat[which(rf.dat$OlsenP <= 100), ]
colnames(rf.dat)
str(rf.dat)

rf.dat$BEDROCK <- as.factor(rf.dat$BEDROCK)
rf.dat$SOIL.TYPE <- as.factor(rf.dat$SOIL.TYPE)
rf.dat$BIOMES <- as.factor(rf.dat$BIOMES)

set.seed(111)

output.rf<- randomForest(OlsenP ~ .,  
                         ntree=500, keep.forest=T,
                         importance=T, mtry=3, 
                         data = rf.dat[,-c(1,2)])
output.rf
imp.dat <- randomForest::importance(output.rf, scale=T)
predict.train <- predict(output.rf, data = rf.dat)
predicted.dat <- data.frame(predict.train, rf.dat)
cor(predicted.dat$OlsenP,predict.train)

#importance plot
imp.dat <- as.data.frame(imp.dat)

imp.dat$Predictors <- c("SOC","MAP","MAT","Sand", "Clay", "pH","Parent material","Soil order",
                        "Depth",
                        "NPP","Slope",
                        "Elevation", "Biome","Errosion"
                         
)
names(imp.dat) <- c("IncMSE", "IncNodePurity", "Predictors")
imp.dat <- mutate(imp.dat, Predictor.type=Predictors)

imp.dat$Predictor.type[imp.dat$Predictor.type == "Parent material"] <- "Parent material"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Soil order"] <- "Soil"
imp.dat$Predictor.type[imp.dat$Predictor.type == "Errosion"] <- "Soil"
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

imp.dat

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
  dplyr::select(predict.train, OlsenP) %>% 
  dplyr::mutate(Density = get_density(predict.train, OlsenP, n = 200))

prec.obs.plot <- 
  ggplot(data=predicted.dat, aes(x=predict.train, y=OlsenP))+
  geom_hex(binwidth=c(1,1), na.rm = T, alpha=.7, show.legend=T)+
  scale_fill_gradient(low = "red", high = "yellow")+
  geom_smooth(method = 'lm', se = T)+
  geom_abline(aes(slope=1, intercept=0), 
              color="grey50", size=1, linetype="longdash")+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 170),
                     breaks = c(0, 25,50,75, 100,125, 150,175))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 175),
                     breaks = c(0, 25,50, 75,100,125, 150,175))+
  labs(x=expression(paste("Predicted soil Olsen P (mg kg"^-1,")")), 
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

ggsave("OlsenP_with_errosion.tiff", importance.accuracy.plots, 
       width = 28, height = 13, units = "cm", scale = 1, dpi = 300)


