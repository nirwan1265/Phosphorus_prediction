#### FILTERED DATA 
# Load Data
bray_africa <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_AFRLAC.csv")
bray_global <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_global.csv")
olsen_africa <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_AFRLAC.csv")
olsen_global <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_global.csv")


# Filtering and running random forrest

### Bray Africa
str(bray_africa)
bray_africa <- bray_africa[complete.cases(bray_africa), ] %>% dplyr::select(-c(18:20))
bray_africa$BEDROCK <- as.factor(bray_africa$BEDROCK)
bray_africa$SOIL.TYPE <- as.factor(bray_africa$SOIL.TYPE)
bray_africa$BIOMES <- as.factor(bray_africa$BIOMES)

#### Adding stp data:
var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
var_raster <- raster(var_tif)
bray_africa$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_africa$x, latitude = bray_africa$y))
bray_africa <- bray_africa[complete.cases(bray_africa), ]

output_bray_africa<- randomForest(p_avg ~ .,  
                                  ntree=500, keep.forest=T,
                                  importance=T, mtry=3, 
                                  data = bray_africa[,-c(1,2)])


# Bray Global
str(bray_global)
bray_global <- bray_global[complete.cases(bray_global), ] %>% dplyr::select(-c(18:20))
bray_global$BEDROCK <- as.factor(bray_global$BEDROCK)
bray_global$SOIL.TYPE <- as.factor(bray_global$SOIL.TYPE)
bray_global$BIOMES <- as.factor(bray_global$BIOMES)

#### Adding stp data:
var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
var_raster <- raster(var_tif)
bray_global$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_global$x, latitude = bray_global$y))
bray_global <- bray_global[complete.cases(bray_global), ]


output_bray_global<- randomForest(p_avg ~ .,  
                                  ntree=500, keep.forest=T,
                                  importance=T, mtry=3, 
                                  data = bray_global[,-c(1,2)])


# Olsen Africa
str(olsen_africa)
olsen_africa <- olsen_africa[complete.cases(olsen_africa), ] %>% dplyr::select(-c(17:19))
olsen_africa$BEDROCK <- as.factor(olsen_africa$BEDROCK)
olsen_africa$SOIL.TYPE <- as.factor(olsen_africa$SOIL.TYPE)
olsen_africa$BIOMES <- as.factor(olsen_africa$BIOMES)

#### Adding stp data:
var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
var_raster <- raster(var_tif)
olsen_africa$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_africa$x, latitude = olsen_africa$y))
olsen_africa <- olsen_africa[complete.cases(olsen_africa), ]


output_olsen_africa<- randomForest(p_avg ~ .,  
                                   ntree=500, keep.forest=T,
                                   importance=T, mtry=3, 
                                   data = olsen_africa[,-c(1,2)])


# Olsen Global
olsen_global <- olsen_global[complete.cases(olsen_global), ] %>% dplyr::select(-c(17:19))
olsen_global$BEDROCK <- as.factor(olsen_global$BEDROCK)
olsen_global$SOIL.TYPE <- as.factor(olsen_global$SOIL.TYPE)
olsen_global$BIOMES <- as.factor(olsen_global$BIOMES)

#### Adding stp data:
var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
var_raster <- raster(var_tif)
olsen_global$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_global$x, latitude = olsen_global$y))
olsen_global <- olsen_global[complete.cases(olsen_global), ]


output_olsen_global<- randomForest(p_avg ~ .,  
                                   ntree=500, keep.forest=T,
                                   importance=T, mtry=3, 
                                   data = olsen_global[,-c(1,2)])


####### Importance
# Bray africa
imp_bray_africa <- randomForest::importance(output_bray_africa, scale=T)
predict_bray_africa <- predict(output_bray_africa, data = bray_africa)
val_bray_africa <- data.frame(predict_bray_africa, bray_africa)


# Bray global - global test
imp_bray_global <- randomForest::importance(output_bray_global, scale=T)
predict_bray_global <- predict(output_bray_global, data = bray_global)
val_bray_global <- data.frame(predict_bray_global, bray_global)

# Bray global - latinafrica test
imp_bray_global_lataf <- randomForest::importance(output_bray_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(bray_global[1,], bray_africa)
colnames(bray_global)
colnames(bray_africa)
colnames(xtest)
xtest <- xtest[-1,]
predict_bray_global_lataf <- predict(output_bray_global, newdata = xtest[,-c(1,2)])
val_bray_global_lataf <- data.frame(predict_bray_global, bray_global)


# Olsen Africa
imp_olsen_africa <- randomForest::importance(output_olsen_africa, scale=T)
predict_olsen_africa <- predict(output_olsen_africa, data = olsen_africa)
val_olsen_africa <- data.frame(predict_olsen_africa, olsen_africa)


# Olsen Global
imp_olsen_global <- randomForest::importance(output_olsen_global, scale=T)
predict_olsen_global <- predict(output_olsen_global, data = olsen_global)
val_olsen_global <- data.frame(predict_olsen_global, olsen_global)


# Olsen Global - latinafrica test
imp_olsen_global_lataf <- randomForest::importance(output_olsen_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(olsen_global[1,], olsen_africa)
xtest <- xtest[-1,]
colnames(xtest)
predict_olsen_global_lataf <- predict(output_olsen_global, newdata = xtest[,-c(1,2)])
val_olsen_global_lataf <- data.frame(predict_olsen_global, olsen_global)
dim(olsen_africa)
dim(olsen_global)
length(predict_olsen_global_lataf)

###### Correlation
# Bray Africa
cor_bray_africa <- cor(predict_bray_africa,bray_africa$p_avg)
mse_bray_africa <- mean((predict_bray_africa - bray_africa$p_avg)^2)

# Bray global - global corelation
cor_bray_global <- cor(predict_bray_global,bray_global$p_avg)
mse_bray_global <- mean((predict_bray_global - bray_global$p_avg)^2)

# Bray global - LatAfrica corelation
cor_bray_global_lataf <- cor(predict_bray_global_lataf,bray_africa$p_avg)
mse_bray_global <- mean((predict_bray_global_lataf - bray_africa$p_avg)^2)

# olsen Africa
cor_olsen_africa <- cor(predict_olsen_africa,olsen_africa$p_avg)
mse_olsen_africa <- mean((predict_olsen_africa - olsen_africa$p_avg)^2)

# olsen global - global corelation
cor_olsen_global <- cor(predict_olsen_global,olsen_global$p_avg)
mse_olsen_global <- mean((predict_olsen_global - olsen_global$p_avg)^2)

# olsen global - LatAfrica corelation
cor_olsen_global_lataf <- cor(predict_olsen_global_lataf,olsen_africa$p_avg)
mse_olsen_global <- mean((predict_olsen_global_lataf - olsen_africa$p_avg)^2)

