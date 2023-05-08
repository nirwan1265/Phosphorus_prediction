#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Load Data
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Bray afrlat
bray_afrlat <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_AFRLAC.csv")
str(bray_afrlat)
bray_afrlat <- bray_afrlat[complete.cases(bray_afrlat), ] %>% dplyr::select(-c(18:20))
bray_afrlat$BEDROCK <- as.factor(bray_afrlat$BEDROCK)
bray_afrlat$SOIL.TYPE <- as.factor(bray_afrlat$SOIL.TYPE)
bray_afrlat$BIOMES <- as.factor(bray_afrlat$BIOMES)

#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# bray_afrlat$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_afrlat$x, latitude = bray_afrlat$y))
# bray_afrlat <- bray_afrlat[complete.cases(bray_afrlat), ]


### Bray Global
bray_global <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_global.csv")
str(bray_global)
bray_global <- bray_global[complete.cases(bray_global), ] %>% dplyr::select(-c(18:20))
bray_global$BEDROCK <- as.factor(bray_global$BEDROCK)
bray_global$SOIL.TYPE <- as.factor(bray_global$SOIL.TYPE)
bray_global$BIOMES <- as.factor(bray_global$BIOMES)

#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# bray_global$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_global$x, latitude = bray_global$y))
# bray_global <- bray_global[complete.cases(bray_global), ]


### Olsen AfrLat
olsen_afrlat <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_AFRLAC.csv")
str(olsen_afrlat)
olsen_afrlat <- olsen_afrlat[complete.cases(olsen_afrlat), ] %>% dplyr::select(-c(17:19))
olsen_afrlat$BEDROCK <- as.factor(olsen_afrlat$BEDROCK)
olsen_afrlat$SOIL.TYPE <- as.factor(olsen_afrlat$SOIL.TYPE)
olsen_afrlat$BIOMES <- as.factor(olsen_afrlat$BIOMES)

#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# olsen_afrlat$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_afrlat$x, latitude = olsen_afrlat$y))
# olsen_afrlat <- olsen_afrlat[complete.cases(olsen_afrlat), ]


### Olsen Global
olsen_global <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_global.csv")
olsen_global <- olsen_global[complete.cases(olsen_global), ] %>% dplyr::select(-c(17:19))
olsen_global$BEDROCK <- as.factor(olsen_global$BEDROCK)
olsen_global$SOIL.TYPE <- as.factor(olsen_global$SOIL.TYPE)
olsen_global$BIOMES <- as.factor(olsen_global$BIOMES)

#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# olsen_global$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_global$x, latitude = olsen_global$y))
# olsen_global <- olsen_global[complete.cases(olsen_global), ]


### Stp AfrLat
stp_afrlat <- read.csv("data/filtered_data/P_stp_he2022_predictors_he2022_AFRLAC.csv")
str(stp_afrlat)
stp_afrlat <- stp_afrlat[complete.cases(stp_afrlat), ] 
stp_afrlat$BEDROCK <- as.factor(stp_afrlat$BEDROCK)
stp_afrlat$SOIL_TYPE <- as.factor(stp_afrlat$SOIL_TYPE)
stp_afrlat$BIOMES <- as.factor(stp_afrlat$BIOMES)
stp_afrlat$WRB_SOIL_TYPE <- as.factor(stp_afrlat$WRB_SOIL_TYPE)


### Stp Global
stp_global <- read.csv("data/filtered_data/P_stp_he2022_predictors_he2022_global.csv")
str(stp_global)
stp_global <- stp_global[complete.cases(stp_global), ]
stp_global$BEDROCK <- as.factor(stp_global$BEDROCK)
stp_global$SOIL_TYPE <- as.factor(stp_global$SOIL_TYPE)
stp_global$BIOMES <- as.factor(stp_global$BIOMES)
stp_global$WRB_SOIL_TYPE <- as.factor(stp_global$WRB_SOIL_TYPE)
str(stp_global)

