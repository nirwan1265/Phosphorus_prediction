#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Load Data
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### Bray afrlac
bray_afrlac <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_AFRLAC.csv")
colnames(bray_afrlac)

# Adding country and filter by continent
bray_afrlac <- data.frame( 
  LONGITUDE = bray_afrlac$x,
  LATITUDE = bray_afrlac$y,
  p_avg = bray_afrlac$p_avg,
  ID = bray_afrlac$ID,
  SOC = bray_afrlac$SOC,
  MAT = bray_afrlac$MAT,
  MAP= bray_afrlac$MAP,
  SAND = bray_afrlac$SAND,
  CLAY = bray_afrlac$CLAY,
  PH = bray_afrlac$PH,
  BEDROCK = bray_afrlac$BEDROCK,
  `SOIL.TYPE` = bray_afrlac$`SOIL.TYPE`,
  DEPTH = bray_afrlac$DEPTH,
  NPP = bray_afrlac$NPP,
  SLOPE = bray_afrlac$SLOPE,
  ELEVATION = bray_afrlac$ELEVATION,
  BIOMES = bray_afrlac$BIOMES,
  EROIDX = bray_afrlac$EROIDX,
  TCEQ = bray_afrlac$TCEQ,
  CACO3 = bray_afrlac$CACO3,
  mapname = map.where(database="world", 
                      bray_afrlac$x, bray_afrlac$y)) %>%
  dplyr::inner_join(iso3166) %>%
  dplyr::inner_join(countryRegions, by =c("a3"="ISO3")) %>%
  dplyr::select(c(1:20,29))
table(bray_afrlac$GEO3major)


str(bray_afrlac)
bray_afrlac <- bray_afrlac[complete.cases(bray_afrlac), ] %>% dplyr::select(-c(18:20))
bray_afrlac$BEDROCK <- as.factor(bray_afrlac$BEDROCK)
bray_afrlac$SOIL.TYPE <- as.factor(bray_afrlac$SOIL.TYPE)
bray_afrlac$BIOMES <- as.factor(bray_afrlac$BIOMES)
bray_afrlac$GEO3major <- as.factor(bray_afrlac$GEO3major)

### Bray afrlac
bray_afrlac <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_AFRLAC.csv")
str(bray_afrlac)
bray_afrlac <- bray_afrlac[complete.cases(bray_afrlac), ] %>% dplyr::select(-c(18:20))
bray_afrlac$BEDROCK <- as.factor(bray_afrlac$BEDROCK)
bray_afrlac$SOIL.TYPE <- as.factor(bray_afrlac$SOIL.TYPE)
bray_afrlac$BIOMES <- as.factor(bray_afrlac$BIOMES)


#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# bray_afrlac$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_afrlac$x, latitude = bray_afrlac$y))
# bray_afrlac <- bray_afrlac[complete.cases(bray_afrlac), ]
# bray_afrlac$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_afrlac$x, latitude = bray_afrlac$y))
# bray_afrlac <- bray_afrlac[complete.cases(bray_afrlac), ]



### Bray Global
bray_global <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_global.csv")

colnames(bray_global)
# Adding country and filter by continent
bray_global <- data.frame( 
  LONGITUDE = bray_global$x,
  LATITUDE = bray_global$y,
  p_avg = bray_global$p_avg,
  ID = bray_global$ID,
  SOC = bray_global$SOC,
  MAT = bray_global$MAT,
  MAP= bray_global$MAP,
  SAND = bray_global$SAND,
  CLAY = bray_global$CLAY,
  PH = bray_global$PH,
  BEDROCK = bray_global$BEDROCK,
  `SOIL.TYPE` = bray_global$`SOIL.TYPE`,
  DEPTH = bray_global$DEPTH,
  NPP = bray_global$NPP,
  SLOPE = bray_global$SLOPE,
  ELEVATION = bray_global$ELEVATION,
  BIOMES = bray_global$BIOMES,
  EROIDX = bray_global$EROIDX,
  TCEQ = bray_global$TCEQ,
  CACO3 = bray_global$CACO3,
  mapname = map.where(database="world", 
                      bray_global$x, bray_global$y)) %>%
  dplyr::inner_join(iso3166) %>%
  dplyr::inner_join(countryRegions, by =c("a3"="ISO3")) %>%
  dplyr::select(c(1:20,29))
table(bray_global$GEO3major)



bray_afrlac <- dplyr::filter(bray_global, GEO3major %in% c("Africa", "Latin America and the Caribbean"))
bray_afrlac <- bray_afrlac[complete.cases(bray_afrlac), ] %>% dplyr::select(-c(18:20))
bray_afrlac$BEDROCK <- as.factor(bray_afrlac$BEDROCK)
bray_afrlac$SOIL.TYPE <- as.factor(bray_afrlac$SOIL.TYPE)
bray_afrlac$BIOMES <- as.factor(bray_afrlac$BIOMES)
bray_afrlac$GEO3major <- as.factor(bray_afrlac$GEO3major)

str(bray_global)
bray_global <- bray_global[complete.cases(bray_global), ] %>% dplyr::select(-c(18:20))
bray_global$BEDROCK <- as.factor(bray_global$BEDROCK)
bray_global$SOIL.TYPE <- as.factor(bray_global$SOIL.TYPE)
bray_global$BIOMES <- as.factor(bray_global$BIOMES)
bray_global$GEO3major <- as.factor(bray_global$GEO3major)
table(bray_global$GEO3major)
bray_global_subset <- bray_global[!bray_global$GEO3major %in% c("North America", "Europe", "West Asia"), ]

### Above and belo 35 latitude
colnames(bray_global)
bray_35above <- bray_global[which(bray_global$LATITUDE >= 35), ]
bray_35below <- bray_global[which(bray_global$LATITUDE < 35), ]



#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# bray_global$stp10 <- raster::extract(var_raster, data.frame(longitude = bray_global$x, latitude = bray_global$y))
# bray_global <- bray_global[complete.cases(bray_global), ]


### Olsen afrlac
olsen_afrlac <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_AFRLAC.csv")
colnames(olsen_afrlac)

# Adding country and filter by continent
olsen_afrlac <- data.frame( 
  LONGITUDE = olsen_afrlac$x,
  LATITUDE = olsen_afrlac$y,
  p_avg = olsen_afrlac$p_avg,
  SOC = olsen_afrlac$SOC,
  MAT = olsen_afrlac$MAT,
  MAP= olsen_afrlac$MAP,
  SAND = olsen_afrlac$SAND,
  CLAY = olsen_afrlac$CLAY,
  PH = olsen_afrlac$PH,
  BEDROCK = olsen_afrlac$BEDROCK,
  `SOIL.TYPE` = olsen_afrlac$`SOIL.TYPE`,
  DEPTH = olsen_afrlac$DEPTH,
  NPP = olsen_afrlac$NPP,
  SLOPE = olsen_afrlac$SLOPE,
  ELEVATION = olsen_afrlac$ELEVATION,
  BIOMES = olsen_afrlac$BIOMES,
  EROIDX = olsen_afrlac$EROIDX,
  TCEQ = olsen_afrlac$TCEQ,
  CACO3 = olsen_afrlac$CACO3,
  mapname = map.where(database="world", 
                      olsen_afrlac$x, olsen_afrlac$y)) %>%
  dplyr::inner_join(iso3166) %>%
  dplyr::inner_join(countryRegions, by =c("a3"="ISO3")) %>%
  dplyr::select(c(1:19,28))
table(olsen_afrlac$GEO3major)


str(olsen_afrlac)
olsen_afrlac <- olsen_afrlac[complete.cases(olsen_afrlac), ] %>% dplyr::select(-c(17:19))
olsen_afrlac$BEDROCK <- as.factor(olsen_afrlac$BEDROCK)
olsen_afrlac$SOIL.TYPE <- as.factor(olsen_afrlac$SOIL.TYPE)
olsen_afrlac$BIOMES <- as.factor(olsen_afrlac$BIOMES)
olsen_afrlac$GEO3major <- as.factor(olsen_afrlac$GEO3major)
#olsen_global_subset <- olsen_global[!olsen_global$GEO3major %in% c("North America", "Europe", "West Asia"), ]



### Olsen afrlac
olsen_afrlac <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_AFRLAC.csv")
str(olsen_afrlac)
olsen_afrlac <- olsen_afrlac[complete.cases(olsen_afrlac), ] %>% dplyr::select(-c(17:19))
olsen_afrlac$BEDROCK <- as.factor(olsen_afrlac$BEDROCK)
olsen_afrlac$SOIL.TYPE <- as.factor(olsen_afrlac$SOIL.TYPE)
olsen_afrlac$BIOMES <- as.factor(olsen_afrlac$BIOMES)


#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)

# olsen_afrlac$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_afrlac$x, latitude = olsen_afrlac$y))
# olsen_afrlac <- olsen_afrlac[complete.cases(olsen_afrlac), ]

# olsen_afrlac$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_afrlac$x, latitude = olsen_afrlac$y))
# olsen_afrlac <- olsen_afrlac[complete.cases(olsen_afrlac), ]



### Olsen Global
olsen_global <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_global.csv")
colnames(olsen_global)

# Adding country and filter by continent
olsen_global <- data.frame( 
  LONGITUDE = olsen_global$x,
  LATITUDE = olsen_global$y,
  p_avg = olsen_global$p_avg,
  SOC = olsen_global$SOC,
  MAT = olsen_global$MAT,
  MAP= olsen_global$MAP,
  SAND = olsen_global$SAND,
  CLAY = olsen_global$CLAY,
  PH = olsen_global$PH,
  BEDROCK = olsen_global$BEDROCK,
  `SOIL.TYPE` = olsen_global$`SOIL.TYPE`,
  DEPTH = olsen_global$DEPTH,
  NPP = olsen_global$NPP,
  SLOPE = olsen_global$SLOPE,
  ELEVATION = olsen_global$ELEVATION,
  BIOMES = olsen_global$BIOMES,
  EROIDX = olsen_global$EROIDX,
  TCEQ = olsen_global$TCEQ,
  CACO3 = olsen_global$CACO3,
  mapname = map.where(database="world", 
                      olsen_global$x, olsen_global$y)) %>%
  dplyr::inner_join(iso3166) %>%
  dplyr::inner_join(countryRegions, by =c("a3"="ISO3")) %>%
  dplyr::select(c(1:19,28))
table(olsen_global$GEO3major)

olsen_afrlac <- dplyr::filter(olsen_global, GEO3major %in% c("Africa", "Latin America and the Caribbean"))
olsen_afrlac <- olsen_afrlac[complete.cases(olsen_afrlac), ] %>% dplyr::select(-c(17:19))
olsen_afrlac$BEDROCK <- as.factor(olsen_afrlac$BEDROCK)
olsen_afrlac$SOIL.TYPE <- as.factor(olsen_afrlac$SOIL.TYPE)
olsen_afrlac$BIOMES <- as.factor(olsen_afrlac$BIOMES)
olsen_afrlac$GEO3major <- as.factor(olsen_afrlac$GEO3major)


str(olsen_global)
olsen_global <- olsen_global[complete.cases(olsen_global), ] %>% dplyr::select(-c(17:19))
olsen_global$BEDROCK <- as.factor(olsen_global$BEDROCK)
olsen_global$SOIL.TYPE <- as.factor(olsen_global$SOIL.TYPE)
olsen_global$BIOMES <- as.factor(olsen_global$BIOMES)
olsen_global$GEO3major <- as.factor(olsen_global$GEO3major)
olsen_global_subset <- olsen_global[!olsen_global$GEO3major %in% c("North America", "Europe", "West Asia"), ]

### Above and belo 35 latitude
colnames(olsen_global)
olsen_35above <- olsen_global[which(olsen_global$LATITUDE >= 35), ]
olsen_35below <- olsen_global[which(olsen_global$LATITUDE < 35), ]



#### Adding stp data:
# var_tif <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/stp.0-10cm.tif"
# var_raster <- raster(var_tif)
# olsen_global$stp10 <- raster::extract(var_raster, data.frame(longitude = olsen_global$x, latitude = olsen_global$y))
# olsen_global <- olsen_global[complete.cases(olsen_global), ]


### Stp afrlac
stp_afrlac <-  read.csv("data/filtered_data/P_stp_he2022_predictors_he2022_AFRLAC.csv")
colnames(stp_afrlac)

# Adding country and filter by continent
stp_afrlac <- data.frame( 
  LONGITUDE = stp_afrlac$LONGITUDE,
  LATITUDE = stp_afrlac$LATITUDE,
  p_avg = stp_afrlac$p_avg,
  SOC = stp_afrlac$SOC,
  SAND = stp_afrlac$SAND,
  CLAY = stp_afrlac$CLAY,
  MAT = stp_afrlac$MAT,
  MAP= stp_afrlac$MAP,
  PH = stp_afrlac$PH,
  NPP = stp_afrlac$NPP,
  SLOPE = stp_afrlac$SLOPE,
  ELEVATION = stp_afrlac$ELEVATION,
  BIOMES = stp_afrlac$BIOMES,
  BEDROCK = stp_afrlac$BEDROCK,
  `SOIL.TYPE` = stp_afrlac$`SOIL.TYPE`,
  DEPTH = stp_afrlac$DEPTH,
  `WRB.SOIL.TYPE` = stp_afrlac$`WRB.SOIL.TYPE`,
  mapname = map.where(database="world", 
                      stp_afrlac$LONGITUDE, stp_afrlac$LATITUDE)) %>%
  dplyr::inner_join(iso3166) %>%
  dplyr::inner_join(countryRegions, by =c("a3"="ISO3")) %>%
  dplyr::select(c(1:17,26))
table(stp_afrlac$GEO3major)


str(stp_afrlac)
stp_afrlac <- stp_afrlac[complete.cases(stp_afrlac), ] 
stp_afrlac$BEDROCK <- as.factor(stp_afrlac$BEDROCK)
stp_afrlac$`SOIL.TYPE` <- as.factor(stp_afrlac$`SOIL.TYPE`)
stp_afrlac$BIOMES <- as.factor(stp_afrlac$BIOMES)
stp_afrlac$`WRB.SOIL.TYPE` <- as.factor(stp_afrlac$`WRB.SOIL.TYPE`)
stp_afrlac$GEO3major <- as.factor(stp_afrlac$GEO3major)

### Stp Global
stp_global <- read.csv("data/filtered_data/P_stp_he2022_predictors_he2022_global.csv")
colnames(stp_global)
# Adding country and filter by continent
stp_global <- data.frame( 
  LONGITUDE = stp_global$LONGITUDE,
  LATITUDE = stp_global$LATITUDE,
  p_avg = stp_global$p_avg,
  SOC = stp_global$SOC,
  SAND = stp_global$SAND,
  CLAY = stp_global$CLAY,
  MAT = stp_global$MAT,
  MAP= stp_global$MAP,
  PH = stp_global$PH,
  NPP = stp_global$NPP,
  SLOPE = stp_global$SLOPE,
  ELEVATION = stp_global$ELEVATION,
  BIOMES = stp_global$BIOMES,
  BEDROCK = stp_global$BEDROCK,
  `SOIL.TYPE` = stp_global$`SOIL.TYPE`,
  DEPTH = stp_global$DEPTH,
  `WRB.SOIL.TYPE` = stp_global$`WRB.SOIL.TYPE`,
  mapname = map.where(database="world", 
                      stp_global$LONGITUDE, stp_global$LATITUDE)) %>%
  dplyr::inner_join(iso3166) %>%
  dplyr::inner_join(countryRegions, by =c("a3"="ISO3")) %>%
  dplyr::select(c(1:17,26))

### Above and belo 35 latitude
colnames(stp_global)
stp_35above <- stp_global[which(stp_global$LATITUDE >= 35), ]
stp_35below <- stp_global[which(stp_global$LATITUDE < 35), ]



### Stp afrlac
#stp_afrlac <- read.csv("data/filtered_data/P_stp_he2022_predictors_he2022_AFRLAC.csv")
str(stp_afrlac)
stp_afrlac <- stp_afrlac[complete.cases(stp_afrlac), ] 
stp_afrlac$BEDROCK <- as.factor(stp_afrlac$BEDROCK)
stp_afrlac$`SOIL.TYPE` <- as.factor(stp_afrlac$`SOIL.TYPE`)
stp_afrlac$BIOMES <- as.factor(stp_afrlac$BIOMES)
stp_afrlac$`WRB.SOIL.TYPE` <- as.factor(stp_afrlac$`WRB.SOIL.TYPE`)


### Stp Global
str(stp_global)
stp_global <- stp_global[complete.cases(stp_global), ]
stp_global$BEDROCK <- as.factor(stp_global$BEDROCK)
stp_global$`SOIL.TYPE` <- as.factor(stp_global$`SOIL.TYPE`)
stp_global$BIOMES <- as.factor(stp_global$BIOMES)
stp_global$`WRB.SOIL.TYPE` <- as.factor(stp_global$`WRB.SOIL.TYPE`)
stp_global$GEO3major <- as.factor(stp_global$GEO3major)
stp_global_subset <- stp_global[!stp_global$GEO3major %in% c("North America", "Europe", "West Asia"), ]
str(stp_global)



