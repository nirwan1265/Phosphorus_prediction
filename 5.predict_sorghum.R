### Load sorghum georef for Only Africa
dir_sorghum <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Research/data/Phenotype/"
sorghum <- read.csv(paste0(dir_sorghum,"taxa_geoloc_pheno.csv")) %>% 
  dplyr::filter(
    sp == "Sorghum bicolor",
    GEO3 %in% c("Central Africa", "Eastern Africa", "North Africa", "Southern Africa", "Western Africa")
  ) %>%
  dplyr::select(2,6,7)


# Separate into 35 latitude above and below
sorghum_35above <- sorghum[which(sorghum$lat >= 35), ]
colnames(sorghum_35above) <- c("Taxa","Long","Lat")
sorghum_35below <- sorghum[which(sorghum$lat < 35), ]
colnames(sorghum_35below) <- c("Taxa","Long","Lat")


# Loading the metamodel
dir_meta <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Github/Phosphorus_prediction/model_RDS/"
meta_model_35above <- readRDS(paste0(dir_meta,"output_stp_35above.RDS"))
meta_model_35below <- readRDS(paste0(dir_meta,"output_stp_35below.RDS"))


# Extracting the values of predictors from the raster files
dir_raster <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Research/data/raster"

# raster files:
raster_files <- list.files(path = dir_raster, pattern = "\\.tif$", full.names = TRUE)

# Function - extracting values from a raster file for all coordinates 
extract_raster_values <- function(raster_file, df) {
  r <- raster::brick(raster_file)
  
  coords <- df[, c("Long", "Lat")]
  coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  coords_transformed <- sp::spTransform(coords_sp, crs(r))
  
  values <- raster::extract(x = r, y = coords_transformed)
  return(as.vector(values))
}

# Getting the values for the tif files
## AFRICA does not have above 35 
for (raster_file in raster_files) {
  # Get the variable name from the file name
  var_name <- gsub("\\.tif$", "", basename(raster_file))
  
  # Extract values for all coordinates in the pheno sorghum_35below frame
  extracted_values <- extract_raster_values(raster_file, sorghum_35below)
  
  # Add the extracted values as a new column in the pheno sorghum_35below frame
  sorghum_35below[[var_name]] <- extracted_values
}


# Removing NA values:
sorghum_35below <- sorghum_35below[complete.cases(sorghum_35below), ]


# Reording columns and column names to match the original data used for prediction
colnames(stp_35below) # from 2.data_load
colnames(sorghum_35below)

# colnames(sorghum_35below) <- c("Taxa","LONGITUDE","LATITUDE","BEDROCK","BIOMES",
#                                "CLAY","NPP","DEPTH","ELEVATION","NPP","MAP","MAT",
#                                "PH","SAND","SLOPE","SOC","SOIL.TYPE","WRB.SOIL.TYPE")
row.names(sorghum_35below) <- sorghum_35below$Taxa
#sorghum_35below <- sorghum_35below[,c(2,3,16,14,6,12,11,13,10,15,9,5,4,17,8,18)]

# Changing to factors
sorghum_35below$BEDROCK <- as.factor(sorghum_35below$BEDROCK)
sorghum_35below$`SOIL.USDA` <- as.factor(sorghum_35below$`SOIL.USDA`)
sorghum_35below$BIOME <- as.factor(sorghum_35below$BIOME)
sorghum_35below$`SOIL.WRB` <- as.factor(sorghum_35below$`SOIL.WRB`)
str(sorghum_35below)
str(stp_35below)

# Predicting 15 models using bagging and stacking
predictions_df <- data.frame()
for (i in 1:length(meta_model_35below[["model"]])) {
  # Make predictions
  predictions <- predict(meta_model_35below[["model"]][[i]], newdata = sorghum_35below[,-c(1,2,3)])
  
  # Add predictions to the data frame
  predictions_df <- cbind(predictions_df, predictions)
}



names(meta_model_35below[["model"]][[i]]$forest$xlevels)
names(sorghum_35below[,-c(1,2,3)])


# Create a list to store the differences
differences <- list()

# Loop over the factor variables
for(var_name in names(sorghum_35below)[sapply(sorghum_35below, is.factor)]) {
  training_levels <- levels(meta_model_35below[["model"]][[i]]$forest$xlevels[[var_name]])
  newdata_levels <- levels(sorghum_35below[[var_name]])
  
  # Find levels present in newdata but not in training data
  diff_levels <- setdiff(newdata_levels, training_levels)
  
  # If there are any such levels, store them in the list
  if(length(diff_levels) > 0) {
    differences[[var_name]] <- diff_levels
  }
}

# Print the differences
print(differences)

