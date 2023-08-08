##### NC MAP

### Load the training dataset
bray <- read.csv(paste0(getwd(),"/data/P_data/bray_global.csv")) %>% dplyr::select(c(1,2,3,18))
rownames(bray) <- seq(1:nrow(bray))
bray_35above <- bray[which(bray$LATITUDE >= 35), ]
bray_35below <- bray[which(bray$LATITUDE < 35), ]
bray_35below$data <- "train"
bray_35above$data <- "train"



#### Testing data:
# North Carolina
# Define the extent of North Carolina (approximately)
xmin <- -84.3
xmax <- -75.5
ymin <- 33.8
ymax <- 36.6

# Define the resolution
res <- 0.1  # approximately 10 km

# Generate the grid
longs <- seq(from = xmin, to = xmax, by = res)
lats <- seq(from = ymin, to = ymax, by = res)

# Create a data frame of all combinations of longitudes and latitudes
grid <- expand.grid(LONGITUDE = longs, LATITUDE = lats)
grid$GEO3major <- "NA"
grid$p_avg <- 0
grid$data <- "test"
grid_35above <- grid[which(grid$LATITUDE >= 35), ]
grid_35below <- grid[which(grid$LATITUDE < 35), ]



# Combining the testing and training data
bray_combined_data_35below <- rbind(bray_35below,grid_35below)


# Extracting the values of predictors from the raster files
dir_raster <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Research/data/raster"

# raster files:
raster_files <- list.files(path = dir_raster, pattern = "\\.tif$", full.names = TRUE)

# Function - extracting values from a raster file for all coordinates 
extract_raster_values <- function(raster_file, df) {
  r <- raster::brick(raster_file)
  
  coords <- df[, c("LONGITUDE", "LATITUDE")]
  coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  coords_transformed <- sp::spTransform(coords_sp, crs(r))
  
  values <- raster::extract(x = r, y = coords_transformed)
  return(as.vector(values))
}

# Getting the predictor values:
for (raster_file in raster_files) {
  # Get the variable name from the file name
  var_name <- gsub("\\.tif$", "", basename(raster_file))
  
  # Extract values for all coordinates in the pheno maize_35below frame
  extracted_values_35below <- extract_raster_values(raster_file, bray_combined_data_35below)
  #extracted_values_35above <- extract_raster_values(raster_file, bray_combined_data_35above)
  
  # Add the extracted values as a new column in the pheno maize_35below frame
  bray_combined_data_35below[[var_name]] <- extracted_values_35below
  #bray_combined_data_35above[[var_name]] <- extracted_values_35above
  
}

bray_combined_data_35below <- bray_combined_data_35below[complete.cases(bray_combined_data_35below[,6:20]), ]
#bray_combined_data_35above <- bray_combined_data_35above[complete.cases(bray_combined_data_35above[,6:20]), ]

#write.csv(bray_combined_data_35below,"bray_combined_data_35below.csv",row.names =F)
#write.csv(bray_combined_data_35above,"bray_combined_data_35above.csv",row.names =F)



####### Using previous model

# Loading the metamodel
dir_meta <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Github/Phosphorus_prediction/model_RDS/"
meta_model <- readRDS(paste0(dir_meta,"bray_total_35below.RDS"))

data <- bray_combined_data_35below
sampsize <- 70

columns_to_factor <- c("BEDROCK", "SOIL.USDA", "BIOME", "GEO3major")
for (column in columns_to_factor) {
  data[[column]] <- as.factor(data[[column]])
}

# Splitting data into training and testing sets
train_indices <- data$data == "train"
train_data <- data[train_indices, ]
test_data <- data[!train_indices, ]

# Ensuring same levels in both train_data and test_data
for (column in columns_to_factor) {
  union_levels <- levels(data[[column]])
  train_data[[column]] <- factor(train_data[[column]], levels = union_levels)
  test_data[[column]] <- factor(test_data[[column]], levels = union_levels)
}

# Store the row names of the test data
test_row_names <- rownames(test_data)

# Exclude unwanted columns
exclude_cols <- c("LONGITUDE", "LATITUDE", "GEO3major", "data")
train_data <- train_data[, !(names(train_data) %in% exclude_cols)]
test_data <- test_data[, !(names(test_data) %in% exclude_cols)]


# Required variables
num_models <- 15
model_list <- list()
train_predictions <- matrix(NA, nrow = nrow(train_data), ncol = num_models)
test_predictions <- matrix(NA, nrow = nrow(test_data), ncol = num_models)

# Train individual models in the ensemble
for (i in 1:num_models) {
  # Bagging (Bootstrap aggregating) - random subset of training data
  bag_indices <- sample(nrow(train_data), replace = TRUE)
  bag_data <- train_data[bag_indices, ]
  
  # Random forest model with starta and sampsize
  model <- randomForest(p_avg ~ .,  
                        ntree = 200,
                        keep.forest = TRUE,
                        importance = TRUE,
                        mtry = 3,
                        sampsize = sampsize,
                        strata = train_data$GEO3major,
                        data = bag_data)
  model_list[[i]] <- model
}

# Predictions for each individual model
for (i in 1:num_models) {
  train_predictions[, i] <- predict(model_list[[i]], newdata = train_data)
  test_predictions[, i] <- predict(model_list[[i]], newdata = test_data)
}
colnames(test_predictions) <- c("Model 1","Model 2","Model 3","Model 4","Model 5",
                                "Model 6","Model 7","Model 8","Model 9","Model 10",
                                "Model 11","Model 12","Model 13","Model 14","Model 15")




# Meta-predictions for the training and test sets from staking
test_meta_predictions <- as.data.frame(predict(meta_model[["meta_model"]], newdata = test_predictions))
test_meta_predictions$ID <- test_row_names
test_meta_predictions <- test_meta_predictions[,c(2,1)]
colnames(test_meta_predictions)[2] <- "p_avg"

# Combining with previous values:
grid_35below$ID <- rownames(grid_35below)
final_df <- inner_join(test_meta_predictions,grid_35below, by = "ID")
final_df <- final_df[,c(2,3,4)]
colnames(final_df)[1] <- c("p_avg")


str(final_df)


library(gstat)
library(raster)
library(sp)

coordinates(final_df) <- ~LONGITUDE+LATITUDE

# Define the spatial grid
grd <- expand.grid(LONGITUDE = seq(from = bbox(final_df)[1,1], to = bbox(final_df)[1,2], by = 0.01),
                   LATITUDE = seq(from = bbox(final_df)[2,1], to = bbox(final_df)[2,2], by = 0.01))

coordinates(grd) <- ~LONGITUDE+LATITUDE
gridded(grd) <- TRUE

# Perform IDW interpolation
idw_result <- idw(formula = p_avg ~ 1, locations = final_df, newdata = grd)

# Convert to raster
raster_result <- raster(idw_result)
raster_result

spplot(idw_result)


# Write to GeoTIFF
writeRaster(raster_result, filename="p_avg_heatmap.tif", format="GTiff", overwrite=TRUE)
