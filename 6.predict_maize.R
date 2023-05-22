### Load maize georef for Only Africa
dir_maize <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Research/data/Phenotype/"
maize <- read.csv(paste0(dir_maize,"taxa_geoloc_pheno.csv")) %>% 
  dplyr::filter(
    sp == "Zea mays",
    GEO3 %in% c("Caribbean", "Meso-America", "South America")
  ) %>%
  dplyr::select(2,6,7)
maize$GEO3major <- "NA"
maize$p_avg <- 0
rownames(maize) <- maize$Taxa
maize <- maize[,-1]
str(maize)
# Separate into 35 latitude above and below
maize_35above <- maize[which(maize$lat >= 35), ]
colnames(maize_35above) <- c("LONGITUDE","LATITUDE","GEO3major","p_avg")
maize_35below <- maize[which(maize$lat < 35), ]
colnames(maize_35below) <- c("LONGITUDE","LATITUDE","GEO3major","p_avg")
maize_35below$data <- "test"

### Load the training dataset
bray <- read.csv(paste0(getwd(),"/data/P_data/bray_global.csv")) %>% dplyr::select(c(1,2,3,18))
rownames(bray) <- seq(1:nrow(bray))
bray_35above <- bray[which(bray$LATITUDE >= 35), ]
bray_35below <- bray[which(bray$LATITUDE < 35), ]
bray_35below$data <- "train"
str(bray_35below)
str(maize_35below)


# Combine them
bray_total_35below <- rbind(bray_35below,maize_35below)
str(bray_total_35below)
tail(bray_total_35below)

# Loading the metamodel
dir_meta <- "/Users/nirwantandukar/Library/Mobile Documents/com~apple~CloudDocs/Github/Phosphorus_prediction/model_RDS/"
#meta_model_35above <- readRDS(paste0(dir_meta,"output_stp_35above.RDS"))
#meta_model_35below <- readRDS(paste0(dir_meta,"output_stp_35below.RDS"))


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

# Getting the values for the tif files
## AFRICA does not have above 35 
for (raster_file in raster_files) {
  # Get the variable name from the file name
  var_name <- gsub("\\.tif$", "", basename(raster_file))
  
  # Extract values for all coordinates in the pheno maize_35below frame
  extracted_values <- extract_raster_values(raster_file, bray_total_35below)
  
  # Add the extracted values as a new column in the pheno maize_35below frame
  bray_total_35below[[var_name]] <- extracted_values
}

dir_pheno=paste0(getwd(),"/predicted_pheno")
# Removing NA values:
bray_total_35below <- bray_total_35below[complete.cases(bray_total_35below), ]
#bray_total_35below$taxa <- rownames(bray_total_35below)

str(bray_total_35below)
table(bray_total_35below$data)

# Random forest model:
run_meta_model <- function(data, sampsize) {
  
  # Converting columns to factors
  columns_to_factor <- c("BEDROCK", "SOIL.USDA", "BIOME", "GEO3major")
  for (column in columns_to_factor) {
    data[[column]] <- as.factor(data[[column]])
  }
  
  # Splitting data into training and testing sets
  train_indices <- data$data == "train"
  train_data <- data[train_indices, ]
  test_data <- data[!train_indices, ]
  tail(test_data)
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
  
  # Renaming Training Columns
  colnames(train_predictions) <- paste("Model", 1:num_models)
  
  # Combining Predictions and Actual Values to create meta-data
  meta_data <- cbind(train_predictions, p_avg = train_data$p_avg)
  
  # Cross-validation using 5 folds
  cv <- caret::trainControl(method = "cv", number = 5)
  
  # Training the meta-model using RRF (Regularized Random Forest) with cross-validation
  meta_model <- caret::train(p_avg ~ .,
                             data = meta_data,
                             method = "RRF",
                             trControl = cv)
  
  # Renaming Testing Columns
  colnames(test_predictions) <- colnames(train_predictions)
  
  # Meta-predictions for the training and test sets from staking
  train_meta_predictions <- predict(meta_model, newdata = train_predictions)
  test_meta_predictions <- predict(meta_model, newdata = test_predictions)
  
  # Adding the stored row names to the final output dataframe
  test_prediction_final <- data.frame(Taxa = test_row_names, Predictions = as.vector(test_meta_predictions))
  
  #### Model Performances
  # Correlation
  train_correlation <- cor(train_meta_predictions, train_data$p_avg)
  #test_correlation <- cor(test_meta_predictions, test_data$p_avg)
  test_correlation <- "NA"
  
  # R-squared
  train_r_squared <- cor(train_meta_predictions, train_data$p_avg)^2
  #test_r_squared <- cor(test_meta_predictions, test_data$p_avg)^2
  test_r_squared <- "NA"
  # MSE
  train_mse <- mean((train_meta_predictions - train_data$p_avg)^2)
  #test_mse <- mean((test_meta_predictions - test_data$p_avg)^2)
  test_mse <- "NA"
  # Constructing the nested list
  results <- list(
    model = model_list,
    meta_model = meta_model,
    model_performance = list(
      correlation = list(
        training = train_correlation,
        testing = test_correlation
      ),
      r_squared = list(
        training = train_r_squared,
        testing = test_r_squared
      ),
      mse = list(
        training = train_mse,
        testing = test_mse
      )
    ),
    Prediction = test_prediction_final
  )
  
  return(results)
  
}

# Running the function with your specific datasets
data <- bray_total_35below
sampsize <- 70
output_bray_35below <- run_meta_model(data, sampsize)
