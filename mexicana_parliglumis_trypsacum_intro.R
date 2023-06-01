### Load the training dataset
bray <- read.csv(paste0(getwd(),"/data/P_data/bray_global.csv")) %>% dplyr::select(c(1,2,3,18))
rownames(bray) <- seq(1:nrow(bray))
bray_35above <- bray[which(bray$LATITUDE >= 35), ]
bray_35below <- bray[which(bray$LATITUDE < 35), ]
bray_35below$data <- "train"
bray_35above$data <- "train"
#bray_35above <- bray_35above[1:500,]

### Read the testing data
dir <- paste0(getwd(),"/data/fausto_data/")
mexicana <- read.csv(paste0(dir,"mexicana_ocurrences_gpf.csv"), sep ="\t") %>%
  dplyr::select(c(1:16,22,23))
mexicana <- mexicana[complete.cases(mexicana$decimalLatitude), ]
mexicana <- mexicana[complete.cases(mexicana$decimalLongitude), ]
colnames(mexicana)[17:18] <- c("LATITUDE","LONGITUDE")
colnames(mexicana)[1] <- "ID"

parviglumis <- read.csv(paste0(dir,"parviglumis_ocurrences_gpf.csv"), sep ="\t") %>%
  dplyr::select(c(1:16,22,23))
parviglumis <- parviglumis[complete.cases(parviglumis$decimalLatitude), ]
colnames(parviglumis)[17:18] <- c("LATITUDE","LONGITUDE")
colnames(parviglumis)[1] <- "ID"

trypsacum <- read.csv(paste0(dir,"trypsacum_ocurrences_gbif.csv"), sep ="\t") %>%
  dplyr::select(c(1:16,22,23))
trypsacum <- trypsacum[complete.cases(trypsacum$decimalLatitude), ]
colnames(trypsacum)[17:18] <- c("LATITUDE","LONGITUDE")
colnames(trypsacum)[1] <- "ID"


intro <- read.csv(paste0(dir,"intro_B37xTeo.csv"))
intro <- intro[complete.cases(intro$LATITUDE), ]
intro$GEO3major <- "NA"
intro$p_avg <- 0
colnames(intro)[1] <- "ID"

rownames(intro) <- intro[,1]
intro <- intro[-1]

# Combining the data
combined_data <- rbind(mexicana,parviglumis,trypsacum) 
combined_data$GEO3major <- "NA"
combined_data$p_avg <- 0
combined_data <- combined_data[,c(1,17:20)]
rownames(combined_data) <- combined_data[,1]
combined_data <- combined_data[,-1]
combined_data <- rbind(combined_data,intro)

# Separating into 35 above and below
combined_data <- combined_data[complete.cases(combined_data[,2]), ]
combined_data$data <- as.vector("test")

combined_data_35above <- combined_data[which(combined_data$LATITUDE >= 35), ]
combined_data_35below <- combined_data[which(combined_data$LATITUDE < 35), ]
#combined_data_35above <- combined_data_35above[1:500,]

# Combining the testing and training data
bray_combined_data_35below <- rbind(bray_35below,combined_data_35below)
bray_combined_data_35above <- rbind(bray_35above,combined_data_35above)

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
  extracted_values_35above <- extract_raster_values(raster_file, bray_combined_data_35above)
  
  # Add the extracted values as a new column in the pheno maize_35below frame
  bray_combined_data_35below[[var_name]] <- extracted_values_35below
  bray_combined_data_35above[[var_name]] <- extracted_values_35above
  
}
bray_combined_data_35below <- bray_combined_data_35below[complete.cases(bray_combined_data_35below[,6:20]), ]
bray_combined_data_35above <- bray_combined_data_35above[complete.cases(bray_combined_data_35above[,6:20]), ]

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
combined_data_35below$ID <- rownames(combined_data_35below)
final_df <- inner_join(test_meta_predictions,combined_data_35below, by = "ID")

intro <- read.csv(paste0(dir,"intro_B37xTeo.csv"))
intro <- intro[complete.cases(intro$LATITUDE), ]
intro$GEO3major <- "NA"
intro$p_avg <- 0
colnames(intro)[1] <- "ID"


final_df$ID <- as.character(final_df$ID)
mexicana$ID <- as.character(mexicana$ID)
parviglumis$ID <- as.character(parviglumis$ID)
trypsacum$ID <- as.character(trypsacum$ID)
intro$ID <- as.character(intro$ID)

mexicana_predict <- dplyr::inner_join(final_df,mexicana, by = "ID")
mexicana_predict <- mexicana_predict[,-c(5:7)]

parviglumis_predict <- dplyr::inner_join(final_df,parviglumis, by = "ID")
parviglumis_predict <- parviglumis_predict[,-c(5:7)]

trypsacum_predict <- dplyr::inner_join(final_df,trypsacum, by = "ID")
trypsacum_predict <- trypsacum_predict[,-c(5:7)]

intro_predict <- dplyr::inner_join(final_df,intro, by = "ID")
intro_predict <- intro_predict[,c(1:4)]


# write.csv(mexicana_predict,"mexicana_predict.csv",row.names = F)
# write.csv(parviglumis_predict,"parviglumis_predict.csv",row.names = F)
# write.csv(trypsacum_predict,"trypsacum_predict.csv",row.names = F)
# write.csv(intro_predict,"intro_predict.csv",row.names = F)


# Getting PH values for these files:
dir_pheno <- paste0(getwd(),"/predicted_pheno/")
mexicana_ph <- read.csv(paste0(dir_pheno,"mexicana_predict.csv"))
parviglumis_ph <- read.csv(paste0(dir_pheno,"parviglumis_predict.csv"))
trypsacum_ph <- read.csv(paste0(dir_pheno,"trypsacum_predict.csv"))
intro_ph <- read.csv(paste0(dir_pheno,"introgression_B73xTeo_predict.csv"))

for (raster_file in raster_files) {
  # Get the variable name from the file name
  var_name <- gsub("\\.tif$", "", basename(raster_file))
  
  # Extract values for all coordinates in the pheno maize_35below frame
  extracted_values_mexicana <- extract_raster_values(raster_file, mexicana_ph)
  extracted_values_parviglumis <- extract_raster_values(raster_file, parviglumis_ph)
  extracted_values_trypsacum <- extract_raster_values(raster_file, trypsacum_ph)
  extracted_values_intro <- extract_raster_values(raster_file, intro_ph)
  
  
  # Add the extracted values as a new column in the pheno maize_35below frame
  mexicana_ph[[var_name]] <- extracted_values_mexicana
  parviglumis_ph[[var_name]] <- extracted_values_parviglumis
  trypsacum_ph[[var_name]] <- extracted_values_trypsacum
  intro_ph[[var_name]] <- extracted_values_intro 
}

write.csv(mexicana_ph,"mexicana_predict.csv",row.names = F)
write.csv(parviglumis_ph,"parviglumis_predict.csv",row.names = F)
write.csv(trypsacum_ph,"trypsacum_predict.csv",row.names = F)
write.csv(intro_ph,"introgression_B73xTeo_predict.csv",row.names = F)
