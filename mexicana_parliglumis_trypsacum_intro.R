### Load the training dataset
bray <- read.csv(paste0(getwd(),"/data/P_data/bray_global.csv")) %>% dplyr::select(c(1,2,3,18))
rownames(bray) <- seq(1:nrow(bray))
bray_35above <- bray[which(bray$LATITUDE >= 35), ]
bray_35below <- bray[which(bray$LATITUDE < 35), ]
bray_35below$data <- "train"
bray_35above$data <- "train"

### Read the testing data
dir <- paste0(getwd(),"/data/fausto_data/")
mexicana <- read.csv(paste0(dir,"mexicana_ocurrences_gpf.csv"), sep ="\t") %>%
  dplyr::select(c(1:16,22,23))
mexicana <- mexicana[complete.cases(mexicana$decimalLatitude), ]
mexicana <- mexicana[complete.cases(mexicana$decimalLongitude), ]
colnames(mexicana)[17:18] <- c("LATITUDE","LONGITUDE")

parviglumis <- read.csv(paste0(dir,"parviglumis_ocurrences_gpf.csv"), sep ="\t") %>%
  dplyr::select(c(1:16,22,23))
parviglumis <- parviglumis[complete.cases(parviglumis$decimalLatitude), ]
colnames(parviglumis)[17:18] <- c("LATITUDE","LONGITUDE")

trypsacum <- read.csv(paste0(dir,"trypsacum_ocurrences_gbif.csv"), sep ="\t") %>%
  dplyr::select(c(1:16,22,23))
trypsacum <- trypsacum[complete.cases(trypsacum$decimalLatitude), ]
colnames(trypsacum)[17:18] <- c("LATITUDE","LONGITUDE")

intro <- read.csv(paste0(dir,"intro_B37xTeo.csv"))
intro <- intro[complete.cases(intro$LATITUDE), ]
intro$GEO3major <- "NA"
intro$p_avg <- 0
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


# Combining the testing and training data
bray_combined_data_35below <- rbind(bray_35below,combined_data_35below)
bray_combined_data_35above <- rbind(bray_35below,combined_data_35above)

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


# Running the model:
data <- combined_data
