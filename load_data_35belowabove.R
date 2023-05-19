#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Load Data
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dir <- paste0(getwd(),"/data/P_data/")

bray_global <- read.csv(paste0(dir,"bray_global.csv")) %>% dplyr::select(c(3,1,2,18))
bray_35above <- bray_global[which(bray_global$LATITUDE >= 35), ]
bray_35below <- bray_global[which(bray_global$LATITUDE < 35), ]
rm(bray_global)

olsen_global <- read.csv(paste0(dir,"olsen_global.csv")) %>% dplyr::select(c(3,1,2,19))
olsen_35above <- olsen_global[which(olsen_global$LATITUDE >= 35), ]
olsen_35below <- olsen_global[which(olsen_global$LATITUDE < 35), ]
rm(olsen_global)

stp_global <- read.csv(paste0(dir,"stp_global.csv")) %>% dplyr::select(c(3,1,2,18))
stp_35above <- stp_global[which(stp_global$LATITUDE >= 35), ]
stp_35below <- stp_global[which(stp_global$LATITUDE < 35), ]
rm(stp_global)

### Getting the raster values
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

# Extracting the values
# List of data frames to process
data_frames <- list("bray_35above", "bray_35below", "olsen_35above", "olsen_35below", "stp_35above", "stp_35below")

# Loop over each data frame
for (df_name in data_frames) {
  # Get the actual data frame from the name
  df <- get(df_name)
  
  # Loop over each raster file
  for (raster_file in raster_files) {
    # Get the variable name from the file name
    var_name <- gsub("\\.tif$", "", basename(raster_file))
    
    # Extract values for all coordinates in the current data frame
    extracted_values <- extract_raster_values(raster_file, df)
    
    # Add the extracted values as a new column in the current data frame
    df[[var_name]] <- extracted_values
  }
  
  # Assign the modified data frame back to the global environment
  assign(df_name, df, envir = .GlobalEnv)
}

bray_35above <- bray_35above[complete.cases(bray_35above), ]
bray_35below <- bray_35below[complete.cases(bray_35below), ]
olsen_35above <- olsen_35above[complete.cases(olsen_35above), ]
olsen_35below <- olsen_35below[complete.cases(olsen_35below), ]
stp_35above <- stp_35above[complete.cases(stp_35above), ]
stp_35below <- stp_35below[complete.cases(stp_35below), ]


write.csv(bray_35above,"bray_35above.csv",row.names = F)
write.csv(bray_35below,"bray_35below.csv",row.names = F)
write.csv(olsen_35above,"olsen_35above.csv",row.names = F)
write.csv(olsen_35below,"olsen_35below.csv",row.names = F)
write.csv(stp_35above,"stp_35above.csv",row.names = F)
write.csv(stp_35below,"stp_35below.csv",row.names = F)


