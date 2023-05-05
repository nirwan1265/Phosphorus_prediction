# Packages required
library(sp)
library(raster)
#install.packages("rgdal")
library(rgdal)

#raster
# Directory
dir_raster <- "~/Library/Mobile Documents/com~apple~CloudDocs/Research/Data/tif/"

# Pheno file
pheno <- read.csv("data/P_Olsen_mcdowell2023_predictors_he2022_LAC_AFR.csv")
pheno <- read.csv("data/Total_P_He.csv")

# raster files:
raster_files <- list.files(path = dir_raster, pattern = "\\.tif$", full.names = TRUE)

# raster_layer <- raster(raster_files)
# coords <- pheno[, c("Longitude", "Latitude")]
# coords_sf <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = 4326)
# 
# coords_sf <- st_as_sf(coords, coords = c("Latitude", "Longitude"), crs = 4326)
# values <- extract(raster_layer, coords_sf)
# 
# 
# print(crs(raster_layer))
var <- raster(raster_files)

# Changing the coordinate system
extract_raster_values <- function(raster_file, df) {
  r <- raster::brick(raster_file)
  #print(crs(r)) # Print the CRS
  coords <- df[, c("Longitude", "Latitude")]
  coords_sf <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = 4326)
  coords_transformed <- st_transform(coords_sf, crs(r))
  values <- raster::extract(x = r, y = coords_transformed)
  return(as.vector(values))
}


# Adding all the values
for (raster_file in raster_files) {
  # Get the variable name from the file name
  var_name <- sub("_0-.*", "", basename(raster_file))
  
  # Extract values for all coordinates in the pheno data frame
  extracted_values <- extract_raster_values(raster_file, pheno)
  
  # Add the extracted values as a new column in the pheno data frame
  pheno[[var_name]] <- extracted_values
}



pheno <- pheno[complete.cases(pheno), ]

write.csv(pheno,"Total_P_errosion.csv",row.names = F)
