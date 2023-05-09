library(psych)
library(dplyr)
##### PCA
bray_africa <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_AFRLAC.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()
bray_global <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_global.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()
olsen_africa <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_AFRLAC.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()
olsen_global <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_global.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()

# Remove constant/zero variance columns
bray_africa_clean <- bray_africa %>% select_if(function(x) var(x, na.rm = TRUE) > 0)

# Perform PCA on the cleaned data
pca_bray_africa <- prcomp(bray_africa_clean, center = TRUE, scale. = TRUE)
quartz()
pairs.panels(pca_bray_africa$x[,1:3],
             gap = 0,
             #bg = c("red", "yellow", "blue")[bray_africa_clean$ELEVATION],
             pch = 21)

# Remove constant/zero variance columns
bray_global_clean <- bray_global %>% select_if(function(x) var(x, na.rm = TRUE) > 0)

# Perform PCA on the cleaned data
pca_bray_global <- prcomp(bray_global_clean, center = TRUE, scale. = TRUE)
quartz()


quartz()
par(mfrow = c(1,2))
plot(pca_bray_africa$x[,1:2])
plot(pca_bray_global$x[,1:2])
     
     