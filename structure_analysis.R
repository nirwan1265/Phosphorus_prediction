library(psych)
library(dplyr)
library(GPArotation)
library(rrcov3way)


##### PCA
bray_africa <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_AFRLAC.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()
bray_global <- read.csv("data/filtered_data/P_Bray_mcdowell2023_predictors_he2022_global.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()
olsen_africa <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_AFRLAC.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()
olsen_global <- read.csv("data/filtered_data/P_Olsen_mcdowell2023_predictors_he2022_global.csv") %>% dplyr::select(-c(1,2,3)) %>% na.omit()

# Remove constant/zero variance columns
bray_africa_clean <- bray_africa %>% select_if(function(x) var(x, na.rm = TRUE) > 0)

# Perform PCA on the cleaned data
pca_bray_africa <- prcomp(bray_africa_clean, center = TRUE, scale. = TRUE)
# pairs.panels(pca_bray_africa$x[,1:5],
#              gap = 0,
#              #bg = c("red", "yellow", "blue")[bray_africa_clean$ELEVATION],
#              pch = 21)

# Remove constant/zero variance columns
bray_global_clean <- bray_global %>% select_if(function(x) var(x, na.rm = TRUE) > 0)

# Perform PCA on the cleaned data
pca_bray_global <- prcomp(bray_global_clean, center = TRUE, scale. = TRUE)
quartz()
par(mfrow = c(1,2))
plot(pca_bray_africa$x[,1:2])
plot(pca_bray_global$x[,1:2])
     

# Proportion of variance explained
pca_var_bray_global <- summary(pca_bray_global)
pca_var_bray_africa <- summary(pca_bray_africa)

prop_bray_global <- pca_var_bray_global[["importance"]][2,]
prop_bray_africa <- pca_var_bray_africa[["importance"]][2,]


plot(prop_bray_global,prop_bray_africa)
cor(prop_bray_global,prop_bray_africa)



### Correlation of first 2 PCA's
# https://medium.com/@data.dev.backyard/comparing-similarity-of-two-datasets-using-pca-a-technical-review-of-principal-component-analysis-94e528e4b191
pca_2_global <- as.data.frame(pca_bray_global[["rotation"]][,1:2])
pca_2_global <- c(unlist(pca_2_global["PC1"]),unlist(pca_2_global["PC2"]))
names(pca_2_global) <- NULL

pca_2_africa <- as.data.frame(pca_bray_africa[["rotation"]][,1:2])
pca_2_africa <- c(unlist(pca_2_africa["PC1"]),unlist(pca_2_africa["PC2"]))
names(pca_2_africa) <- NULL

plot(pca_2_global,pca_2_africa)
cor(pca_2_global,pca_2_africa)


##### Congruence
#factor congruence of factors and components, both rotated
round(congruence(pca_bray_africa$rotation[,1],pca_bray_global$rotation[,1]),3)


