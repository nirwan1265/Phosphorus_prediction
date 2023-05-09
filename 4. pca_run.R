library(factoextra)
library(FactoMineR)

####
# NOTE:
# Run 2. load_data.R and then run this script
####

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
<<<<<<< HEAD
#  FAMD 
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Bray afrlat
famd_bray_afrlac <- FAMD(bray_afrlac[,-1,18], graph = F)
quartz()
fviz_pca_ind(famd_bray_afrlac,
             title = "FAMD - Bray_afrlac - First two principal components",
             palette = "ucscgb",
             pointshape = 19,
             pointsize = 2,
             #fill = "lightgray",
             repel = TRUE,
             label = "none",
             col.ind = bray_afrlac$GEO3major)

### Bray global
famd_bray_global <- FAMD(bray_global[,-1,18], graph = F)
quartz()
fviz_pca_ind(famd_bray_global,
             title = "FAMD - Bray_global - First two principal components",
             palette = "ucscgb",
             pointshape = 19,
             pointsize = 2,
             #fill = "lightgray",
             repel = TRUE,
             label = "none",
             col.ind = bray_global$SOIL.TYPE)
table(bray_global$GEO3major)
=======
#  Remove constant/zero variance columns
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bray Global
bray_global_clean <- bray_global %>%
  #dplyr::mutate_if(is.factor, as.numeric) %>%
  dplyr::select_if(function(x) var(x, na.rm = TRUE) > 0) %>%
  dplyr::select(-c(1,2))


# Bray AfricaLat
bray_afrlat_clean <- bray_afrlat %>%
  #dplyr::mutate_if(is.factor, as.numeric) %>%
  dplyr::select_if(function(x) var(x, na.rm = TRUE) > 0) %>%
  dplyr::select(-c(1,2))



quartz()
par(mfrow = c(1,2))
famd_result <- FAMD(bray_afrlat[,-1], graph = F)
quartz()
fviz_pca_ind(famd_result,
             title = "Individuals - First two principal components",
             palette = "jco",
             pointshape = 21,
             pointsize = 2,
             fill = "lightgray",
             repel = TRUE,
             label = "none") # Use repel to avoid text overlapping


famd_result <- FAMD(bray_global[,-1], graph = F)
quartz()
fviz_pca_ind(famd_result,
             title = "Individuals - First two principal components",
             palette = "jco",
             pointshape = 21,
             pointsize = 2,
             fill = "lightgray",
             repel = TRUE,
             label = "none") # Use repel to avoid text overlapping

>>>>>>> 69e7c3b431bfc4233ed3bf6e6b71f68aec04d6b2
