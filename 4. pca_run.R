library(factoextra)
library(FactoMineR)

####
# NOTE:
# Run 2. load_data.R and then run this script
####

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
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
             col.ind = bray_global$GEO3major)
table(bray_global$GEO3major)

### olsen afrlat
famd_olsen_afrlac <- FAMD(olsen_afrlac[,-1,17], graph = F)
quartz()
fviz_pca_ind(famd_olsen_afrlac,
             title = "FAMD - olsen_afrlac - First two principal components",
             palette = "ucscgb",
             pointshape = 19,
             pointsize = 2,
             #fill = "lightgray",
             repel = TRUE,
             label = "none",
             col.ind = olsen_afrlac$GEO3major)

### olsen global
famd_olsen_global <- FAMD(olsen_global[,-1,17], graph = F)
quartz()
fviz_pca_ind(famd_olsen_global,
             title = "FAMD - olsen_global - First two principal components",
             palette = "ucscgb",
             pointshape = 19,
             pointsize = 2,
             #fill = "lightgray",
             repel = TRUE,
             label = "none",
             col.ind = olsen_global$GEO3major)
table(olsen_global$GEO3major)

#  Remove constant/zero variance columns
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# olsen Global
olsen_global_clean <- olsen_global %>%
  #dplyr::mutate_if(is.factor, as.numeric) %>%
  dplyr::select_if(function(x) var(x, na.rm = TRUE) > 0) %>%
  dplyr::select(-c(1,2))


# olsen AfricaLat
olsen_afrlat_clean <- olsen_afrlat %>%
  #dplyr::mutate_if(is.factor, as.numeric) %>%
  dplyr::select_if(function(x) var(x, na.rm = TRUE) > 0) %>%
  dplyr::select(-c(1,2))



quartz()
par(mfrow = c(1,2))
famd_result <- FAMD(olsen_afrlat[,-1], graph = F)
quartz()
fviz_pca_ind(famd_result,
             title = "Individuals - First two principal components",
             palette = "jco",
             pointshape = 21,
             pointsize = 2,
             fill = "lightgray",
             repel = TRUE,
             label = "none") # Use repel to avoid text overlapping


famd_result <- FAMD(olsen_global[,-1], graph = F)
quartz()
fviz_pca_ind(famd_result,
             title = "Individuals - First two principal components",
             palette = "jco",
             pointshape = 21,
             pointsize = 2,
             fill = "lightgray",
             repel = TRUE,
             label = "none") # Use repel to avoid text overlapping


