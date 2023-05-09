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
             col.ind = bray_global$SOIL.TYPE)
table(bray_global$GEO3major)
