#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Running Random Forrest 
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Bray afrlac

output_bray_afrlac<- randomForest(p_avg ~ .,  
                                  ntree=500, keep.forest=T,
                                  importance=T, mtry=3, 
                                  sampsize=70,
                                  strata=bray_afrlac$GEO3major,
                                  data = bray_afrlac[,-c(1,2,18)])




# Bray Global


output_bray_global<- randomForest(p_avg ~ .,  
                                  ntree=500, keep.forest=T,
                                  importance=T, mtry=3, 
                                  # sampsize=70,
                                  # strata=bray_global$GEO3major,
                                  data = bray_global[,-c(1,2,18)])


# Olsen afrlac
output_olsen_afrlac<- randomForest(p_avg ~ .,  
                                   ntree=500, keep.forest=T,
                                   importance=T, mtry=3, 
                                   sampsize=180,
                                   strata = olsen_afrlac$GEO3major,
                                   data = olsen_afrlac[,-c(1,2,17)])


# Olsen Global

output_olsen_global<- randomForest(p_avg ~ .,  
                                   ntree=500, keep.forest=T,
                                   importance=T, mtry=3, 
                                   # sampsize=180,
                                   # strata=olsen_global$GEO3major,
                                   data = olsen_global[,-c(1,2,17)])


### stp afrlac
output_stp_afrlac<- randomForest(p_avg ~ .,  
                                 ntree=500, keep.forest=T,
                                 importance=T, mtry=3, 
                                 sampsize=310,
                                 strata=stp_afrlat$GEO3major,
                                 data = stp_afrlac[,-c(1,2,18)])


# stp Global
output_stp_global<- randomForest(p_avg ~ .,  
                                 ntree=500, keep.forest=T,
                                 importance=T, mtry=3, 
                                 # sampsize=310,
                                 # strata=stp_global$GEO3major,
                                 data = stp_global[,-c(1,2,18)])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Feature Selection - Importance
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bray afrlac
imp_bray_afrlac <- randomForest::importance(output_bray_afrlac, scale=T)
predict_bray_afrlac <- predict(output_bray_afrlac, data = bray_afrlac)
val_bray_afrlac <- data.frame(predict_bray_afrlac, bray_afrlac)

# Bray global - global test
imp_bray_global <- randomForest::importance(output_bray_global, scale=T)
predict_bray_global <- predict(output_bray_global, data = bray_global)
val_bray_global <- data.frame(predict_bray_global, bray_global)

# Bray global - latinafrlac test
imp_bray_global_lataf <- randomForest::importance(output_bray_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(bray_global[1,], bray_afrlac)
colnames(bray_global)
colnames(bray_afrlac)
colnames(xtest)
xtest <- xtest[-1,]
predict_bray_global_lataf <- predict(output_bray_global, newdata = xtest[,-c(1,2)])
val_bray_global_lataf <- data.frame(predict_bray_global, bray_global)


# Olsen afrlac
imp_olsen_afrlac <- randomForest::importance(output_olsen_afrlac, scale=T)
predict_olsen_afrlac <- predict(output_olsen_afrlac, data = olsen_afrlac)
val_olsen_afrlac <- data.frame(predict_olsen_afrlac, olsen_afrlac)

# Olsen Global
imp_olsen_global <- randomForest::importance(output_olsen_global, scale=T)
predict_olsen_global <- predict(output_olsen_global, data = olsen_global)
val_olsen_global <- data.frame(predict_olsen_global, olsen_global)

# Olsen Global - latinafrlac test
imp_olsen_global_lataf <- randomForest::importance(output_olsen_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(olsen_global[1,], olsen_afrlac)
xtest <- xtest[-1,]
colnames(xtest)
predict_olsen_global_lataf <- predict(output_olsen_global, newdata = xtest[,-c(1,2)])
val_olsen_global_lataf <- data.frame(predict_olsen_global, olsen_global)
dim(olsen_afrlac)
dim(olsen_global)
length(predict_olsen_global_lataf)


# stp afrlac
imp_stp_afrlac <- randomForest::importance(output_stp_afrlac, scale=T)
predict_stp_afrlac <- predict(output_stp_afrlac, data = stp_afrlac)
val_stp_afrlac <- data.frame(predict_stp_afrlac, stp_afrlac)

# stp global - global test
imp_stp_global <- randomForest::importance(output_stp_global, scale=T)
predict_stp_global <- predict(output_stp_global, data = stp_global)
val_stp_global <- data.frame(predict_stp_global, stp_global)

# stp global - latinafrlac test
imp_stp_global_lataf <- randomForest::importance(output_stp_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(stp_global[1,], stp_afrlac)
colnames(stp_global)
colnames(stp_afrlac)
colnames(xtest)
xtest <- xtest[-1,]
predict_stp_global_lataf <- predict(output_stp_global, newdata = xtest[,-c(1,2)])
val_stp_global_lataf <- data.frame(predict_stp_global, stp_global)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Correlations and MSE
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bray afrlac
cor_bray_afrlac <- cor(predict_bray_afrlac,bray_afrlac$p_avg)
mse_bray_afrlac <- mean((predict_bray_afrlac - bray_afrlac$p_avg)^2)

# Bray global - global corelation
cor_bray_global <- cor(predict_bray_global,bray_global$p_avg)
mse_bray_global <- mean((predict_bray_global - bray_global$p_avg)^2)

# Bray global - Latafrlac corelation
cor_bray_global_lataf <- cor(predict_bray_global_lataf,bray_afrlac$p_avg)
mse_bray_global_lataf <- mean((predict_bray_global_lataf - bray_afrlac$p_avg)^2)

# olsen afrlac
cor_olsen_afrlac <- cor(predict_olsen_afrlac,olsen_afrlac$p_avg)
mse_olsen_afrlac <- mean((predict_olsen_afrlac - olsen_afrlac$p_avg)^2)

# olsen global - global corelation
cor_olsen_global <- cor(predict_olsen_global,olsen_global$p_avg)
mse_olsen_global <- mean((predict_olsen_global - olsen_global$p_avg)^2)

# olsen global - Latafrlac corelation
cor_olsen_global_lataf <- cor(predict_olsen_global_lataf,olsen_afrlac$p_avg)
mse_olsen_global_lataf <- mean((predict_olsen_global_lataf - olsen_afrlac$p_avg)^2)


# stp afrlac
cor_stp_afrlac <- cor(predict_stp_afrlac,stp_afrlac$p_avg)
mse_stp_afrlac <- mean((predict_stp_afrlac - stp_afrlac$p_avg)^2)

# stp global - global corelation
cor_stp_global <- cor(predict_stp_global,stp_global$p_avg)
mse_stp_global <- mean((predict_stp_global - stp_global$p_avg)^2)

# stp global - Latafrlac corelation
cor_stp_global_lataf <- cor(predict_stp_global_lataf,stp_afrlac$p_avg)
mse_stp_global_lataf <- mean((predict_stp_global_lataf - stp_afrlac$p_avg)^2)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Final Table
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table <- data.frame(Model = c("Bray_Global","Bray_afrlac","Bray_Global",
                              "Olsen_Global","Olsen_afrlac","Olsen_Global",
                              "Stp_Global","Stp_afrlac","Stp_Global"),
                    Validation_set = c("Global","afrlac","afrlac",
                                       "Global","afrlac","afrlac",
                                       "Global","afrlac","afrlac"),
                    Feature = c("Bray","Bray","Bray",
                                "Olsen","Olsen","Olsen",
                                "Stp","Stp","Stp"),
                    R2 = c(cor_bray_global,cor_bray_afrlac,cor_bray_global_lataf,
                           cor_olsen_global,cor_olsen_afrlac,cor_olsen_global_lataf,
                           cor_stp_global,cor_stp_afrlac,cor_stp_global_lataf),
                    MSE = c(mse_bray_global, mse_bray_afrlac, mse_bray_global_lataf,
                            mse_olsen_global, mse_olsen_afrlac, mse_olsen_global_lataf,
                            mse_stp_global, mse_stp_afrlac, mse_stp_global_lataf))
table