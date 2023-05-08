#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Running Random Forrest 
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Bray afrlat
output_bray_afrlat<- randomForest(p_avg ~ .,  
                                  ntree=500, keep.forest=T,
                                  importance=T, mtry=3, 
                                  data = bray_afrlat[,-c(1,2)])


# Bray Global


output_bray_global<- randomForest(p_avg ~ .,  
                                  ntree=500, keep.forest=T,
                                  importance=T, mtry=3, 
                                  data = bray_global[,-c(1,2)])


# Olsen afrlat
output_olsen_afrlat<- randomForest(p_avg ~ .,  
                                   ntree=500, keep.forest=T,
                                   importance=T, mtry=3, 
                                   data = olsen_afrlat[,-c(1,2)])


# Olsen Global

output_olsen_global<- randomForest(p_avg ~ .,  
                                   ntree=500, keep.forest=T,
                                   importance=T, mtry=3, 
                                   data = olsen_global[,-c(1,2)])


### stp afrlat
output_stp_afrlat<- randomForest(p_avg ~ .,  
                                 ntree=500, keep.forest=T,
                                 importance=T, mtry=3, 
                                 data = stp_afrlat[,-c(1,2)])


# stp Global
output_stp_global<- randomForest(p_avg ~ .,  
                                 ntree=500, keep.forest=T,
                                 importance=T, mtry=3, 
                                 data = stp_global[,-c(1,2)])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Feature Selection - Importance
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bray afrlat
imp_bray_afrlat <- randomForest::importance(output_bray_afrlat, scale=T)
predict_bray_afrlat <- predict(output_bray_afrlat, data = bray_afrlat)
val_bray_afrlat <- data.frame(predict_bray_afrlat, bray_afrlat)

# Bray global - global test
imp_bray_global <- randomForest::importance(output_bray_global, scale=T)
predict_bray_global <- predict(output_bray_global, data = bray_global)
val_bray_global <- data.frame(predict_bray_global, bray_global)

# Bray global - latinafrlat test
imp_bray_global_lataf <- randomForest::importance(output_bray_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(bray_global[1,], bray_afrlat)
colnames(bray_global)
colnames(bray_afrlat)
colnames(xtest)
xtest <- xtest[-1,]
predict_bray_global_lataf <- predict(output_bray_global, newdata = xtest[,-c(1,2)])
val_bray_global_lataf <- data.frame(predict_bray_global, bray_global)


# Olsen afrlat
imp_olsen_afrlat <- randomForest::importance(output_olsen_afrlat, scale=T)
predict_olsen_afrlat <- predict(output_olsen_afrlat, data = olsen_afrlat)
val_olsen_afrlat <- data.frame(predict_olsen_afrlat, olsen_afrlat)

# Olsen Global
imp_olsen_global <- randomForest::importance(output_olsen_global, scale=T)
predict_olsen_global <- predict(output_olsen_global, data = olsen_global)
val_olsen_global <- data.frame(predict_olsen_global, olsen_global)

# Olsen Global - latinafrlat test
imp_olsen_global_lataf <- randomForest::importance(output_olsen_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(olsen_global[1,], olsen_afrlat)
xtest <- xtest[-1,]
colnames(xtest)
predict_olsen_global_lataf <- predict(output_olsen_global, newdata = xtest[,-c(1,2)])
val_olsen_global_lataf <- data.frame(predict_olsen_global, olsen_global)
dim(olsen_afrlat)
dim(olsen_global)
length(predict_olsen_global_lataf)


# stp afrlat
imp_stp_afrlat <- randomForest::importance(output_stp_afrlat, scale=T)
predict_stp_afrlat <- predict(output_stp_afrlat, data = stp_afrlat)
val_stp_afrlat <- data.frame(predict_stp_afrlat, stp_afrlat)

# stp global - global test
imp_stp_global <- randomForest::importance(output_stp_global, scale=T)
predict_stp_global <- predict(output_stp_global, data = stp_global)
val_stp_global <- data.frame(predict_stp_global, stp_global)

# stp global - latinafrlat test
imp_stp_global_lataf <- randomForest::importance(output_stp_global, scale=T)
# making class equal
# https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
xtest <- rbind(stp_global[1,], stp_afrlat)
colnames(stp_global)
colnames(stp_afrlat)
colnames(xtest)
xtest <- xtest[-1,]
predict_stp_global_lataf <- predict(output_stp_global, newdata = xtest[,-c(1,2)])
val_stp_global_lataf <- data.frame(predict_stp_global, stp_global)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Correlations and MSE
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bray afrlat
cor_bray_afrlat <- cor(predict_bray_afrlat,bray_afrlat$p_avg)
mse_bray_afrlat <- mean((predict_bray_afrlat - bray_afrlat$p_avg)^2)

# Bray global - global corelation
cor_bray_global <- cor(predict_bray_global,bray_global$p_avg)
mse_bray_global <- mean((predict_bray_global - bray_global$p_avg)^2)

# Bray global - Latafrlat corelation
cor_bray_global_lataf <- cor(predict_bray_global_lataf,bray_afrlat$p_avg)
mse_bray_global_lataf <- mean((predict_bray_global_lataf - bray_afrlat$p_avg)^2)

# olsen afrlat
cor_olsen_afrlat <- cor(predict_olsen_afrlat,olsen_afrlat$p_avg)
mse_olsen_afrlat <- mean((predict_olsen_afrlat - olsen_afrlat$p_avg)^2)

# olsen global - global corelation
cor_olsen_global <- cor(predict_olsen_global,olsen_global$p_avg)
mse_olsen_global <- mean((predict_olsen_global - olsen_global$p_avg)^2)

# olsen global - Latafrlat corelation
cor_olsen_global_lataf <- cor(predict_olsen_global_lataf,olsen_afrlat$p_avg)
mse_olsen_global_lataf <- mean((predict_olsen_global_lataf - olsen_afrlat$p_avg)^2)


# stp afrlat
cor_stp_afrlat <- cor(predict_stp_afrlat,stp_afrlat$p_avg)
mse_stp_afrlat <- mean((predict_stp_afrlat - stp_afrlat$p_avg)^2)

# stp global - global corelation
cor_stp_global <- cor(predict_stp_global,stp_global$p_avg)
mse_stp_global <- mean((predict_stp_global - stp_global$p_avg)^2)

# stp global - Latafrlat corelation
cor_stp_global_lataf <- cor(predict_stp_global_lataf,stp_afrlat$p_avg)
mse_stp_global_lataf <- mean((predict_stp_global_lataf - stp_afrlat$p_avg)^2)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
#  Final Table
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table <- data.frame(Model = c("Bray_Global","Bray_AfrLat","Bray_Global",
                              "Olsen_Global","Olsen_AfrLat","Olsen_Global",
                              "Stp_Global","Stp_AfrLat","Stp_Global"),
                    Validation_set = c("Global","AfrLat","AfrLat",
                                       "Global","AfrLat","AfrLat",
                                       "Global","AfrLat","AfrLat"),
                    Feature = c("Bray","Bray","Bray",
                                "Olsen","Olsen","Olsen",
                                "Stp","Stp","Stp"),
                    R2 = c(cor_bray_global,cor_bray_afrlat,cor_bray_global_lataf,
                           cor_olsen_global,cor_olsen_afrlat,cor_olsen_global_lataf,
                           cor_stp_global,cor_stp_afrlat,cor_stp_global_lataf),
                    MSE = c(mse_bray_global, mse_bray_afrlat, mse_bray_global_lataf,
                            mse_olsen_global, mse_olsen_afrlat, mse_olsen_global_lataf,
                            mse_stp_global, mse_stp_afrlat, mse_stp_global_lataf))
table