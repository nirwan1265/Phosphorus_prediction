set.seed(123)  
#Splitting data into training and testing sets
train_indices <- sample(nrow(bray_afrlac), 0.7 * nrow(bray_afrlac))
train_data <- bray_afrlac[train_indices, ]
test_data <- bray_afrlac[-train_indices, ]

# Number of iterative models to run
num_models <- 15

# Required variables
model_list <- list()
train_predictions <- matrix(NA, nrow = nrow(train_data), ncol = num_models)
test_predictions <- matrix(NA, nrow = nrow(test_data), ncol = num_models)

# Train individual models in the ensemble
for (i in 1:num_models) {
  # Bagging (Bootstrap aggregating) - random subset of training data, 
  bag_indices <- sample(nrow(train_data), replace = TRUE)
  bag_data <- train_data[bag_indices, ]
  
  # Random forest model with starta and sampsize 
  model <- randomForest(p_avg ~ .,  
                        ntree = 200,
                        keep.forest = TRUE,
                        importance = TRUE,
                        mtry = 3,
                        sampsize = 70,
                        strata = train_data$GEO3major,
                        data = bag_data[, -c(1, 2, 18)])
  model_list[[i]] <- model
}

# Predictions for each individual model
for (i in 1:num_models) {
  train_predictions[, i] <- predict(model_list[[i]], newdata = train_data)
  test_predictions[, i] <- predict(model_list[[i]], newdata = test_data)
}

#Renaming Training Columns
colnames(train_predictions) <- paste("Model", 1:num_models)


# Combining Predictions and Actual Values to create meta-data
meta_data <- cbind(train_predictions, p_avg = train_data$p_avg)

# Cross-validation using 5 folds
cv <- trainControl(method = "cv", number = 10)

# Training the meta-model using RRF (Regularized Random Forrest) with cross-validation
meta_model <- caret::train(p_avg ~ ., 
                    data = meta_data,
                    method = "RRF",
                    #trControl = trainControl(method = "none"))
                    trControl = cv)

# Renaming Testing Columns
colnames(test_predictions) <- colnames(train_predictions)


# Meta-predictions for the training and test sets from staking
train_meta_predictions <- predict(meta_model, newdata = train_predictions)
test_meta_predictions <- predict(meta_model, newdata = test_predictions)


#### Model Performances
# Correlation
train_correlation <- cor(train_meta_predictions, train_data$p_avg)
test_correlation <- cor(test_meta_predictions, test_data$p_avg)

# R_squared
train_r_squared <- cor(train_meta_predictions, train_data$p_avg)^2
test_r_squared <- cor(test_meta_predictions, test_data$p_avg)^2

# MSE
train_mse <- mean((train_meta_predictions - train_data$p_avg)^2)
test_mse <- mean((test_meta_predictions - test_data$p_avg)^2)


# Printing the results
print(paste("Correlation coefficient for training data:", train_correlation))
print(paste("Correlation coefficient for test data:", test_correlation))
print(paste("R-squared for training data:", train_r_squared))
print(paste("R-squared for test data:", test_r_squared))
print(paste("MSE for training data:", train_mse))
print(paste("MSE for test data:", test_mse))
