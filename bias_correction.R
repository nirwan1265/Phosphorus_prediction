# EDM function
empirical_distribution_matching <- function(predictions, observations) {
  # Compute the ECDF of the observed values
  ecdf_observed <- ecdf(observations)
  
  # For each prediction, find its quantile in the predicted ECDF
  ecdf_predicted <- ecdf(predictions)
  quantiles <- ecdf_predicted(predictions)
  
  # Use the quantile to get the corresponding value from the observed ECDF
  adjusted_predictions <- quantile(observations, probs = quantiles)
  
  return(adjusted_predictions)
}

# Random forest model:
run_meta_model <- function(data, sampsize) {
  
  # Converting columns to factors
  columns_to_factor <- c("BEDROCK", "SOIL.USDA", "BIOME", "GEO3major")
  for (column in columns_to_factor) {
    data[[column]] <- as.factor(data[[column]])
  }
  
  # Splitting data into training and testing sets
  train_indices <- data$data == "train"
  train_data <- data[train_indices, ]
  test_data <- data[!train_indices, ]
  
  # Ensuring same levels in both train_data and test_data
  for (column in columns_to_factor) {
    union_levels <- levels(data[[column]])
    train_data[[column]] <- factor(train_data[[column]], levels = union_levels)
    test_data[[column]] <- factor(test_data[[column]], levels = union_levels)
  }
  
  # Store the row names of the test data
  test_row_names <- rownames(test_data)
  
  # Exclude unwanted columns
  exclude_cols <- c("LONGITUDE", "LATITUDE", "GEO3major", "data")
  train_data <- train_data[, !(names(train_data) %in% exclude_cols)]
  test_data <- test_data[, !(names(test_data) %in% exclude_cols)]
  
  # Required variables
  num_models <- 15
  model_list <- list()
  train_predictions <- matrix(NA, nrow = nrow(train_data), ncol = num_models)
  test_predictions <- matrix(NA, nrow = nrow(test_data), ncol = num_models)
  
  # Train individual models in the ensemble
  for (i in 1:num_models) {
    # Bagging (Bootstrap aggregating) - random subset of training data
    bag_indices <- sample(nrow(train_data), replace = TRUE)
    bag_data <- train_data[bag_indices, ]
    
    # Random forest model with strata and sampsize
    model <- randomForest(p_avg ~ .,  
                          ntree = 200,
                          keep.forest = TRUE,
                          importance = TRUE,
                          mtry = 3,
                          sampsize = sampsize,
                          strata = train_data$GEO3major,
                          data = bag_data)
    model_list[[i]] <- model
  }
  
  # Predictions for each individual model
  for (i in 1:num_models) {
    train_predictions[, i] <- predict(model_list[[i]], newdata = train_data)
    test_predictions[, i] <- predict(model_list[[i]], newdata = test_data)
  }
  
  # Renaming Training Columns
  colnames(train_predictions) <- paste("Model", 1:num_models)
  
  # Combining Predictions and Actual Values to create meta-data
  meta_data <- cbind(train_predictions, p_avg = train_data$p_avg)
  
  # Cross-validation using 5 folds
  cv <- caret::trainControl(method = "cv", number = 5)
  
  # Training the meta-model using RRF (Regularized Random Forest) with cross-validation
  meta_model <- caret::train(p_avg ~ .,
                             data = meta_data,
                             method = "RRF",
                             trControl = cv)
  
  # Renaming Testing Columns
  colnames(test_predictions) <- colnames(train_predictions)
  
  # Meta-predictions for the training and test sets from stacking
  train_meta_predictions <- predict(meta_model, newdata = train_predictions)
  test_meta_predictions <- predict(meta_model, newdata = test_predictions)
  
  # Apply EDM to adjust the predictions
  train_meta_predictions_adjusted <- empirical_distribution_matching(train_meta_predictions, train_data$p_avg)
  # For test predictions, use the training data's observed values to adjust
  test_meta_predictions_adjusted <- empirical_distribution_matching(test_meta_predictions, train_data$p_avg)
  
  # Adding the stored row names to the final output dataframe
  test_prediction_final <- data.frame(Taxa = test_row_names, Predictions = as.vector(test_meta_predictions_adjusted))
  
  #### Model Performances
  # Correlation
  train_correlation <- cor(train_meta_predictions_adjusted, train_data$p_avg)
  test_correlation <- "NA"
  
  # R-squared
  train_r_squared <- cor(train_meta_predictions_adjusted, train_data$p_avg)^2
  test_r_squared <- "NA"
  
  # MSE
  train_mse <- mean((train_meta_predictions_adjusted - train_data$p_avg)^2)
  test_mse <- "NA"
  
  # Constructing the nested list
  results <- list(
    model = model_list,
    meta_model = meta_model,
    model_performance = list(
      correlation = list(
        training = train_correlation,
        testing = test_correlation
      ),
      r_squared = list(
        training = train_r_squared,
        testing = test_r_squared
      ),
      mse = list(
        training = train_mse,
        testing = test_mse
      )
    ),
    Prediction = test_prediction_final
  )
  
  return(results)
}
