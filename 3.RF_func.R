run_meta_model <- function(data, sampsize) {
  #set.seed(123)  
  # Splitting data into training and testing sets
  train_indices <- sample(nrow(data), 0.7 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Number of iterative models to run
  num_models <- 15
  
  # Required variables
  model_list <- list()
  train_predictions <- matrix(NA, nrow = nrow(train_data), ncol = num_models)
  test_predictions <- matrix(NA, nrow = nrow(test_data), ncol = num_models)
  
  # Train individual models in the ensemble
  for (i in 1:num_models) {
    # Bagging (Bootstrap aggregating) - random subset of training data
    bag_indices <- sample(nrow(train_data), replace = TRUE)
    bag_data <- train_data[bag_indices, ]
    
    # Random forest model with starta and sampsize
    model <- randomForest(p_avg ~ .,  
                          ntree = 200,
                          keep.forest = TRUE,
                          importance = TRUE,
                          mtry = 3,
                          sampsize = sampsize,
                          strata = train_data$GEO3major,
                          data = bag_data[, -c(1, 2, 17)])
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
  cv <- caret::trainControl(method = "cv", number = 10)
  
    # Training the meta-model using RRF (Regularized Random Forest) with cross-validation
  meta_model <- caret::train(p_avg ~ .,
                             data = meta_data,
                             method = "RRF",
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
  
  # R-squared
  train_r_squared <- cor(train_meta_predictions, train_data$p_avg)^2
  test_r_squared <- cor(test_meta_predictions, test_data$p_avg)^2
  
  # MSE
  train_mse <- mean((train_meta_predictions - train_data$p_avg)^2)
  test_mse <- mean((test_meta_predictions - test_data$p_avg)^2)
  
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
    )
  )
  
  return(results)
  
}

# Bray has 18 columns 
data <- bray_afrlac
sampsize <- 70
output_bray_afrlac <- run_meta_model(data, sampsize)

data <- bray_global
sampsize <- 70
output_bray_olsen <- run_meta_model(data, sampsize)

# Bray has 18 columns
data <- bray_35below
sampsize <- 70
output_bray_35below <- run_meta_model(data, sampsize)

# Olsen has 17 columns, need to change the number here data = bag_data[, -c(1, 2, 18)] in RF model
data <- olsen_afrlac
sampsize <- 180
output_olsen_afrlac <- run_meta_model(data, sampsize)

data <- olsen_global
sampsize <- 180
output_olsen_global <- run_meta_model(data, sampsize)

# Stp has 18 columns. 
data <- stp_afrlac
sampsize <- 310
output_stp_afrlac <- run_meta_model(data, sampsize)

data <- stp_global
sampsize <- 310
output_stp_global <- run_meta_model(data, sampsize)


