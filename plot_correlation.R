bray_35below = read.csv("data/P_data/bray_35below.csv")
data <- bray_35below
train_indices <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Number of iterative models to run
num_models <- 15
sampsize <- 70
# Required variables
model_list <- list()
train_predictions <- matrix(NA, nrow = nrow(train_data), ncol = num_models)
test_predictions <- matrix(NA, nrow = nrow(test_data), ncol = num_models)
train_correlations <- vector(length=num_models)
test_correlations <- vector(length=num_models)


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
                        data = bag_data[, -c(2,3,4)])
  model_list[[i]] <- model
}

# For each model in the ensemble
for (i in 1:num_models) {
  
  # Generate predictions for the training and test data
  train_predictions[, i] <- predict(model_list[[i]], newdata = train_data)
  test_predictions[, i] <- predict(model_list[[i]], newdata = test_data)
  
  # Calculate correlation for the training and test predictions
  train_correlations[i] <- cor(train_predictions[, i], train_data$p_avg)
  test_correlations[i] <- cor(test_predictions[, i], test_data$p_avg)
  
}

# Create a data frame for correlations
correlation_df <- data.frame(Model = 1:num_models, 
                             Train_Correlation = train_correlations, 
                             Test_Correlation = test_correlations)


# Getting the ENSEMBLE training data
meta_model_bray_sorghum <- readRDS("bray_total_35below_sorghum.RDS")
ensemble_correlation <- meta_model_bray_sorghum[["model_performance"]][["correlation"]][["training"]]
min_model <- min(correlation_df$Model)
max_model <- max(correlation_df$Model)

x <- ggplot(correlation_df, aes(x = Model)) +
  geom_point(aes(y = Train_Correlation, color = "Train"), size = 5) +
  geom_line(aes(y = Train_Correlation, color = "Train")) +
  geom_point(aes(y = Test_Correlation, color = "Test"), size = 5) +
  geom_line(aes(y = Test_Correlation, color = "Test")) +
  geom_hline(aes(yintercept = ensemble_correlation), linetype = "dashed", color = "black") +
  annotate("segment", x = -Inf, xend = Inf, y = ensemble_correlation, yend = ensemble_correlation,
           colour = "black", linetype = "dashed") +
  geom_blank(aes(y = 0, color = "Ensemble Model")) +
  labs(title = "Bray Phosphorus Prediction", x = "Model Number", y = "Correlation") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red", "Ensemble Model" = "black"), 
                     name = "Legends",
                     labels = c("Ensemble Model","Test", "Train")) +
  scale_y_continuous(limits = c(0.35, max(ensemble_correlation, max(correlation_df$Train_Correlation, correlation_df$Test_Correlation)))) +
  scale_x_continuous(breaks = seq(from = min_model, to = max_model, by = 2)) +
  theme_minimal(base_size=30) +
  theme(legend.position = "top")


y <- x + scale_y_break(breaks = c(0.45,0.7), scales = "fixed", expand = T,space = 0.1,ticklabels = c(0.73,0.76,0.79))
quartz()
x

dev.off()
ggsave("my_plot.png", plot = x, width = 10, height = 6, dpi = 300)
y
