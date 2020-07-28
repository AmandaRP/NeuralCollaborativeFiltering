# Main script to run. Calls readData.R and NCF.R to read/wrangle data and build
# model architecture. This script also trains the model and provides evaluation metrics.

# Read and wrangle data: ----------------------------------------------------

movielense <- TRUE # set to false to use pinterest data
source("readData.R")

# Define model --------------------------------------------------------------

source("NCF.R")

# Train model -------------------------------------------------------------

# First define callbacks to stop model early when validation loss increases and to save best model
callback_list <- list(
  callback_early_stopping(patience = 2),
  callback_model_checkpoint(filepath = "model.h5", monitor = "val_loss", save_best_only = TRUE)
)
  
history <- 
  model %>% 
  fit(
    x = list(user_input = as.array(train$user), 
         item_input = as.array(train$item)),
    y = as.array(train$label),
    epochs = 10,
    batch_size = 2048, 
    validation_data = list(list(user_input = as.array(validation$user), 
                                item_input = as.array(validation$item)), 
                           as.array(validation$label)),
    shuffle = TRUE, 
    callbacks = callback_list
  ) 

# Load best model:
model <- load_model_hdf5("my_model.h5")

# Evaluate results --------------------------------------------------------

history
#plot(history)

# Evaluate returns same metrics that were defined in the compile (accuracy in this case)
(results <- model %>% evaluate(list(test$user, test$item), test$label))

# Get predictions for test set:
test_pred <- model %>% 
  predict(x = list(test$user, test$item)) %>%
  bind_cols(pred = ., test)

# Compute hit rate and ndcg
source("evaluation.R")
compute_hr(test_pred, 10)
compute_ndcg(test_pred, 10)










