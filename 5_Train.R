source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")


train_model <- function(fts,
                        model,
                        train,
                        kfold = 0,
                        train_validate_ratio = 0.8,
                        parallel = TRUE,
                        use_tunred_HP = NULL,
                        min_child_weight,
                        early_stopping_rounds,
                        seed = 1,
                        return_pred_only = FALSE,
                        ...) {
  # Garbage collection to free up memory
  gc()
  
  # Extract model specifications
  weight <- model_spec[[model]]$exposure
  response <- model_spec[[model]]$response
  objective <- model_spec[[model]]$objective
  eval_metric <- model_spec[[model]]$eval_metric
  
  # Filter training data to include only rows with positive weight
  train <- train[train[[weight]] > 0]
  train_y <- train[[response]]
  train_weight <- train[[weight]]
  
  # Create training and validation samples
  sample_result <- tryCatch({
    if (kfold > 0) {
      KT_create_sample(df = train, weight = train_weight, y = train_y, kfold = kfold)
    } else {
      KT_create_sample(df = train, weight = train_weight, y = train_y, train_validate_split = train_validate_ratio)
    }
  }, error = function(e) {
    stop("Error in creating training and validation samples: ", e$message)
  })
  
  # Adjust min_child_weight based on the length of training weights
  min_child_weight <- min_child_weight * length(sample_result$train_weight)
  
  # Set parameters for the model
  params <- if (!is.null(use_tunred_HP)) {
    use_tunred_HP
  } else {
    list(objective = objective, eval_metric = eval_metric, min_child_weight = min_child_weight, ...)
  }
  
  # Determine the number of threads to use for parallel processing
  nthread <- if (parallel) detectCores() else -1
  
  # Train the model using cross-validation or train-validate split
  train_result <- tryCatch({
    if (kfold > 0) {
      KT_xgb_cv(
        train = sample_result$train %>% select(fts),
        train_y = sample_result$train_y,
        train_weight = sample_result$train_weight,
        folds = sample_result$kfold,
        params = params,
        nthread = nthread
      )
    } else {
      KT_xgb_train(
        train = sample_result$train %>% select(fts),
        train_y = sample_result$train_y,
        train_weight = sample_result$train_weight,
        validate = sample_result$validate %>% select(fts),
        validate_y = sample_result$validate_y,
        validate_weight = sample_result$validate_weight,
        params = params,
        nthread = nthread,
        early_stopping_rounds = early_stopping_rounds,
        seed = seed
      )
    }
  }, error = function(e) {
    stop("Error in training the model: ", e$message)
  })
  
  # Return predictions only if specified
  if (return_pred_only) {
    return(predict(train_result$model, newdata = as.matrix(train %>% select(fts)), type = "response"))
  } else {
    # Explain the model and generate predictions
    explain_result <- tryCatch({
      KT_xgb_explain(model = train_result$model, pred_data = sample_result$train %>% select(fts))
    }, error = function(e) {
      stop("Error in explaining the model: ", e$message)
    })
    
    pred <- tryCatch({
      predict(train_result$model, newdata = as.matrix(mltools::one_hot(train %>% select(fts))), type = "response")
    }, error = function(e) {
      stop("Error in generating predictions: ", e$message)
    })
    
    # Return the results including importance plots and SHAP values
    return(list(
      imp_plot = list(
        imp_gain = train_result$imp_plot,
        imp_shap = explain_result$ft_importance_plot,
        imp_comparison = KT_plot_compare_ft_imp(
          train_result$imp_plot$data %>% arrange(Gain) %>% select(Feature) %>% pull,
          explain_result$ft_importance_plot$data %>% arrange(pc_contri) %>% select(variable) %>% pull
        ) + theme(legend.position = "none") + theme_light(base_size = 18) + ggtitle("gain vs SHAP importance"),
        imp_shap_X = explain_result$ft_importance_X,
        EIXinteraction_gain_matrix = explain_result$EIXimportance_matrix,
        EIX_gain = explain_result$EIXimportance,
        EIXinteraction_gain = explain_result$EIXimportanceX
      ),
      shap_values = list(
        main_effect = explain_result$main_effect,
        interaction_effect = explain_result$interaction
      ),
      model = train_result$model,
      pred = pred,
      pmml_fmap = r2pmml::as.fmap(as.data.table(sample_result$train %>% select(fts)))
    ))
  }
  
  # Garbage collection to free up memory
  gc()
}


