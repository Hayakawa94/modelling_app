tune_model <- function(fts,
                       model,
                       train,
                       kfold = 0,
                       train_validate_ratio = 0.8,
                       eta = c(0.01, 0.1),
                       max_depth = c(2L, 5L),
                       min_child_weight = c(1, 100),
                       subsample = c(0.7, 1),
                       colsample_bytree = c(0.7, 1),
                       lambda = c(3, 3),
                       alpha = c(3, 3),
                       monotone_constraints,
                       interaction_constraints,
                       gamma = c(1, 1),
                       nrounds = 100,
                       parallel = TRUE,
                       iters.k = 1,
                       iters.n = 4,
                       ncluster = parallel::detectCores() - 1,
                       initPoints = 10) {
  # Garbage collection to free up memory
  gc()
  
  # Check data types of columns in train
  invalid_cols <- list()
  for(x in names(train %>% select(fts))){
    if(!any(class(train[[x]])  %in% c("numeric", "factor", "integer") )){
      invalid_cols[[x]] = class(train[[x]])
    }
  }
  
  if (length(invalid_cols)>0) {
    print(invalid_cols)
    stop(paste("Error: The train data contains columns with invalid data types(" , names(invalid_cols),  "). Only numeric, factor, and integer types are allowed." ))
  }
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
  sample_result <- 
    if (kfold > 0) {
      KT_create_sample(df = train, weight = train_weight, y = train_y, kfold = kfold)
    } else {
      KT_create_sample(df = train, weight = train_weight, y = train_y, train_validate_split = train_validate_ratio)
    }
  
  # Adjust min_child_weight based on the length of training weights
  min_child_weight <- min_child_weight * length(sample_result$train_weight)
  
  # Define parameter bounds
  bounds <- list(
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    lambda = lambda,
    alpha = alpha,
    gamma = gamma
  )
  
  # Separate fixed parameters from bounds
  fixed_param <- lapply(bounds, function(x) if (x[1] == x[2]) x[1] else NULL) %>% setNames(names(bounds)) %>% compact()
  bounds <- lapply(bounds, function(x) if (x[1] == x[2]) NULL else x) %>% setNames(names(bounds)) %>% compact()
  
  # Perform Bayesian tuning with cross-validation or train-validate split
  result <- 
    if (kfold > 0) {
      KT_xgb_baysian_tune(
        train = sample_result$train %>% select(fts),
        train_y = sample_result$train_y,
        train_weight = sample_result$train_weight,
        folds = sample_result$kfold,
        bounds = bounds,
        HP_fixed = fixed_param,
        monotone_constraints = monotone_constraints,
        interaction_constraints = interaction_constraints,
        nrounds = nrounds,
        objective = objective,
        eval_metric = eval_metric,
        parallel = parallel,
        iters.k = iters.k,
        iters.n = iters.n,
        ncluster = ncluster,
        initPoints = initPoints
      )
    } else {
      KT_xgb_baysian_tune(
        train = sample_result$train %>% select(fts),
        train_y = sample_result$train_y,
        train_weight = sample_result$train_weight,
        validate = sample_result$validate %>% select(fts),
        validate_y = sample_result$validate_y,
        validate_weight = sample_result$validate_weight,
        bounds = bounds,
        HP_fixed = fixed_param,
        monotone_constraints = monotone_constraints,
        interaction_constraints = interaction_constraints,
        nrounds = nrounds,
        objective = objective,
        eval_metric = eval_metric,
        parallel = parallel,
        iters.k = iters.k,
        iters.n = iters.n,
        ncluster = ncluster,
        initPoints = initPoints
      )
    }
  gc()
  
  return(result)
}