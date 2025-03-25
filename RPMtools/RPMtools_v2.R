# Install and load necessary packages
required_packages <- c(
  'tidyverse', 'odbc', 'dbplyr', 'data.table', 'CatEncoders', 'glue', 'plotly',
  'htmltools', 'dplyr', 'sf', 'gridExtra', 'tidyr', 'lubridate', 'reshape2',
  'reticulate', 'ggplot2', 'ParBayesianOptimization', 'mlbench', 'resample',
  'xgboost', 'Matrix', 'pracma', 'RColorBrewer', 'cartogram', 'tmap', 'spdep',
  'deldir', 'sp', 'purrr', 'DescTools', 'readxl', 'openxlsx', 'fastglm',
  'dtplyr', 'pbapply', 'patchwork', 'shiny', 'writexl'
)

new_packages <- setdiff(required_packages, installed.packages()[, "Package"])
if (length(new_packages) > 0) install.packages(new_packages)

invisible(lapply(required_packages, library, character.only = TRUE))

# Options
options(scipen = 999)

# Utility: Create Equal Bins
KT_create_equal_bin <- function(weight, nbin) {
  if (any(is.na(weight)) || any(weight < 0)) stop("Invalid weights: Weights must be non-negative and not NA.")
  if (length(weight) == 0) stop("Weight vector is empty.")
  if (nbin <= 0) stop("Number of bins must be a positive integer.")
  
  cumulative_sum <- cumsum(weight)
  bins <- cut(cumulative_sum, breaks = nbin, labels = FALSE)
  if (any(is.na(bins))) warning("Some bins are empty due to insufficient data.")
  return(bins)
}

# Gini Calculation
KT_calc_gini <- function(actual, weight, predicted) {
  if (length(actual) != length(weight) || length(actual) != length(predicted)) {
    stop("Input vectors 'actual', 'weight', and 'predicted' must have the same length.")
  }
  if (any(is.na(actual)) || any(is.na(weight)) || any(is.na(predicted))) {
    stop("Inputs contain NA values. Please remove or impute them.")
  }
  if (all(weight == 0)) stop("All weights are zero. Cannot compute Gini.")
  
  df <- data.frame(actual = as.numeric(actual), weight = as.numeric(weight), predicted)
  sorted_idx <- order(df$predicted)
  w_s <- df$weight[sorted_idx]
  a_s <- df$actual[sorted_idx]
  a_c <- cumsum(a_s * w_s)
  w_c <- cumsum(w_s)
  gini <- 1 - 2 * pracma::trapz(w_c / max(w_c), a_c / max(a_c))
  return(gini)
}

# Normalized Gini
KT_calc_gini_norm <- function(actual, weight, predicted) {
  return(KT_calc_gini(actual, weight, predicted) / KT_calc_gini(actual, weight, actual))
}

# Resample Gini
KT_resample_gini <- function(n, actual, weight, predicted, normalize = FALSE) {
  if (n <= 0) stop("Number of resamples must be a positive integer.")
  if (any(weight < 0)) warning("Negative weights detected; results may be incorrect.")
  
  gini_vector <- numeric(n)
  df <- data.frame(actual, weight, predicted)
  set.seed(123)
  for (i in seq_len(n)) {
    sampled_df <- df[sample(nrow(df), replace = TRUE), ]
    gini_vector[i] <- if (normalize) {
      KT_calc_gini_norm(sampled_df$actual, sampled_df$weight, sampled_df$predicted)
    } else {
      KT_calc_gini(sampled_df$actual, sampled_df$weight, sampled_df$predicted)
    }
  }
  return(gini_vector)
}

# Gini Plotting
KT_plot_compare_gini <- function(n, actual, weight, base, challenger, normalize = FALSE) {
  base_gini <- KT_resample_gini(n, actual, weight, base, normalize)
  challenger_gini <- KT_resample_gini(n, actual, weight, challenger, normalize)
  challenger_win_rate <- mean(challenger_gini > base_gini)
  gini_df <- data.frame(Model = rep(c("Base", "Challenger"), each = n), Gini = c(base_gini, challenger_gini))
  ggplot(gini_df, aes(x = Gini, fill = Model)) +
    geom_density(alpha = 0.3) +
    ggtitle(glue("Gini Comparison | Challenger Win Rate: {scales::percent(challenger_win_rate)}"))
}

# Weighted R-squared
KT_weighted_Rsq <- function(actual, pred, weight) {
  if (length(actual) != length(pred) || length(actual) != length(weight)) {
    stop("Input vectors 'actual', 'pred', and 'weight' must have the same length.")
  }
  if (any(is.na(actual)) || any(is.na(pred)) || any(is.na(weight))) {
    stop("Inputs contain NA values. Please remove or impute them.")
  }
  if (all(weight == 0)) stop("All weights are zero. Cannot compute weighted R-squared.")
  
  residual_ss <- sum(((actual - pred)^2) * weight)
  total_ss <- sum(((actual - mean(actual))^2) * weight)
  r_squared <- 1 - residual_ss / total_ss
  return(r_squared)
}

# Lift Calculation
KT_calc_lift <- function(pred, actual, weight, nbin) {
  if (length(pred) != length(actual) || length(actual) != length(weight)) {
    stop("Input vectors 'pred', 'actual', and 'weight' must have the same length.")
  }
  if (nbin <= 0) stop("Number of bins must be a positive integer.")
  
  pred <- pred * (sum(actual) / sum(pred * weight))  # Rebase predictions
  lift_df <- data.frame(pred, actual, weight) %>%
    filter(weight > 0) %>%
    arrange(pred) %>%
    mutate(pred = pred * weight, bin = KT_create_equal_bin(weight, nbin)) %>%
    group_by(bin) %>%
    summarise(across(everything(), sum)) %>%
    mutate(actual = actual / weight, pred = pred / weight, AvE = actual / pred)
  return(lift_df)
}

# Double Lift Calculation
KT_calc_dl <- function(actual, weight, base, challenger, nbin) {
  if (length(actual) != length(weight) || length(actual) != length(base) || length(actual) != length(challenger)) {
    stop("Input vectors 'actual', 'weight', 'base', and 'challenger' must have the same length.")
  }
  if (nbin <= 0) stop("Number of bins must be a positive integer.")
  
  df <- data.frame(actual, weight, base, challenger) %>%
    filter(weight > 0) %>%
    mutate(model_ratio = base / challenger) %>%
    arrange(model_ratio) %>%
    mutate(bin = KT_create_equal_bin(weight, nbin)) %>%
    group_by(bin) %>%
    summarise(across(everything(), sum)) %>%
    mutate(actual = actual / weight, base = base / weight, challenger = challenger / weight, AvE = actual / base)
  return(df)
}
################################ AvE ###################################

# Function: Calculate Actual vs Expected (AvE)
KT_calc_ave <- function(ft, actual, pred, challenger, weight, rebase = TRUE) {
  if (missing(ft)) stop("Feature ('ft') is missing.")
  if (missing(actual) || missing(pred) || missing(weight)) stop("One or more required inputs ('actual', 'pred', 'weight') are missing.")
  if (any(is.na(c(ft, actual, pred, weight)))) stop("Inputs contain NA values. Please remove or impute them.")
  
  if (missing(challenger)) challenger <- pred

  if (rebase) {
    pred <- pred * (sum(actual) / sum(pred * weight))
    challenger <- challenger * (sum(actual) / sum(challenger * weight))
  }

  df <- data.frame(ft, actual, pred, challenger, weight)
  df <- df %>%
    mutate(across(c(pred, challenger), ~ .x * weight))  # Apply weights
  
  overall <- df %>%
    summarise(across(c(actual, pred, challenger, weight), sum)) %>%
    mutate(
      actual_overall_avg = actual / weight,
      pred_overall_avg = pred / weight,
      challenger_overall_avg = challenger / weight
    )
  
  result <- df %>%
    group_by(ft) %>%
    summarise(across(c(actual, pred, challenger, weight), sum)) %>%
    mutate(
      actual = actual / weight,
      pred = pred / weight,
      challenger = challenger / weight,
      ave = actual / pred,
      challenger_ave = actual / challenger,
      actual_overall_avg = overall$actual_overall_avg,
      pred_overall_avg = overall$pred_overall_avg
    )
  
  return(result)
}

# Function: Random Fold AvE Consistency
KT_calc_ave_consistency_random_fold <- function(ft, actual, pred, weight, challenger, nfold = 5, plot_scale = 5000) {
  if (missing(ft)) stop("Feature ('ft') is missing.")
  if (nfold <= 0) stop("Number of folds ('nfold') must be a positive integer.")
  
  folds <- KT_create_fold_idx(data.frame(ft), nfold)
  folds[["Full"]] <- unlist(folds) %>% as.vector()
  AvE_df_list <- list()

  for (fold in names(folds)) {
    fold_data <- folds[[fold]]
    AvE_df_list[[fold]] <- KT_calc_ave(ft = ft[fold_data], actual = actual[fold_data], pred = pred[fold_data], weight = weight[fold_data]) %>%
      mutate(sample = fold)
  }

  ave_df <- rbindlist(AvE_df_list) %>%
    mutate(
      sample = factor(sample, levels = KT_dym_sort(unique(sample))),
      bar_group = ifelse(grepl("fold", sample), "fold", "full")
    )
  
  p <- ggplotly(
    ggplot(ave_df, aes(x = ft, group = sample, fill = bar_group)) +
      geom_hline(yintercept = 1, color = "#39ff14") +
      geom_line(aes(y = ave, color = sample)) +
      geom_bar(aes(y = weight / plot_scale), stat = "identity", alpha = 0.4, position = "dodge") +
      scale_fill_manual(values = c("fold" = "grey", "full" = "orange")) +
      scale_y_continuous(name = "Actual/Expected", sec.axis = sec_axis(~ . * plot_scale, name = "weight")) +
      theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
      ggtitle("AvE Consistency Across Random Folds")
  )
  
  return(list(ave_df = ave_df, ave_plot = p))
}

# Function: Resample AvE
KT_resample_ave <- function(n, ft, actual, pred, challenger, weight) {
  if (n <= 0) stop("Number of resamples ('n') must be a positive integer.")
  if (missing(challenger)) challenger <- pred

  ave_sim <- list()
  df <- data.frame(ft, actual, pred, challenger, weight)
  
  main_ave <- KT_calc_ave(ft = df$ft, actual = df$actual, pred = df$pred, challenger = df$challenger, weight = df$weight)
  main_ave$sample <- "main"
  ave_sim[["iter_0"]] <- main_ave

  for (x in seq_len(n)) {
    set.seed(x)
    sampled_df <- df %>% sample_frac(size = 0.3, replace = FALSE)
    ave_sim[[glue("iter_{x}")]] <- KT_calc_ave(sampled_df$ft, sampled_df$actual, sampled_df$pred, sampled_df$challenger, sampled_df$weight) %>%
      mutate(sample = x)
  }

  variables <- list()
  for (var in c("actual", "pred", "ave", "challenger", "challenger_ave")) {
    variables[[var]] <- rbindlist(ave_sim) %>%
      select(ft, !!as.name(var), sample) %>%
      pivot_wider(names_from = sample, values_from = !!as.name(var)) %>%
      rowwise() %>%
      mutate(lb = quantile(c_across(2:(n + 1)), 0.05, na.rm = TRUE),
             ub = quantile(c_across(2:(n + 1)), 0.95, na.rm = TRUE)) %>%
      select(ft, main, lb, ub) %>%
      mutate(variable = var)
  }

  ave_df <- data.frame(rbindlist(variables), weight = main_ave$weight)
  return(list(ave_df = ave_df, main_ave = main_ave))
}

KT_plot_ave <- function(n, ft, actual, pred, challenger, weight, factor_name, title, rescale = 30) {
  # Validate inputs
  if (missing(ft) || missing(actual) || missing(pred) || missing(weight)) {
    stop("Required inputs ('ft', 'actual', 'pred', 'weight') are missing.")
  }
  if (length(ft) != length(actual) || length(actual) != length(pred) || length(pred) != length(weight)) {
    stop("All input vectors ('ft', 'actual', 'pred', 'weight') must have the same length.")
  }
  if (n <= 0) stop("'n' must be a positive integer.")
  
  if (missing(challenger)) {
    challenger <- pred
  }
  
  test <- KT_resample_ave(
    n = n,
    ft = ft,
    actual = actual,
    pred = pred,
    challenger = challenger,
    weight = weight
  )
  
  line_size <- 1.2
  point_size <- 2.3
  rescale2 <- rescale * 1000

  # Plot 1: Actual vs Predicted
  p1 <- test$ave_df %>%
    filter(grepl("actual|pred", variable)) %>%
    mutate(weight = ifelse(variable == "actual", weight, 0)) %>%
    ggplot(aes(x = ft, group = variable, colour = variable, fill = variable)) +
    geom_point(aes(y = main), size = point_size) +
    geom_line(aes(y = main), size = line_size) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.15, color = NA) +
    scale_colour_manual("", values = c("red", "blue")) +
    scale_fill_manual("", values = c("red", "blue")) +
    xlab(factor_name) +
    ggtitle(title) +
    theme_bw() +
    theme(panel.background = element_blank(), axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    theme(legend.position = "bottom") +
    geom_bar(aes(y = weight / rescale), stat = "identity", size = 0.1, color = "black", alpha = 0.4) +
    scale_y_continuous(name = "", sec.axis = sec_axis(~ . * rescale, name = "weight"))

  # Plot 2: Compare actual, challenger, and predicted
  p2 <- test$ave_df %>%
    filter(variable %in% c("actual", "challenger", "pred")) %>%
    mutate(weight = ifelse(variable == "actual", weight, 0)) %>%
    ggplot(aes(x = ft, group = variable, colour = variable, fill = variable)) +
    geom_point(aes(y = main), size = point_size) +
    geom_line(aes(y = main), size = line_size) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.15, color = NA) +
    scale_colour_manual("", values = c("red", "green", "blue")) +
    scale_fill_manual("", values = c("red", "green", "blue")) +
    xlab(factor_name) +
    ggtitle(title) +
    theme_bw() +
    theme(panel.background = element_blank(), axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    theme(legend.position = "bottom") +
    geom_bar(aes(y = weight / rescale), stat = "identity", size = 0.1, color = "black", alpha = 0.4) +
    scale_y_continuous(name = "", sec.axis = sec_axis(~ . * rescale, name = "weight"))

  # Plot 3: AvE ribbon plot
  p3 <- test$ave_df %>%
    filter(grepl("ave", variable)) %>%
    mutate(weight = ifelse(variable == "ave", weight, 0)) %>%
    ggplot(aes(x = ft, group = variable, colour = variable, fill = variable)) +
    geom_point(aes(y = main), size = point_size) +
    geom_line(aes(y = main), size = line_size) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.15, color = NA) +
    scale_colour_manual("", values = c("red", "blue")) +
    scale_fill_manual("", values = c("red", "blue")) +
    geom_hline(yintercept = 1, linetype = 2) +
    xlab(factor_name) +
    ggtitle(title) +
    theme_bw() +
    theme(panel.background = element_blank(), axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    theme(legend.position = "bottom") +
    geom_bar(aes(y = weight / rescale2), stat = "identity", size = 0.1, color = "black", alpha = 0.4) +
    scale_y_continuous(name = "actual/expected", sec.axis = sec_axis(~ . * rescale2, name = "weight"))

  return(list(
    compare_plot = list(compare_ave_plot = p2, compare_ave_plot_rb = p3),
    model_plot = list(ave_plot = p1),
    ave_df = test$ave_df
  ))
}

################################ Explain Model ####################################

KT_plot_shap <- function(sv, ft, ft_name, excl, loess_strength) {
  if (missing(sv) || missing(ft)) stop("Inputs 'sv' and 'ft' are required.")
  
  df <- data.frame(sv, ft)
  if (!missing(excl)) {
    df <- df %>% filter(!ft %in% excl)
  }
  
  p <- df %>%
    arrange(ft) %>%
    ggplot(aes(x = ft, y = sv)) +
    geom_point(alpha = 0.3, size = 2, colour = "blue", fill = "blue", stroke = NA) +
    theme_bw() +
    theme(panel.background = element_blank(), axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    xlab(ft_name) +
    ylab("SHAP Values") +
    ggtitle(glue("{ft_name} SHAP Trend"))
  
  if (!missing(loess_strength)) {
    p <- p + geom_smooth(aes(y = sv), span = loess_strength, method = "loess", se = FALSE)
  }
  
  return(p)
}
######################################## SHAP Interaction Plot ########################################

KT_plot_shap_w_interaction <- function(sv, ft, ft_name, excl, interaction, loess_strength) {
  # Validate inputs
  if (missing(sv) || missing(ft)) stop("Inputs 'sv' and 'ft' are required.")
  if (!missing(excl) && !all(excl %in% ft)) warning("Some 'excl' values are not in 'ft'.")

  df <- if (!missing(excl)) {
    data.frame(sv, ft) %>% filter(!ft %in% excl)
  } else {
    data.frame(sv, ft, interaction)
  }
  
  p <- df %>%
    group_by(interaction) %>%
    arrange(ft) %>%
    ggplot(aes(x = ft, y = sv, colour = interaction, group = interaction)) +
    geom_point(alpha = 0.3, size = 1.5, stroke = NA) +
    scale_color_viridis_c() +
    theme_bw() +
    theme(panel.background = element_blank(), axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    xlab(ft_name) +
    ylab("SHAP Values")
  
  if (!missing(loess_strength)) {
    p <- p + geom_smooth(aes(y = sv), span = loess_strength, method = "loess", se = FALSE)
  }
  
  return(p)
}


KT_plot_compare_shap <- function(sv_base, sv_challenger, base_ft, challenger_ft, ft_name, loess_strength) {
  # Validate inputs
  if (missing(sv_base) || missing(sv_challenger) || missing(base_ft) || missing(challenger_ft)) {
    stop("All inputs 'sv_base', 'sv_challenger', 'base_ft', and 'challenger_ft' are required.")
  }

  df_base <- data.frame(sv = sv_base, ft = base_ft, scenario = "base")
  df_challenger <- data.frame(sv = sv_challenger, ft = challenger_ft, scenario = "challenger")
  df <- rbind(df_base, df_challenger)
  
  p <- df %>%
    group_by(scenario) %>%
    arrange(ft) %>%
    ggplot(aes(x = ft, y = sv, group = scenario, colour = scenario)) +
    geom_point(alpha = 0.1, size = 1.5, stroke = NA, shape = 21) +
    scale_colour_manual(values = c("blue", "red")) +
    theme_bw() +
    theme(panel.background = element_blank(), axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    xlab(ft_name) +
    ylab("SHAP Values") +
    ggtitle(glue("{ft_name} SHAP Trend"))
  
  if (!missing(loess_strength)) {
    p <- p + geom_smooth(aes(y = sv), span = loess_strength, method = "loess", se = FALSE)
  }

  return(p)
}

######################################## Prepare UK Lookup Map ########################################

KT_prepare_uk_lookup_map <- function() {
  postcode_lookup_geometry <- list()
  postcode_lookup_poly <- list()
  postcode_regex <- list()
  postcode_lookup_shp <- list()

  lvl <- list(
    "area" = "([A-Z][A-Z]{0,1})",
    "district" = "(([A-Z][A-Z]{0,1})[0-9][A-Z0-9]{0,1})",
    "sector" = "((([A-Z][A-Z]{0,1})[0-9][A-Z0-9]{0,1}) {0,}[0-9])",
    "postcode" = "^(((([A-Z][A-Z]{0,1})[0-9][A-Z0-9]{0,1}) {0,}[0-9])[A-Z]{2})$"
  )
  
  postcode <- tryCatch({
    data.table::fread("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/ukpostcodes.csv")
  }, error = function(e) stop("Failed to read UK postcode file. Please check the file path."))

  for (level in c("area", "district", "sector")) {
    lookup_poly <- postcode %>%
      filter(!is.na(longitude)) %>%
      mutate(name = stringr::str_extract(postcode, lvl[[level]])) %>%
      select(name, longitude, latitude) %>%
      group_by(name) %>%
      summarise(across(c(longitude, latitude), mean))
    
    lookup_geo <- lookup_poly %>%
      filter(latitude < 90) %>%
      st_as_sf(coords = c("longitude", "latitude"))
    
    postcode_lookup_poly[[level]] <- lookup_poly
    postcode_lookup_geometry[[level]] <- lookup_geo
    postcode_regex[[level]] <- lvl[[level]]
    postcode_lookup_shp[[level]] <- tryCatch({
      read_sf(glue("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/Distribution/{level}s.shp"))
    }, error = function(e) warning(glue("Failed to read shapefile for level '{level}'. Please check the file path.")))
  }

  postcode_lookup_poly[["postcode"]] <- postcode %>% select(-id) %>% rename(name = postcode)
  
  return(list(
    postcode_lookup_lonlat = postcode_lookup_poly,
    postcode_lookup_point = postcode_lookup_geometry,
    postcode_lookup_shp = postcode_lookup_shp,
    postcode_regex = postcode_regex
  ))
}

######################################## Plot UK Map ########################################

KT_plot_uk_map <- function(df, value, title, size, group = NA, alpha = 0.5, nrow = 1) {
  if (missing(df) || missing(value) || missing(title)) {
    stop("Inputs 'df', 'value', and 'title' are required.")
  }
  
  point_plot <- df %>%
    ggplot() +
    geom_point(aes(color = !!as.name(value), geometry = geometry, size = size), 
               alpha = alpha, stat = "sf_coordinates", stroke = 0.15, show.legend = TRUE) +
    scale_color_viridis_c() +
    theme(panel.background = element_blank(), axis.text = element_text(colour = "white")) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(title)
  
  shape_plot <- df %>%
    ggplot() +
    geom_sf(aes(fill = !!as.name(value), geometry = geometry), colour = NA) +
    scale_fill_viridis_c() +
    theme(panel.background = element_blank(), axis.text = element_text(colour = "white")) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(title)
  
  return(list(point_plot = point_plot, shape_plot = shape_plot))
}
################################## XGB Modelling Pipeline #####################################################

# Train XGBoost Model
KT_xgb_train <- function(train, train_y, train_weight, validate, validate_y, validate_weight,
                         params, verbose = 1, nthread = max(floor(parallel::detectCores() * 2 / 3), 1),
                         early_stopping_rounds = NULL) {
  # Validate inputs
  if (missing(train) || missing(train_y) || missing(train_weight)) stop("Training data, labels, and weights are required.")
  if (nrow(train) != length(train_y) || nrow(train) != length(train_weight)) stop("Mismatch in row count between 'train', 'train_y', and 'train_weight'.")
  if (!missing(validate) && (nrow(validate) != length(validate_y) || nrow(validate) != length(validate_weight))) {
    stop("Mismatch in row count between 'validate', 'validate_y', and 'validate_weight'.")
  }

  # Prepare DMatrix for training
  train_mat <- xgb.DMatrix(data = as.matrix(train), label = train_y, weight = train_weight)

  if (missing(validate)) {
    watchlist <- list(train = train_mat)
    early_stopping_rounds <- NULL
  } else {
    validate_mat <- xgb.DMatrix(data = as.matrix(validate), label = validate_y, weight = validate_weight)
    watchlist <- list(train = train_mat, validate = validate_mat)
  }

  # Train the model
  model <- xgb.train(
    params = params,
    data = train_mat,
    nrounds = params$nrounds,
    watchlist = watchlist,
    print_every_n = 5,
    early_stopping_rounds = early_stopping_rounds,
    maximize = FALSE,
    verbose = verbose,
    nthread = nthread
  )

  # Process evaluation log
  eval_log <- as.data.frame(model$evaluation_log)
  if (missing(validate)) {
    names(eval_log) <- c("iter", "train_loss")
    eval_log$test_loss <- eval_log$train_loss
  } else {
    names(eval_log) <- c("iter", "train_loss", "test_loss")
  }

  # Generate loss plot
  loss_plot <- eval_log %>%
    pivot_longer(-iter, names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = iter, y = value, group = variable, color = variable)) +
    geom_line(size = 1.2, alpha = 0.8) +
    theme_minimal() +
    labs(title = "Training Loss", x = "Iteration", y = "Loss")

  # Generate feature importance plot
  importance_df <- xgb.importance(model = model)
  imp_plot <- ggplot(importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Feature Importance by Gain", x = "Feature", y = "Gain")

  return(list(model = model, loss_plot = loss_plot, imp_plot = imp_plot))
}

# Create K-Fold Indexes
KT_create_fold_idx <- function(df, k) {
  if (missing(df) || missing(k)) stop("Data frame and number of folds are required.")
  if (k <= 0) stop("'k' must be a positive integer.")

  folds <- lapply(1:k, function(i) as.integer(seq(i, nrow(df), by = k)))
  names(folds) <- paste0("fold", 1:k)
  return(folds)
}

# Cross-Validation for XGBoost
KT_xgb_cv <- function(train, train_y, train_weight, folds, params, verbose = 0, nthread = max(floor(parallel::detectCores() * 2 / 3), 1)) {
  # Validate inputs
  if (missing(train) || missing(train_y) || missing(train_weight) || missing(folds)) stop("Training data, labels, weights, and folds are required.")
  if (nrow(train) != length(train_y) || nrow(train) != length(train_weight)) stop("Mismatch in row count between 'train', 'train_y', and 'train_weight'.")

  # Prepare DMatrix for cross-validation
  train_mat <- xgb.DMatrix(data = as.matrix(train), label = train_y, weight = train_weight)

  # Perform cross-validation
  cv_model <- xgb.cv(
    params = params,
    data = train_mat,
    nrounds = params$nrounds,
    folds = folds,
    print_every_n = 5,
    early_stopping_rounds = 10,
    maximize = FALSE,
    verbose = verbose,
    nthread = nthread
  )

  # Process evaluation log
  eval_log <- as.data.frame(cv_model$evaluation_log)
  names(eval_log) <- c("iter", "train_loss", "train_std", "test_loss", "test_std")

  # Generate loss plot
  loss_plot <- eval_log %>%
    select(iter, train_loss, test_loss) %>%
    pivot_longer(-iter, names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = iter, y = value, group = variable, color = variable)) +
    geom_line(size = 1.2, alpha = 0.8) +
    theme_minimal() +
    labs(title = "Cross-Validation Loss", x = "Iteration", y = "Loss")

  return(list(cv_model = cv_model, loss_plot = loss_plot))
}


KT_xgb_bayesian_tune <- function(train, train_y, train_weight, validate = NULL, validate_y = NULL, 
                                  validate_weight = NULL, folds = NULL, bounds, HP_fixed = list(),
                                  nrounds = 400, monotone_constraints = NULL, 
                                  interaction_constraints = NULL, objective = "reg:tweedie",
                                  eval_metric = "tweedie-nloglik@1.5", parallel = FALSE,
                                  iters.k = 1, iters.n = 4, ncluster = max(floor(detectCores() * 2 / 3), 1),
                                  initPoints = 10, verbose = 1) {
  # Validate inputs
  if (missing(train) || missing(train_y) || missing(train_weight) || missing(bounds)) {
    stop("Training data, labels, weights, and parameter bounds are required.")
  }
  if (nrow(train) != length(train_y) || nrow(train) != length(train_weight)) {
    stop("Mismatch in row count between 'train', 'train_y', and 'train_weight'.")
  }
  if (!is.null(validate) && (nrow(validate) != length(validate_y) || nrow(validate) != length(validate_weight))) {
    stop("Mismatch in row count between 'validate', 'validate_y', and 'validate_weight'.")
  }

  # Garbage collection
  gc()

  cv <- !is.null(folds)
  cluster_obj <- if (cv) {
    c("train", "train_y", "train_weight", "folds", "bounds", "nrounds", "objective", "eval_metric", 
      "monotone_constraints", "interaction_constraints")
  } else {
    c("train", "train_y", "train_weight", "validate", "validate_y", "validate_weight", "bounds", 
      "nrounds", "objective", "eval_metric", "monotone_constraints", "interaction_constraints")
  }

  if (parallel) {
    library(doParallel)
    cl <- makeCluster(ncluster)
    registerDoParallel(cl)
    clusterExport(cl, cluster_obj, envir = environment())
    clusterEvalQ(cl, expr = {
      library(xgboost)
      library(tidyverse)
    })
  } else {
    iters.k <- 1
  }

  # Objective function for Bayesian optimization
  obj_fun <- function(eta, ...) {
    params <- c(list(eta = eta, objective = objective, eval_metric = eval_metric, 
                     monotone_constraints = monotone_constraints, 
                     interaction_constraints = interaction_constraints, nrounds = nrounds), 
                HP_fixed, list(...))

    if (cv) {
      model <- KT_xgb_cv(train = train, train_y = train_y, train_weight = train_weight, 
                         folds = folds, params = params)$cv_model
    } else {
      model <- KT_xgb_train(train = train, train_y = train_y, train_weight = train_weight, 
                            validate = validate, validate_y = validate_y, 
                            validate_weight = validate_weight, params = params, 
                            early_stopping_rounds = 5)$model
    }

    best_iteration <- which.min(model$evaluation_log$test_loss)
    validate_loss <- model$evaluation_log[best_iteration, "test_loss"]
    validate_iter <- paste(model$evaluation_log$test_loss, collapse = ",")

    return(list(Score = as.numeric(validate_loss), num_rounds = best_iteration, validate_iter = validate_iter))
  }

  # Run Bayesian optimization
  opt_results <- bayesOpt::bayesOpt(
    FUN = obj_fun,
    bounds = bounds,
    initPoints = initPoints,
    iters.n = iters.n,
    iters.k = iters.k,
    parallel = parallel,
    verbose = verbose
  )

  if (parallel) {
    stopCluster(cl)
    registerDoSEQ()
  }

  # Generate iteration plot
  tune_iteration <- data.frame()
  for (x in seq_len(nrow(opt_results$scoreSummary))) {
    validate_loss <- as.numeric(strsplit(opt_results$scoreSummary$validate_iter[x], ",")[[1]])
    train_iteration <- seq_along(validate_loss)
    BayOpt_iteration <- as.factor(rep(opt_results$scoreSummary$Iteration[x], length(validate_loss)))

    tune_iteration <- rbind(tune_iteration, data.frame(BayOpt_iteration, train_iteration, validate_loss))
  }

  tune_iteration_plot <- tune_iteration %>%
    ggplot(aes(x = train_iteration, y = validate_loss, color = BayOpt_iteration, group = BayOpt_iteration)) +
    geom_line(size = 1.5) +
    theme_minimal() +
    labs(title = "Bayesian Optimization Iterations", x = "Training Iteration", y = "Validation Loss")

  # Generate hyperparameter trends
  hyperparameters <- list(tune_iteration = tune_iteration_plot)
  HP <- intersect(names(bounds), colnames(opt_results$scoreSummary))
  for (x in HP) {
    hyperparameters[[x]] <- opt_results$scoreSummary %>%
      ggplot(aes_string(x = x, y = "Score", color = "Iteration")) +
      geom_point(size = 2.5) +
      theme_minimal() +
      labs(title = paste("Hyperparameter Trend:", x), x = x, y = "Score")
  }

  # Prepare best parameters
  opt_results$scoreSummary <- opt_results$scoreSummary %>% rename(nrounds = num_rounds)
  best_params <- opt_results$scoreSummary %>% arrange(Score) %>% slice(1) %>% as.list()
  best_params <- c(best_params, list(monotone_constraints = monotone_constraints, 
                                     interaction_constraints = interaction_constraints), 
                   HP_fixed)
  best_params$objective <- objective
  best_params$eval_metric <- eval_metric

  return(list(opt_results = opt_results$scoreSummary, hyperparameters_trends = hyperparameters, 
              best_params = best_params))
}



KT_xgb_explain <- function(model, pred_data, sample_size = 6000) {
  # Validate inputs
  if (missing(model) || missing(pred_data)) stop("Model and prediction data are required.")

  pred_data <- as.matrix(pred_data)

  print("Running importance metrics...")
  interaction_gain <- interactions(xgb_model = model, data = pred_data, option = "interactions")
  EIXimportance <- importance(model, pred_data, option = "variables")
  EIXimportanceX <- importance(model, pred_data, option = "interactions")

  print("Calculating SHAP values...")
  set.seed(33)
  pred_data_main_effect <- pred_data[sample(nrow(pred_data), min(nrow(pred_data), sample_size), replace = FALSE), ]
  shap_main_effect <- predict(model, newdata = pred_data_main_effect, predcontrib = TRUE) %>% as.data.frame()

  shap_main_effect <- shap_main_effect %>% mutate_all(~ KT_quantile_clip(.x, min = 0.001, max = 0.999))

  # Interaction SHAP values
  pred_data_interaction <- pred_data[sample(nrow(pred_data), min(nrow(pred_data), sample_size), replace = FALSE), ]
  shap_interaction <- predict(model, newdata = pred_data_interaction, predinteraction = TRUE) %>% as.data.frame()

  # Feature importance from SHAP
  ft_importance <- abs(shap_main_effect) %>%
    select(-BIAS) %>%
    summarise_all(sum) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    mutate(pc_contri = value / sum(value)) %>%
    arrange(desc(pc_contri))

  ft_importance_plot <- ft_importance %>%
    filter(pc_contri > 0.0001) %>%
    ggplot(aes(x = reorder(variable, pc_contri), y = pc_contri)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = "SHAP Feature Contribution", x = "Feature", y = "Contribution (%)")

  return(list(main_effect = list(pred_data_main_effect = pred_data_main_effect, shap_main_effect = shap_main_effect), 
              interaction = list(pred_data_interaction = pred_data_interaction, shap_interaction = shap_interaction), 
              ft_importance = ft_importance, ft_importance_plot = ft_importance_plot, 
              EIXimportance = EIXimportance, EIXimportanceX = EIXimportanceX, 
              EIXimportance_matrix = interaction_gain))
}
#################### Boruta feature selection #####################################

# library(Boruta)
# 
# xgb.boruta=Boruta(train,
#                   y=train_y[[1]],
#                   maxRuns=12, 
#                   doTrace=2,
#                   holdHistory=TRUE,
#                   getImp=getImpXgboost,
#                   max.depth=model$params$max_depth, 
#                   eta=model$params$eta, 
#                   nthread=4, 
#                   min_child_weight=model$params$min_child_weight,
#                   eval_metric=model$params$eval_metric, 
#                   nrounds=model$params$nrounds, 
#                   objective = model$params$objective,
#                   tree_method="hist",
#                   subsample = model$params$subsample,
#                   colsample_bytree = model$params$colsample_bytree,
#                   alpha = model$params$alpha
#                   
#                   
# )
# 
# 
# boruta_dec=attStats(xgb.boruta)
# 
# #get the names of each feature
# imp_features=row.names(boruta_dec)[which(boruta_dec$decision!="Rejected")]
# #get feature importance history
# boruta.imp.df=as.data.frame(xgb.boruta$ImpHistory)
# #keep only confirmed and tentative features
# boruta.imp.df=boruta.imp.df[,names(boruta.imp.df)%in%imp_features]
# #transform the data to a data frame with two columns: feature and importance value
# boruta.imp.df=melt(boruta.imp.df)
# #create a data frame by adding the decision for each feature as well
# boruta.imp.df=cbind.data.frame(boruta.imp.df, 
#                                decision=boruta_dec$decision[match(boruta.imp.df$variable, 
#                                                                   row.names(boruta_dec))])
# #reorder features data frame by the importance median value
# feature_order=with(boruta.imp.df, reorder(variable, value, median, order = TRUE))
# boruta.imp.df$variable=factor(boruta.imp.df$variable, levels = levels(feature_order))
# 
# boruta.imp.df %>% ggplot(.,aes(y = variable ,  x = value , fill =  decision )) + geom_boxplot() + theme(legend.background = "bottom") +theme_gray(base_size = 25)

