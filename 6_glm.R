
create_pairs <- function(vector) {
  pairs <- sapply(1:(length(vector) - 1), function(i) {
    paste0("(", vector[i], ",", vector[i + 1], "]")
  })
  return(pairs)
}

normalize_feature <- function(feature, min_val, max_val) {
  ifelse(feature < min_val, 0, ifelse(feature > max_val, 1, (feature - min_val) / (max_val - min_val)))
}


# Function to create splines
create_splines <- function(df, splines_dt) {
  tryCatch({
    # Garbage collection to free up memory
    gc()
    
    # Ensure splines_dt is distinct
    splines_dt <- splines_dt %>% distinct()
    overlay_fts_dt <- data.table(idx = 1:nrow(df))
    
    # Loop through each row in splines_dt to create splines
    for (i in 1:nrow(splines_dt)) {
      feature <- splines_dt[["feature"]][i]
      spline <- splines_dt[["id"]][i]
      min_val <- as.numeric(splines_dt[["x0_lvl_name"]][i])
      max_val <- as.numeric(splines_dt[["x1_lvl_name"]][i])
      
      # Normalize the feature and add it to overlay_fts_dt
      overlay_fts_dt[[spline]] <- normalize_feature(feature = df[[feature]], min_val = min_val, max_val = max_val)
    }
    
    # Return the overlay features data table without the index column
    return(overlay_fts_dt %>% select(-idx))
  }, error = function(e) {
    message("Error in create_splines: ", e$message)
    return(NULL)
  })
}

# Function to fit GLM model
glm_fit <- function(glm_train, splines_dt, response, base, weight, fam, pmml_max_band = 2000) {
  tryCatch({
    
    # Ensure splines_dt is distinct
    splines_dt <- splines_dt %>% distinct()
    
    # Create splines for the features
    overlay_fts_dt <- create_splines(df = glm_train %>% mutate_all(~ifelse(is.na(.), KT_calculate_mode(.), .)), splines_dt = splines_dt)
    
    # Create model matrix
    x <- model.matrix(~., data = overlay_fts_dt)
    base_ave <- response / base
    
    # Fit the GLM model
    t0 <- Sys.time()
    suppressWarnings({
      adj_fit <- fastglm(x = x, y = base_ave, weights = base * weight, family = fam)
    })
    print(glue("glm fit total run time {Sys.time() - t0}"))
    # Predict adjusted values
    adj <- predict(adj_fit, newdata = x, type = "response")
    indiv_eff <- list()
    
    # Calculate individual effects
    for (ft in splines_dt$feature %>% unique()) {
      excl_terms <- names(adj_fit$coefficients)[!grepl(glue("^{ft}"), names(adj_fit$coefficients))]
      temp_model <- adj_fit
      temp_model$coefficients[excl_terms[excl_terms != "(Intercept)"]] <- 0
      indiv_eff[[ft]] <- predict(temp_model, newdata = x, type = "response")
    }
    
    # Extract coefficients
    coefficients <- coef(adj_fit)
    
    # Create data table for results
    result <- data.table(
      id = gsub("`", "", names(coefficients)),
      estimate = coefficients
    )
    
    # Create LP models
    fitted_fts <- splines_dt$feature %>% unique
    LP_models <- lapply(fitted_fts, function(ft) {
      temp <- adj_fit
      temp$coefficients[['(Intercept)']] <- 0
      effect_to_remove <- setdiff(fitted_fts, ft)
      coef_name <- names(temp$coefficients)
      temp$coefficients[coef_name[grepl(paste0("^", effect_to_remove, collapse = "|"), coef_name)]] <- 0
      temp
    }) %>% setNames(., fitted_fts)
    
    # Create lookup tables and band logic
    imp_values <- lapply(glm_train, function(x) KT_calculate_mode(x)) %>% setNames(., names(glm_train))
    feature_range <- lapply(glm_train, function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))) %>% setNames(., names(glm_train))
    lookup_tables_list <- list()
    band_logic_for_rdr_list <- list()
    result %>% left_join(splines_dt, by = "id") -> model_summary
    
    for (x in unique(model_summary$feature[!is.na(model_summary$feature)])) {
      model_summary %>% filter(feature == x) -> temp
      lapply(temp$id, function(y) 
        normalize_feature(imp_values[[x]],
                          min_val = temp[temp$id == y][["x0_lvl_name"]],
                          max_val = temp[temp$id == y][["x1_lvl_name"]]) * temp[temp$id == y][["estimate"]]) %>% 
        Reduce("+", .) -> imp_rel
      
      band_req <- if (unique(temp$dtype) == "integer" & sort(feature_range[[x]])[1] == 0 & sort(feature_range[[x]])[2] == 1) {
        FALSE
      } else {
        TRUE
      }
      
      if (band_req) {
        model_summary %>%
          filter(feature == x) %>%
          select(x0_lvl_name, x1_lvl_name, estimate) %>%
          rename(x0 = x0_lvl_name, x1 = x1_lvl_name) %>%
          arrange(x0) %>%
          mutate(range = x1 - x0,
                 prop = range / sum(range),
                 band_dist = round(pmml_max_band * prop)) %>%
          mutate(gap = x1 - lead(x0, 1),
                 gap = ifelse(is.na(gap), 0, gap),
                 x0lag1 = lead(x0, 1)) %>%
          rowwise() %>%
          mutate(
            spline = ifelse(gap == 0, 
                            list(seq(x0, x1, length.out = band_dist) %>% round(3) %>% unique), 
                            list(c(seq(x0, x1, length.out = band_dist), seq(x1, x0lag1, length.out = 2)) %>% round(3) %>% unique)),
            spline_norm = list(normalize_feature(spline, min_val = x0, max_val = x1)),
            rel = list(as.vector(spline_norm)[as.vector(spline_norm) > 0] * estimate),
            last_rel = (rel[[length(rel)]]),
            band = list(create_pairs(spline %>% as.vector()))
          ) %>% ungroup() %>%
          mutate(band = lapply(seq_along(band), function(x) {
            if (length(band) == 1) {
              c(glue("<={spline[[x]][1]}"), band[[x]], glue(">{tail(spline[[x]], 1)}"), "default")
            } else if (x == 1) {
              c(glue("<={spline[[x]][1]}"), band[[x]])
            } else if (x == length(band)) {
              c(band[[x]], glue(">{tail(spline[[x]], 1)}"), "default")
            } else {
              band[[x]]
            }
          }),
          last_rel = cumsum(last_rel),
          rel = lapply(1:length(rel), function(x) if (x == 1) { rel[[x]] } else { rel[[x]] + last_rel[x - 1] }),
          rel = lapply(seq_along(rel), function(x) {
            if (length(rel) == 1) {
              c(0, rel[[x]], tail(rel[[x]], 1), imp_rel)
            } else if (x == 1) {
              c(0, rel[[x]])
            } else if (x == length(rel)) {
              c(rel[[x]], tail(rel[[x]], 1), imp_rel)
            } else {
              rel[[x]]
            }
          })
          ) -> rel_data
        
        lapply(1:nrow(rel_data), function(x) data.table(band = rel_data[x,]$band %>% unlist(), relativity = rel_data[x,]$rel %>% unlist())) %>% 
          rbindlist(.) %>% group_by(band) %>% 
          summarise(relativity = mean(relativity)) %>% ungroup() %>%
          mutate(band = factor(band, levels = KT_dym_sort(band)), relativity = (relativity)) %>%
          arrange(band) %>%
          rename({{x}} := band) -> lookup_table
        
        interval <- lookup_table[[x]] %>% as.character
        interval[2:(length(interval) - 2)] -> interval
        
        sub("\\(([^,]+),.*", "\\1", interval) -> lb
        sub(".*,([^]]+)\\]", "\\1", interval) -> ub
        
        band_logic_for_rdr <- data.frame(
          LO = c("<=", rep(">", length(lb)), ">", "Default"),
          LB = c(lb[1], lb, ub[length(ub)], ""),
          UO = c("", rep("<=", length(ub)), "", ""),
          UB = c("", ub, "", ""),
          ln = c(lookup_table[[x]] %>% as.character)
        )
        
      } else {
        lapply(temp$id, function(y) 
          normalize_feature(c(0, 1),
                            min_val = temp[temp$id == y][["x0_lvl_name"]],
                            max_val = temp[temp$id == y][["x1_lvl_name"]]) * temp[temp$id == y][["estimate"]]) %>% 
          Reduce("+", .) -> relativity 
        lookup_table <- data.table()
        feature_range[[x]] %>% sort %>% as.character -> ft
        lookup_table[, (x) := factor(c(ft, "default"))]
        lookup_table$relativity <- c(relativity, imp_rel)
        band_logic_for_rdr <- data.frame(
          LO = c("=", "=", "Default"),
          LB = c("0", "1", ""),
          UO = c("", "", ""),
          UB = c("", "", ""),
          ln = c("0", "1", "default")
        )
      }
      
      lookup_tables_list[[x]] <- lookup_table
      band_logic_for_rdr_list[[x]] <- band_logic_for_rdr
    }
    
    intercept <- model_summary[model_summary$id == "(Intercept)"]$estimate
    lookup_tables_list$intercept <- data.table(intercept = 1, relativity = intercept)
    

    return(list(
      adj = as.numeric(adj),
      model = adj_fit,
      fit = result,
      indiv_eff = indiv_eff,
      imp_values = imp_values,
      feature_range = feature_range,
      lookup_tables = lookup_tables_list,
      band_logic_for_rdr = band_logic_for_rdr_list,
      LP_models = LP_models
    ))
gc()
  }, error = function(e) {
    message("Error in glm_fit: ", e$message)
    return(NULL)
  })
}


# Function to plot fit
plot_fit <- function(ft, actual, pred, challenger, weight, ft_name, rebase = TRUE, point_size = 2, lwd = 0.8, 
                     fit_lines = c("CA_challenger", "obs", "CU_challenger", "CM"),
                     band_ft = TRUE, band_method = "equal", nbreaks = 20, indiv_eff) {
  tryCatch({
    # Rebase predictions if required
    if (rebase) {
      pred <- pred * (sum(actual) / sum(pred * weight)) # rebase
      indiv_eff <- indiv_eff * (sum(actual) / sum(indiv_eff * weight))
      challenger <- challenger * (sum(actual) / sum(challenger * weight)) # rebase
    }
    
    # Band the feature if required
    ft <- if (band_ft) {
      KT_band_data(KT_quantile_clip(ft, 0.001, 0.999), nbreaks = nbreaks, method = band_method, weight = weight)
    } else {
      ft
    }
    
    # Create data frame
    df <- data.frame(ft, actual, pred, challenger, weight, indiv_pred = indiv_eff)
    df <- df %>% mutate_at(vars(c("pred", "challenger", "indiv_pred")), ~ .x * weight)
    
    # Calculate overall averages
    overall <- df %>% select(-ft) %>%
      summarise_all(list(sum)) %>%
      mutate(actual_overall_avg = actual / weight,
             pred_overall_avg = pred / weight, 
             indiv_pred_overall_avg = indiv_pred / weight,
             challenger_overall_avg = challenger / weight)
    
    # Group by feature and calculate averages
    df <- df %>% group_by(ft) %>%
      summarise_all(list(sum)) %>%
      mutate(actual = actual / weight,
             pred = pred / weight,
             challenger = challenger / weight,
             indiv_pred = indiv_pred / weight,
             ave = actual / pred,
             challenger_ave = actual / challenger,
             actual_overall_avg = overall$actual_overall_avg,
             pred_overall_avg = overall$pred_overall_avg,
             actual_overall_avg = overall$actual_overall_avg) %>%
      mutate_at(vars(c("pred", "challenger", "actual", "indiv_pred")), ~ .x / actual_overall_avg) %>%
      mutate(weight = weight / sum(weight)) %>%
      select(c("ft", "pred", "challenger", "actual", "weight", "ave", "challenger_ave", "indiv_pred")) %>%
      rename(CA_base = pred,
             CA_challenger = challenger,
             CM = indiv_pred,
             obs = actual, 
             CU_base = ave,
             CU_challenger = challenger_ave) %>%
      mutate(CU_base = CU_base * CM,
             CU_challenger = CU_challenger * CM) %>%
      melt(id.var = c("ft")) %>%
      as.data.table()
    
    # Filter and plot data
    ave_df <- df %>% filter(variable != "weight") %>%
      filter(variable %in% fit_lines) %>%
      ggplot(aes(x = ft, y = value, group = variable, color = variable, shape = variable)) + 
      geom_bar(data = df[df$variable == "weight"],
               aes(y = df[df$variable == "weight"]$value / max(df[df$variable == "weight"]$value)),
               stat = "identity", size = .1, alpha = 1, position = "dodge", color = "yellow", fill = "yellow") +
      theme_light() +
      geom_line(lwd = lwd, alpha = 0.7) +
      geom_point(size = point_size) +
      scale_color_manual(values = c("CA_base" = "darkgreen",
                                    "CA_challenger" = "darkgreen",
                                    "obs" = '#da14ff',
                                    "CU_base" = "orange",
                                    "CU_challenger" = "orange",
                                    "CM" = "green")) +
      scale_shape_manual(values = c("CA_base" = 16,
                                    "CA_challenger" = 3,
                                    "obs" = 16,
                                    "CU_base" = 16,
                                    "CU_challenger" = 3,
                                    "CM" = 1)) +
      xlab(ft_name) +
      theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
      scale_y_continuous(name = "", sec.axis = sec_axis(~ . * max(df[df$variable == "weight"]$value), name = "weight"))
    
    return(ave_df)
  }, error = function(e) {
    message("Error in plot_fit: ", e$message)
    return(NULL)
  })
}



# Function to calculate AvE
calc_ave <- function(ft, actual, pred, weight, challenger, factor_consistency, ft_name, rebase = FALSE, band_ft = TRUE, nbreaks = 20, band_method = "equal") {
  tryCatch({
    # Convert to data.table and perform garbage collection
    gc()
    
    # Band the feature if required
    ft <- if (band_ft) {
      KT_band_data(KT_quantile_clip(ft, 0.001, 0.999), nbreaks = nbreaks, method = band_method, weight = weight)
    } else {
      ft
    }
    
    # Create data table
    df <- data.table(ft = ft,
                     actual = actual,
                     pred = pred,
                     weight = weight,
                     factor_consistency = factor(factor_consistency, levels = unique(factor_consistency)))
    
    # Rebase predictions if required
    if (rebase) {
      rb_factor <- df[, .(weighted_pred = sum(pred * weight), actual = sum(actual)), by = factor_consistency]
      rb_factor[, rb_factor := actual / weighted_pred]
      df <- merge(df, rb_factor[, .(factor_consistency, rb_factor)], by = "factor_consistency")
      df[, pred := pred * rb_factor]
    }
    
    # Calculate weighted predictions
    df[, `:=`(pred = pred * weight)]
    ave_df <- df[, .(pred = sum(pred), actual = sum(actual), weight = sum(weight)), by = .(ft, factor_consistency)]
    ave_df[, `:=`(pred = pred / weight, actual = actual / weight, ave = actual / pred)]
    ave_df <- ave_df[, .(ft, pred, actual, weight, weight_norm = weight / sum(weight), ave, sample = factor_consistency)]
    
    # Plot the results
    scale <- max(ave_df$weight_norm)
    p <- ggplot(ave_df, aes(x = ft, group = sample, fill = sample, color = sample, weight = weight)) +
      theme_light() +
      geom_hline(yintercept = 1, color = '#39ff14', linetype = "dashed") +
      geom_point(aes(y = ave, color = sample), shape = 4) +
      geom_line(aes(y = ave, color = sample)) +
      geom_bar(aes(y = weight_norm / scale), stat = "identity", size = .1, alpha = .4, position = "dodge") +
      scale_y_continuous(name = "Actual/Expected", sec.axis = sec_axis(~ . * scale * exp(scale), name = "weight")) +
      theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
      ylab("Actual/Expected") +
      xlab(ft_name)
    
    return(list(ave_df = ave_df, ave_plot = p, rebase_data = if (rebase) rb_factor else NULL))
    gc()
  }, error = function(e) {
    message("Error in calc_ave: ", e$message)
    return(NULL)
  })
}

# Function to apply cosmetic changes to the plot
cosmetic_changes <- function(p, alpha_pt = 1, alpha_line = 0.5, size_pt = 2, size_line = 1, fit_loess = TRUE, smooth_strength = 0.4, control_yaxis = TRUE, upper_lim = 2, lower_lim = 0) {
  tryCatch({
    # Perform garbage collection
    gc()
    
    # Adjust point and line aesthetics
    p$layers[[2]]$aes_params$size <- size_pt
    p$layers[[2]]$aes_params$alpha <- alpha_pt
    p$layers[[3]]$aes_params$size <- size_line
    p$layers[[3]]$aes_params$alpha <- alpha_line
    
    # Control y-axis limits if required
    if (control_yaxis) {
      p <- p + coord_cartesian(ylim = c(lower_lim, upper_lim))
    }
    
    # Fit loess curve if required
    if (fit_loess) {
      pout <- p + geom_smooth(aes(y = ave), method = "loess", span = smooth_strength, se = FALSE)
      smooth_data <- data.table(x = ggplot_build(pout)$data[[5]][[3]], y = ggplot_build(pout)$data[[5]][[4]])
    } else {
      pout <- p
      smooth_data <- data.table(x = NULL, y = NULL)
    }
    
    # Convert to interactive plot
    pout <- ggplotly(pout) %>% layout(
      legend = list(
        orientation = 'h',
        x = 0.5,
        xanchor = 'center',
        y = -0.2
      )
    )
    
    return(list(ave_plot = pout, smooth_data = smooth_data))
    gc()
  }, error = function(e) {
    message("Error in cosmetic_changes: ", e$message)
    return(NULL)
  })
}


# Function to predict using GLM spline model
glm_spline_predict <- function(model_out, pred_df, type = "response", predict_LP = FALSE) {
  tryCatch({
    # Replace NA values in pred_df with the mode values from the model
    pred_df <- sapply(model_out$glm_model_out$imp_values %>% names, function(x) replace_na(pred_df[[x]], model_out$glm_model_out$imp_values[[x]])) %>% as.data.table()
    
    # Predict using LP models if predict_LP is TRUE
    if (predict_LP) {
      predictions <- lapply(model_out$glm_model_out$LP_models, function(x) {
        predict(x, newdata = model.matrix(~., data = create_splines(df = pred_df, splines_dt = model_out$drawn_shapes %>% rbindlist(.))), type = type)
      }) %>% setNames(., names(model_out$glm_model_out$LP_models))
    } else {
      # Predict using the main model
      predictions <- predict(model_out$glm_model_out$model, newdata = model.matrix(~., data = create_splines(df = pred_df, splines_dt = model_out$drawn_shapes %>% rbindlist(.))), type = type)
    }
    
    return(predictions)
  }, error = function(e) {
    message("Error in glm_spline_predict: ", e$message)
    return(NULL)
  })
}


