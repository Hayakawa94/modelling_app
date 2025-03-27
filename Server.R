

source(glue("{here::here()}/RPMtools/RPMtools.R"))
source(glue("{here::here()}/1_Feature_summary.R"))
source(glue("{here::here()}/2_EDA.R"))
source(glue("{here::here()}/3_Boruta_feature_selection.R"))
source(glue("{here::here()}/4_Tune.R"))
source(glue("{here::here()}/5_Train.R"))
source(glue("{here::here()}/6_glm.R"))
source(glue("{here::here()}/7_performance.R"))
source(glue("{here::here()}/8_model_comparison.R"))
source(glue("{here::here()}/UI.R"))


server <- function(input, output, session) {
  print("processing_data")
  train <- if(length(train %>% select_if(is.factor)%>% names) >0   ){
    cbind(train , one_hot(train %>% select_if(is.factor) , dropUnusedLevels = T))
  }else{
    train
  }
  
  test <- if(length(test %>% select_if(is.factor)%>% names) >0   ){
    cbind(test , one_hot(test %>% select_if(is.factor), dropUnusedLevels = T))
  }else{
    test
  }
  print("processing_completed")
################### Feature Summary ##############################
  
  # Feature specification dataframe
  feature_spec <- data.frame(
    Features = fts,
    dtype = lapply(fts, function(x) class(train[[x]])) %>% as.character(),
    Use_Feature = rep(FALSE, length(fts)),
    Monotonicity = rep(0, length(fts)),
    Interaction_Constraints = rep(FALSE, length(fts))
  ) %>% arrange(Features)
  
  
  # Event reactive configuration
  config <- eventReactive({
    input$ft_table
    input$model
  }, {
    print(glue("configuring {input$model}..."))
    selected_fts <- hot_to_r(input$ft_table)$Features[which(hot_to_r(input$ft_table)$Use_Feature == TRUE)]
    
    dt_sum <- KT_summarise_dataframe(train %>% select(selected_fts))
    dt_sum %>% filter(Feature %in% selected_fts) %>%
      filter(Proportion_Missing >=0.9999) %>% select(Feature) %>% pull  -> missing_features
    selected_fts <- if (length(missing_features)>0){
      print(glue("{missing_features} will be excluded from modelling data due to 100% missing"))
      setdiff(selected_fts, missing_features)
    }else{
      selected_fts
    }
 

    weight <- model_spec[[input$model]]$exposure
    response <- model_spec[[input$model]]$response
    objective <- model_spec[[input$model]]$objective
    eval_metric <- model_spec[[input$model]]$eval_metric
    
    train <- train[train[[weight]] > 0]
    train_y <- train[[response]]
    train_weight <- train[[weight]]
    train$none <- "NA"
    
    test <- test[test[[weight]] > 0]
    test_y <- test[[response]]
    test_weight <- test[[weight]]
    test$none <- "NA"
    
    return(list(
      train = train %>% select(c("none", selected_fts, weight, response)),
      train_y = train_y,
      train_weight = train_weight,
      test = test %>% select(c(selected_fts, weight, response)),
      test_y = test_y,
      test_weight = test_weight,
      selected_fts = selected_fts,
      objective = objective,
      eval_metric = eval_metric,
      weight = weight,
      response = response,
      dt_sum=dt_sum
    ))
  })
  
  # Reactive summarization result
  dt_sum_result <- reactive({
    req(config())
    print('Processing summarise data')
    
    # dt_sum <- KT_summarise_dataframe(config()$train %>% select(config()$selected_fts))
    total_weighted_response <- sum(config()$train_y * config()$train_weight)
    total_exposure <- sum(config()$train_weight)
    weighted_avg_response <- total_weighted_response / total_exposure
    Max_response <- max(config()$train_y)
    Min_response <- min(config()$train_y)
    max_weight <- max(config()$train_weight)
    min_weight <- min(config()$train_weight)
    
    list(
      dt_sum = config()$dt_sum,
      Claim_data = data.table(
        total_weighted_response,
        weighted_avg_response,
        Max_response,
        Min_response,
        max_weight,
        min_weight,
        total_weight = total_exposure,
        Total_Risk = length(config()$train_y)
      ) %>% melt
    )
  })
  
  # Reactive value to store the table data
  table_data <- reactiveVal(feature_spec)
  output$ft_table <- renderRHandsontable({
    rhandsontable(table_data(), useTypes = TRUE) %>%
      hot_col("Use_Feature", type = "checkbox") %>%
      hot_col("Monotonicity", type = "dropdown", source = c(-1, 0, 1)) %>%
      hot_col("Interaction_Constraints", type = "checkbox")
  })
  
  observeEvent(input$reset_ft_selection, {
    table_data(feature_spec)
  })
  
  outputOptions(output, "ft_table", suspendWhenHidden = FALSE)
  
  # Reactive correlation plot
  correlation <- reactive({
    req(dt_sum_result())
    KT_plot_top_n_correlation(config()$train %>% select(config()$selected_fts), n = input$top_corr)
  })
  
  output$corr_topn_slider <- renderUI({
    n_fts <- hot_to_r(input$ft_table) %>% filter(Use_Feature == TRUE) %>% select(Features) %>% pull %>% length()
    sliderInput("top_corr", "Number of Top Correlations:", min = 1, max = n_fts, value = 5, step = 1)
  })
  
  output$corr_plot <- renderPlotly({
    correlation()
  })
  
  
  
  
  # Function to save Feature Spec tab state
  save_feature_spec <- function() {
    final_data <- hot_to_r(input$ft_table)
    file_name <- paste0(input$file_name_feature, ".rds")
    saveRDS(final_data, file_name)  # Save only the feature specification data
    output$action_message_feature <- renderText("Feature Spec state has been saved.")
  }
  
  load_feature_spec <- function() {
    file_name <- paste0(input$file_name_feature, ".rds")
    if (file.exists(file_name)) {
      loaded_data <- readRDS(file_name)
      table_data(loaded_data)  # Load the feature specification data
      output$action_message_feature <- renderText("Feature Spec state has been loaded.")
    } else {
      output$action_message_feature <- renderText("File not found.")
    }
  }
  
  
  
  
  
  output$dt_sum <- DT::renderDataTable({
    datatable( dt_sum_result()$dt_sum ,options =  list(pageLength = 25) ) 
  })
  
  output$Claim <- DT::renderDataTable({
    dt_sum_result()$Claim_data 
  })
  
  
  observeEvent(input$save_feature, {
    save_feature_spec()
  })
  
  observeEvent(input$load_feature, {
    load_feature_spec()
  })
  
  
  ################### EDA ##############################

  
  
  # Observe changes in the feature selection and update slider inputs accordingly
  observe({
    updateSliderInput(session, "ft_nbreaks",
                      max = min(length(unique(config()$train[[input$eda_ft]])), 100),  
                      min = 1, 
                      value = as.integer(min(length(unique(config()$train[[input$eda_ft]])), 100)))
    updateSliderInput(session, "interaction_nbreaks", max = min(length(unique(config()$train[[input$eda_interaction]])), 15))
  })
  
  # Render UI for filter 1 based on its data type
  observe({
    output$filter1_ui <- renderUI({
      if (is.numeric(config()$train[[input$filter1]])) {
        sliderInput("filter1_value", "Filter 1 Value:", 
                    min = min(config()$train[[input$filter1]], na.rm = TRUE), 
                    max = max(config()$train[[input$filter1]], na.rm = TRUE), 
                    value = range(config()$train[[input$filter1]], na.rm = TRUE), 
                    step = if (is.integer(config()$train[[input$filter1]])) 1 else NULL)
      } else {
        checkboxGroupInput("filter1_value", "Filter 1 Value:", 
                           choices = unique(config()$train[[input$filter1]]), 
                           selected = unique(config()$train[[input$filter1]]))
      }
    })
  })
  
  # Render UI for filter 2 based on its data type
  observe({
    output$filter2_ui <- renderUI({
      if (is.numeric(config()$train[[input$filter2]])) {
        sliderInput("filter2_value", "Filter 2 Value:", 
                    min = min(config()$train[[input$filter2]], na.rm = TRUE), 
                    max = max(config()$train[[input$filter2]], na.rm = TRUE), 
                    value = range(config()$train[[input$filter2]], na.rm = TRUE), 
                    step = if (is.integer(config()$train[[input$filter2]])) 1 else NULL)
      } else {
        checkboxGroupInput("filter2_value", "Filter 2 Value:", 
                           choices = unique(config()$train[[input$filter2]]), 
                           selected = unique(config()$train[[input$filter2]]))
      }
    })
  })
  
  # Render UI for filter 3 based on its data type
  observe({
    output$filter3_ui <- renderUI({
      if (is.numeric(config()$train[[input$filter3]])) {
        sliderInput("filter3_value", "Filter 3 Value:", 
                    min = min(config()$train[[input$filter3]], na.rm = TRUE), 
                    max = max(config()$train[[input$filter3]], na.rm = TRUE), 
                    value = range(config()$train[[input$filter3]], na.rm = TRUE), 
                    step = if (is.integer(config()$train[[input$filter3]])) 1 else NULL)
      } else {
        checkboxGroupInput("filter3_value", "Filter 3 Value:", 
                           choices = unique(config()$train[[input$filter3]]), 
                           selected = unique(config()$train[[input$filter3]]))
      }
    })
  })
  
  # Select all values for filter 1
  observeEvent(input$select_all_filter1, {
    if (is.numeric(config()$train[[input$filter1]])) {
      updateSliderInput(session, "filter1_value", value = range(config()$train[[input$filter1]], na.rm = TRUE))
    } else {
      updateCheckboxGroupInput(session, "filter1_value", selected = unique(config()$train[[input$filter1]]))
    }
  })
  
  # Select all values for filter 2
  observeEvent(input$select_all_filter2, {
    if (is.numeric(config()$train[[input$filter2]])) {
      updateSliderInput(session, "filter2_value", value = range(config()$train[[input$filter2]], na.rm = TRUE))
    } else {
      updateCheckboxGroupInput(session, "filter2_value", selected = unique(config()$train[[input$filter2]]))
    }
  })
  
  # Select all values for filter 3
  observeEvent(input$select_all_filter3, {
    if (is.numeric(config()$train[[input$filter3]])) {
      updateSliderInput(session, "filter3_value", value = range(config()$train[[input$filter3]], na.rm = TRUE))
    } else {
      updateCheckboxGroupInput(session, "filter3_value", selected = unique(config()$train[[input$filter3]]))
    }
  })
  
  # Clear filter 1 values
  observeEvent(input$clear_filter1, {
    if (is.numeric(config()$train[[input$filter1]])) {
      updateSliderInput(session, "filter1_value", value = range(config()$train[[input$filter1]], na.rm = TRUE))
    } else {
      updateCheckboxGroupInput(session, "filter1_value", selected = character(0))
    }
  })
  
  # Clear filter 2 values
  observeEvent(input$clear_filter2, {
    if (is.numeric(config()$train[[input$filter2]])) {
      updateSliderInput(session, "filter2_value", value = range(config()$train[[input$filter2]], na.rm = TRUE))
    } else {
      updateCheckboxGroupInput(session, "filter2_value", selected = character(0))
    }
  })
  
  # Clear filter 3 values
  observeEvent(input$clear_filter3, {
    if (is.numeric(config()$train[[input$filter3]])) {
      updateSliderInput(session, "filter3_value", value = range(config()$train[[input$filter3]], na.rm = TRUE))
    } else {
      updateCheckboxGroupInput(session, "filter3_value", selected = character(0))
    }
  })
  
  # Update select inputs based on feature table
  observe({
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table) %>% filter(Use_Feature == TRUE)
    updateSelectInput(session, "eda_ft", choices = c(ft_spec_table$Features))
    updateSelectInput(session, "eda_interaction", choices = c("none", ft_spec_table$Features))
    updateSelectInput(session, "filter1", choices = c(ft_spec_table$Features))
    updateSelectInput(session, "filter2", choices = c(ft_spec_table$Features))
    updateSelectInput(session, "filter3", choices = c(ft_spec_table$Features))
  })
  
  # Event reactive to aggregate EDA data
  eda_agg_data <- eventReactive(input$refresh_eda, {
    req(config())
    req(sum(c(input$filter1, input$filter2, input$filter3, input$eda_ft, input$eda_interaction) %in% c("none", config()$selected_fts)) == 5)
    req(input$filter1_value, input$filter2_value, input$filter3_value)
    
    filtered_data <- config()$train %>%
      filter(
        if (is.numeric(config()$train[[input$filter1]])) {
          config()$train[[input$filter1]] >= input$filter1_value[1] & config()$train[[input$filter1]] <= input$filter1_value[2]
        } else {
          config()$train[[input$filter1]] %in% input$filter1_value
        },
        if (is.numeric(config()$train[[input$filter2]])) {
          config()$train[[input$filter2]] >= input$filter2_value[1] & config()$train[[input$filter2]] <= input$filter2_value[2]
        } else {
          config()$train[[input$filter2]] %in% input$filter2_value
        },
        if (is.numeric(config()$train[[input$filter3]])) {
          config()$train[[input$filter3]] >= input$filter3_value[1] & config()$train[[input$filter3]] <= input$filter3_value[2]
        } else {
          config()$train[[input$filter3]] %in% input$filter3_value
        }
      )
    
    create_EDA_agg(
      ft = filtered_data[[input$eda_ft]],
      y = filtered_data[[config()$response]] * filtered_data[[config()$weight]],
      weight = filtered_data[[config()$weight]],
      interaction = filtered_data[[input$eda_interaction]],
      ft_nbreaks = input$ft_nbreaks,
      interaction_nbreaks = input$interaction_nbreaks,
      ft_band_type = input$ft_band_type,
      interaction_band_type = input$interaction_band_type
    )
  })
  
  # Observe changes in EDA aggregated data and render plot
  observe({
    req(eda_agg_data())
    print(eda_agg_data())
    
    EDA_smooth_strength <- if (input$eda_fit_loess) {
      input$eda_smooth_strength
    } else {
      0
    }
    
    output$edaPlot <- renderPlotly({
      ggplotly(EDA_plot(agg_df = eda_agg_data(),
                        bar_alpha = input$bar_alpha,
                        lwd = input$lwd,
                        point_size = input$point_size,
                        line_alpha = input$line_alpha,
                        point_alpha = input$point_alpha,
                        ft_name = input$eda_ft,
                        interaction_name = input$eda_interaction,
                        smooth_strength =EDA_smooth_strength )) %>% 
        layout(
          margin = list(
            l = 50,
            r = 50,
            b = 100,
            t = 100,
            pad = 6
          ),
          height = 800
        )
    })
    
  })
  

  ################### Boruta ##############################

  
  # Reactive value to store Boruta result
  load_boruta_result <- reactiveVal()
  
  # Event reactive to run Boruta feature selection
  Boruta_result <- eventReactive(input$Boruta_run, {
    print('Begin Boruta feature selection')
    
    if (length(config()$selected_fts) == 0) {
      print("No features were selected")
    }
    
    req(config())
    
    Bresult <- KT_Boruta(
      train = config()$train %>% select(config()$selected_fts),
      train_y = config()$train_y,
      weight = config()$train_weight,
      max_Runs = input$Boruta_max_run,
      eval_metric = config()$eval_metric,
      objective = config()$objective,
      nrounds = input$Boruta_nrounds,
      max.depth = input$Boruta_max_depth,
      eta = input$Boruta_eta,
      early_stopping_rounds = 5,
      nthread = parallel::detectCores()
    )
    
    show("Update_ft_spec")
    show("boruta_top_selected_fts")
    return(Bresult)
  })
  
  # Observe changes in Boruta result and update slider input
  observe({
    req(Boruta_result())
    updateSliderInput(session, "boruta_top_selected_fts", max = length(Boruta_result()$selected_fts), value = length(Boruta_result()$selected_fts))
  })
  
  # Render Boruta importance plot
  output$Boruta_imp <- renderPlot({
    Boruta_result()$Boruta_p
  })
  
  # Render Boruta SHAP importance plot
  output$Boruta_shap_imp <- renderPlot({
    Boruta_result()$SHAP_imp_plot
  })
  
  # Update feature specification based on Boruta result
  observeEvent(input$Update_ft_spec, {
    boruta_selected_fts <- if (is.null(load_boruta_result())) {
      Boruta_result()$selected_fts
    } else {
      load_boruta_result()$selected_fts
    }
    
    hot_to_r(input$ft_table) %>% 
      left_join(data.table(Features = boruta_selected_fts[1:input$boruta_top_selected_fts], use = TRUE), by = "Features") %>%
      mutate(Use_Feature = use) %>% select(-use) -> boruta_ft_spec
    
    table_data(boruta_ft_spec)
    session
  })
  
  # Save Boruta result to file
  observe({
    req(Boruta_result())
    isolate({
      print("Saving Boruta result")
      saveRDS(list(
        result = Boruta_result(),
        Boruta_max_run = input$Boruta_max_run,
        Boruta_eta = input$Boruta_eta,
        Boruta_max_depth = input$Boruta_max_depth,
        Boruta_nrounds = input$Boruta_nrounds
      ), glue("{input$file_name_boruta}.rds"))
      print("Finished saving Boruta result")
    })
  })
  
  # Load Boruta result from file
  observeEvent(input$load_boruta, {
    if (!file.exists(glue("{input$file_name_boruta}.rds"))) {
      print(glue("{input$file_name_boruta} does not exist"))
    }
    req(file.exists(glue("{input$file_name_boruta}.rds")))
    
    Boruta_result <- readRDS(glue("{input$file_name_boruta}.rds"))
    updateSliderInput(session, "Boruta_max_run", value = Boruta_result$Boruta_max_run)
    updateSliderInput(session, "Boruta_eta", value = Boruta_result$Boruta_eta)
    updateSliderInput(session, "Boruta_max_depth", value = Boruta_result$Boruta_max_depth)
    updateSliderInput(session, "Boruta_nrounds", value = Boruta_result$Boruta_nrounds)
    load_boruta_result(Boruta_result$result)
    
    output$Boruta_imp <- renderPlot({
      Boruta_result$result$Boruta_p
    })
    output$Boruta_shap_imp <- renderPlot({
      Boruta_result$result$SHAP_imp_plot
    })
    
    show("Update_ft_spec")
    show("boruta_top_selected_fts")
    updateSliderInput(session, "boruta_top_selected_fts", max = length(Boruta_result$result$selected_fts), value = length(Boruta_result$result$selected_fts))
  })
  
  
  

  ################### Tune model ##############################

  
  
  
  
  
  
  
  tune_result <- eventReactive(input$tune, {
    tryCatch({
      print('Begin tuning model')
      gc()
      
      # Determine kfold value
      kfold <- if (input$kfold) input$kfold_val else 0
      
      # Ensure feature table is available and has selected features
      req(input$ft_table)
      ft_spec_table <- hot_to_r(input$ft_table) %>% filter(Use_Feature == TRUE)
      
      if (nrow(ft_spec_table) == 0) {
        stop("No features were selected")
      }
      
      req(nrow(ft_spec_table) > 0)
      
      # Extract monotone constraints and interaction constraints
      monotone_constraints <- ft_spec_table %>% select(Monotonicity) %>% pull
      init_X <- seq(1, length(config()$selected_fts))
      interaction_constraints <- lapply(which(ft_spec_table$Interaction_Constraints == TRUE), function(x) c(x))
      interaction_constraints <- append(list(setdiff(init_X, unlist(interaction_constraints))), interaction_constraints)
      
      print(interaction_constraints)
      
      # Tune the model
      tune_model(
        fts = config()$selected_fts,
        model = input$model,
        train = config()$train,
        kfold = kfold,
        train_validate_ratio = input$Ratio,
        eta = input$eta,
        max_depth = input$max_depth,
        min_child_weight = input$min_child_weight,
        subsample = input$subsample,
        colsample_bytree = input$colsample_bytree,
        lambda = input$lambda,
        alpha = input$alpha,
        nrounds = input$nrounds,
        parallel = input$Distribute_Computation,
        interaction_constraints = interaction_constraints,
        monotone_constraints = monotone_constraints,
        gamma = input$gamma,
        ncluster = input$n_core
      )
    }, error = function(e) {
      print(paste("Error in tuning model:", e$message))
      NULL
    })
  })
  
  observe({
    req(tune_result())
    
    # Render tuning iteration plot
    output$tune_iteration_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$tune_iteration
    })
    
    # Render eta plot
    output$eta_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$eta
    })
    
    # Render max depth plot
    output$max_depth_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$max_depth
    })
    
    # Render min child weight plot
    output$min_child_weight_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$min_child_weight
    })
    
    # Render subsample plot
    output$subsample_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$subsample
    })
    
    # Render colsample bytree plot
    output$colsample_bytree_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$colsample_bytree
    })
    
    # Render lambda plot
    output$lambda_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$lambda
    })
    
    # Render alpha plot
    output$alpha_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$alpha
    })
    
    # Render gamma plot
    output$gamma_plot <- renderPlotly({
      tune_result()$hyperparameters_trends$gamma
    })
    
    # Render optimization result table
    output$opt_result_plot <- DT::renderDataTable({
      tune_result()$opt_results %>% mutate_all(~ round(., 3))
    })
  })
  
  # Function to save Tuning tab state
  save_tuning <- function() {
      file_name <- paste0(input$file_name_tuning, ".rds")
      
      saveRDS(list(
        Trainvalidate = input$Trainvalidate,
        Ratio = input$Ratio,
        kfold = input$kfold,
        kfold_val = input$kfold_val,
        eta = input$eta,
        min_child_weight = input$min_child_weight,
        max_depth = input$max_depth,
        alpha = input$alpha,
        lambda = input$lambda,
        colsample_bytree = input$colsample_bytree,
        subsample = input$subsample,
        gamma = input$gamma,
        nrounds = input$nrounds,
        optz_result = tune_result()
      ), file_name)
      
      saveRDS(tune_result()$best_params, glue("{input$file_name_tuning}_best_param.rds"))
      
      output$action_message_tuning <- renderText("Tuning state has been saved.")

  }
  
  # Function to load Tuning tab state
  load_tuning <- function() {
      file_name <- paste0(input$file_name_tuning, ".rds")
      if (file.exists(file_name)) {
        tune_result <- readRDS(file_name)
        
        updateSliderInput(session, "eta", value = tune_result$eta)
        updateSliderInput(session, "min_child_weight", value = tune_result$min_child_weight)
        updateSliderInput(session, "max_depth", value = tune_result$max_depth)
        updateSliderInput(session, "alpha", value = tune_result$alpha)
        updateSliderInput(session, "lambda", value = tune_result$lambda)
        updateSliderInput(session, "colsample_bytree", value = tune_result$colsample_bytree)
        updateSliderInput(session, "subsample", value = tune_result$subsample)
        updateSliderInput(session, "nrounds", value = tune_result$nrounds)
        updateSliderInput(session, "gamma", value = tune_result$gamma)
        
        hp_plots <- tune_result$optz_result$hyperparameters_trends
        
        output$tune_iteration_plot <- renderPlotly({
          hp_plots$tune_iteration
        })
        
        output$eta_plot <- renderPlotly({
          hp_plots$eta
        })
        
        output$max_depth_plot <- renderPlotly({
          hp_plots$max_depth
        })
        
        output$min_child_weight_plot <- renderPlotly({
          hp_plots$min_child_weight
        })
        
        output$colsample_bytree_plot <- renderPlotly({
          hp_plots$colsample_bytree
        })
        
        output$subsample_plot <- renderPlotly({
          hp_plots$subsample
        })
        
        output$lambda_plot <- renderPlotly({
          hp_plots$lambda
        })
        
        output$alpha_plot <- renderPlotly({
          hp_plots$alpha
        })
        
        output$gamma_plot <- renderPlotly({
          hp_plots$gamma
        })
        
        output$opt_result_plot <- DT::renderDataTable({
          tune_result$optz_result$opt_results %>% mutate_all(~ round(., 3))
        })
        output$action_message_tuning <- renderText("Tuning state has been loaded.")
      } else {
        output$action_message_tuning <- renderText("File not found.")
      }

  }
  
  # Function to load best parameters from Tuning tab state
  load_tuning_best_param <- function() {
      file_name <- glue("{input$file_name_tuning}_best_param.rds")
      req(file.exists(file_name))
      
      if (file.exists(file_name)) {
        tune_result <- readRDS(file_name)
        
        updateSliderInput(session, "train_eta", value = tune_result$eta)
        updateSliderInput(session, "train_min_child_weight", value = tune_result$min_child_weight)
        updateSliderInput(session, "train_max_depth", value = tune_result$max_depth)
        updateSliderInput(session, "train_alpha", value = tune_result$alpha)
        updateSliderInput(session, "train_lambda", value = tune_result$lambda)
        updateSliderInput(session, "train_colsample_bytree", value = tune_result$colsample_bytree)
        updateSliderInput(session, "train_subsample", value = tune_result$subsample)
        updateSliderInput(session, "train_nrounds", value = tune_result$nrounds)
        updateSliderInput(session, "train_gamma", value = tune_result$gamma)
        
        output$action_message_tuning <- renderText("Tuning state has been loaded.")
      } else {
        output$action_message_tuning <- renderText("File not found.")
      }

  }
  
  
  
  observeEvent(tune_result(), {
    
    req(tune_result())
    print("Saving tuned result")
    save_tuning()
    print("finished saving tuned result")
  })
  
  observeEvent(input$load_tuning, {
    print("loading tuned result")
    load_tuning()
  })
  observeEvent(input$load_tuning_best_param, {
    print("loading tuned HPs")
    load_tuning_best_param()
  })
  
  
 
  ################### Train model ##############################

  
  
  # Observe changes in kfold input and update Trainvalidate checkbox
  observeEvent(input$kfold, {
    if (input$kfold) {
      updateCheckboxInput(session, "Trainvalidate", value = FALSE)
    }
  })
  
  # Observe changes in Trainvalidate input and update kfold checkbox
  observeEvent(input$Trainvalidate, {
    if (input$Trainvalidate) {
      updateCheckboxInput(session, "kfold", value = FALSE)
    }
  })
  
  # Observe changes in train_kfold input and update train_Trainvalidate checkbox
  observeEvent(input$train_kfold, {
    if (input$train_kfold) {
      updateCheckboxInput(session, "train_Trainvalidate", value = FALSE)
    }
  })
  
  # Observe changes in train_Trainvalidate input and update train_kfold checkbox
  observeEvent(input$train_Trainvalidate, {
    if (input$train_Trainvalidate) {
      updateCheckboxInput(session, "train_kfold", value = FALSE)
    }
  })
  
  # Event reactive to train the model
  train_result <- eventReactive(input$train, {
      print('Begin training model')
      gc()
      
      kfold <- if (input$train_kfold) input$train_kfold_val else 0
      early_stopping_rounds <- if (input$use_early_stopping_rounds) 5 else NULL
      
      req(input$ft_table)
      ft_spec_table <- hot_to_r(input$ft_table) %>% filter(Use_Feature == TRUE)
      
      if (nrow(ft_spec_table) == 0) {
        stop("No features were selected")
      }
      
      monotone_constraints <- ft_spec_table %>% select(Monotonicity) %>% pull
      init_X <- seq(1, length(config()$selected_fts))
      interaction_constraints <- lapply(which(ft_spec_table$Interaction_Constraints == TRUE), function(x) c(x))
      interaction_constraints <- append(list(setdiff(init_X, unlist(interaction_constraints))), interaction_constraints)
      
      print(interaction_constraints)
      
      train_model(
        fts = config()$selected_fts,
        model = input$model,
        train = config()$train,
        kfold = kfold,
        train_validate_ratio = input$train_Ratio,
        eta = input$train_eta,
        max_depth = input$train_max_depth,
        min_child_weight = input$train_min_child_weight,
        subsample = input$train_subsample,
        colsample_bytree = input$train_colsample_bytree,
        lambda = input$train_lambda,
        alpha = input$train_alpha,
        nrounds = input$train_nrounds,
        parallel = TRUE,
        gamma = input$gamma,
        interaction_constraints = interaction_constraints,
        monotone_constraints = monotone_constraints,
        early_stopping_rounds = early_stopping_rounds
      )
  })
  
  # Reactive value to store trained model
  trained_model <- reactiveVal(NULL)
  
  # Reactive value to store SHAP values
  shap_values <- reactiveVal(NULL)
  
  # Reactive to generate SHAP plots
  SHAP_plots <- reactive({
      KT_plot_shap(
        sv = shap_values()$main_effect$shap_main_effect[[input$SHAP_ft]],
        ft = shap_values()$main_effect$pred_data_main_effect[, input$SHAP_ft],
        ft_name = "",
        loess_strength = input$SHAP_smooth_strength,
        point_size = input$SHAP_pt_size,
        alpha = input$SHAP_alpha,
        sample_size = input$SHAP_sample_size
      )
  })
  
  # Reactive to generate SHAP interaction plots
  SHAP_plots_X <- reactive({
      loess_strength <- if (input$SHAP_X_Fit_loess) input$SHAP_smooth_strength else 0
      sv_X <- glue("{input$SHAP_X_ft1}.{input$SHAP_X_ft2}")
      
      KT_plot_shap_w_interaction(
        sv = shap_values()$interaction_effect$shap_interaction[[sv_X]],
        ft = shap_values()$interaction_effect$pred_data_interaction[, input$SHAP_X_ft1],
        ft_name = sv_X,
        interaction = shap_values()$interaction_effect$pred_data_interaction[, input$SHAP_X_ft2],
        loess_strength = loess_strength,
        point_size = input$SHAP_pt_size,
        alpha = input$SHAP_alpha,
        sample_size = input$SHAP_sample_size
      )

  })
  
  # Observe changes in train result and update slider input
  observeEvent(train_result(), {
    model <- train_result()$model
    trained_model(model)
    updateSliderInput(session, "tree_index", max = model$niter - 1)
  })
  
  # Render tree plot
  output$tree_plot <- DiagrammeR::renderGrViz({
    req(trained_model())
    model <- trained_model()
    tree_index <- input$tree_index
    xgb.plot.tree(model = model, trees = tree_index)
  })
  
  # Observe changes and render SHAP plots
  observe({
    output$SHAP_plot <- renderPlot({
      SHAP_plots()
    })
    
    output$SHAP_X_plot <- renderPlot({
      SHAP_plots_X()
    })
  })
  
  # Render gain importance plot
  output$Gain_imp_plot <- renderPlotly({
    ggplotly(train_result()$imp_plot$imp_gain + theme_light()) %>% layout(height = 1000)
  })
  
  # Render SHAP importance plot
  output$SHAP_imp_plot <- renderPlotly({
    ggplotly(train_result()$imp_plot$imp_shap) %>% layout(height = 1000)
  })
  
  # Observe changes in train result and render comparison plot
  observe({
    req(train_result())
    output$imp_comparison <- renderPlot({
      train_result()$imp_plot$imp_comparison
    })
    
    output$Interaction_matrix <- renderPlotly({
      ggplotly(
        dcast(train_result()$imp_plot$EIXinteraction_gain_matrix, Parent ~ Child, value.var = "sumGain") %>%
          melt(id.vars = "Parent") %>%
          filter(!is.na(value)) %>%
          rename(Child = variable, sumGain = value) %>%
          arrange(-sumGain) %>%
          head(input$top_gain_X) %>%
          ggplot(aes(x = Child, y = Parent, fill = sumGain)) +
          geom_tile() +
          scale_fill_gradientn(colors = c("aliceblue", "lightblue", "blue")) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.9))
      ) %>% layout(height = 1000, width = 1000)
    })
    
    output$SHAP_Interaction_matrix <- renderPlotly({
      ggplotly(
        train_result()$imp_plot$imp_shap_X %>%
          head(input$top_shap_X) %>%
          ggplot(aes(x = ft, y = interaction, fill = value)) +
          geom_tile() +
          scale_fill_gradientn(colors = c("aliceblue", "lightblue", "blue")) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.9))
      ) %>% layout(height = 1000, width = 1000)
    })
    
    observeEvent(train_result(), {
      model <- train_result()$model
      trained_model(model)
      updateSliderInput(session, "topfeatures", max = model$nfeatures - 1)
      updateSelectInput(session, "y_axis", choices = c("sumGain", "meanGain", "sumCover", "meanCover", "frequency"))
      updateSelectInput(session, "x_axis", choices = c("sumCover", "sumGain", "meanGain", "meanCover", "frequency"))
    })
    
    output$Interaction_gain <- renderPlot({
      plot(train_result()$imp_plot$EIXinteraction_gain, top = input$topfeatures, text_size = 6) + theme(
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17)
      )
    })
    
    output$Interaction_gain2 <- renderPlot({
      plot(train_result()$imp_plot$EIXinteraction_gain, top = input$topfeatures, xmeasure = input$x_axis, ymeasure = input$y_axis, radar = FALSE)
    })
    
    output$gain <- renderPlot({
      plot(train_result()$imp_plot$EIX_gain, top = input$topfeatures, text_size = 6) + theme(
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17)
      )
    })
    
    output$gain2 <- renderPlot({
      plot(train_result()$imp_plot$EIX_gain, top = input$topfeatures, xmeasure = input$x_axis, ymeasure = input$y_axis, radar = FALSE)
    })
  })
  
  # Function to save Training tab state
  save_Training <- function() {
      file_name <- paste0(input$file_name_Training, ".rds")
      saveRDS(list(
        train_eta = input$train_eta,
        train_min_child_weight = input$train_min_child_weight,
        train_max_depth = input$train_max_depth,
        train_alpha = input$train_alpha,
        train_lambda = input$train_lambda,
        train_colsample_bytree = input$train_colsample_bytree,
        train_subsample = input$train_subsample,
        train_gamma = input$train_gamma,
        train_nrounds = input$train_nrounds,
        train_Ratio = input$train_Ratio,
        train_kfold_val = input$train_kfold_val,
        train_kfold = input$train_kfold,
        train_use_early_stopping_rounds = input$use_early_stopping_rounds,
        model_name = input$model,
        model_output = train_result(),
        ft_spec = hot_to_r(input$ft_table)
      ), file_name)
      
      output$action_message_training <- renderText("Training state has been saved.")

  }
  
  # Function to load Training tab state
  load_Training <- function() {
      file_name <- paste0(input$file_name_Training, ".rds")
      req(file.exists(file_name))
      loaded_state <- readRDS(file_name)
      
      # Update UI elements with loaded state
      updateSliderInput(session, "train_eta", value = loaded_state$train_eta)
      updateSliderInput(session, "train_min_child_weight", value = loaded_state$train_min_child_weight)
      updateSliderInput(session, "train_max_depth", value = loaded_state$train_max_depth)
      updateSliderInput(session, "train_alpha", value = loaded_state$train_alpha)
      updateSliderInput(session, "train_lambda", value = loaded_state$train_lambda)
      updateSliderInput(session, "train_colsample_bytree", value = loaded_state$train_colsample_bytree)
      updateSliderInput(session, "train_subsample", value = loaded_state$train_subsample)
      updateSliderInput(session, "train_nrounds", value = loaded_state$train_nrounds)
      updateSliderInput(session, "train_Ratio", value = loaded_state$train_Ratio)
      updateSliderInput(session, "train_kfold_val", value = loaded_state$train_kfold_val)
      updateSliderInput(session, "train_gamma", value = loaded_state$train_gamma)
      updateSliderInput(session, "use_early_stopping_rounds", value = loaded_state$train_use_early_stopping_rounds)
      updateSliderInput(session, "tree_index", max = loaded_state$model_output$model$niter - 1)
      
      # Update table data
      table_data(feature_spec %>% select(Features, dtype) %>%
                   left_join(loaded_state$ft_spec %>% select(Features, Use_Feature, Monotonicity, Interaction_Constraints), by = "Features") %>%
                   mutate(Monotonicity = ifelse(is.na(Monotonicity), 0, Monotonicity)) %>%
                   mutate_at(vars(Use_Feature, Interaction_Constraints), ~ ifelse(is.na(.x), F, .x)))
      
      # Trigger the rendering of plots
      output$Gain_imp_plot <- renderPlotly({
        ggplotly(loaded_state$model_output$imp_plot$imp_gain + theme_light()) %>% layout(height = 1000)
      })
      
      output$SHAP_imp_plot <- renderPlotly({
        ggplotly(loaded_state$model_output$imp_plot$imp_shap) %>% layout(height = 1000)
      })
      
      output$gain <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIX_gain, top = input$topfeatures, text_size = 6) + theme(
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 17)
        )
      })
      
      output$gain2 <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIX_gain,
             top = input$topfeatures,
             xmeasure = input$x_axis,
             ymeasure = input$y_axis,
             radar = FALSE)
      })
      
      output$Interaction_matrix <- renderPlotly({
        ggplotly(
          dcast(loaded_state$model_output$imp_plot$EIXinteraction_gain_matrix, Parent ~ Child, value.var = "sumGain") %>%
            melt(id.vars = "Parent") %>%
            filter(!is.na(value)) %>%
            rename(Child = variable, sumGain = value) %>%
            arrange(-sumGain) %>%
            head(input$top_gain_X) %>%
            ggplot(aes(x = Child, y = Parent, fill = sumGain)) +
            geom_tile() +
            scale_fill_gradientn(colors = c("aliceblue", "lightblue", "blue")) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.9))
        ) %>% layout(height = 1000, width = 1000)
      })
      
      output$SHAP_Interaction_matrix <- renderPlotly({
        ggplotly(loaded_state$model_output$imp_plot$imp_shap_X %>%
                   head(input$top_shap_X) %>%
                   ggplot(aes(x = ft, y = interaction, fill = value)) +
                   geom_tile() +
                   scale_fill_gradientn(colors = c("aliceblue", "lightblue", "blue")) +
                   theme_light() +
                   theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.9))) %>%
          layout(height = 1000, width = 1000)
      })
      
      output$Interaction_gain <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIXinteraction_gain, top = input$topfeatures, text_size = 6) + theme(
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 17)
        )
      })
      
      output$Interaction_gain2 <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIXinteraction_gain,
             top = input$topfeatures,
             xmeasure = input$x_axis,
             ymeasure = input$y_axis,
             radar = FALSE)
      })
      
      output$tree_plot <- DiagrammeR::renderGrViz({
        req(loaded_state$model_output)
        model <- loaded_state$model_output$model
        xgb.plot.tree(model = model, trees = input$tree_index)
      })
      
      output$imp_comparison <- renderPlot({
        KT_plot_compare_ft_imp(loaded_state$model_output$imp_plot$imp_gain$data$Feature,
                               loaded_state$model_output$imp_plot$imp_shap$data$variable) +
          theme(legend.position = "none") +
          theme_light(base_size = 18) + ggtitle("gain vs SHAP importance")
      })
      
      output$action_message_training <- renderText("Training state has been loaded.")
  }
  
  # Observe event to save PMML model
  observeEvent(input$train, {
      req(train_result())
    print("export_pmml")
      r2pmml::r2pmml(train_result()$model, glue("{input$file_name_Training}.pmml"), fmap = train_result()$pmml_fmap, response_name = "prediction")

  })
  
  # Observe event to save training result
  observeEvent(train_result(), {
      req(train_result())
      print("Saving trained result")
      save_Training()
      print("Finished saving trained result")

  })
  
  # Observe event to load training result
  observeEvent(input$load_Training, {
      print("Loading trained result")
      load_Training()

  })
  
  
  # Reactive file reader for base model
  base_model <- reactiveFileReader(
    intervalMillis = 1000,  # Check every 1000 milliseconds (1 second)
    session = session,
    filePath = reactive({
      req(input$file_name_Training)  # Ensure input$file_name_Training is not NULL
      glue("{input$file_name_Training}.rds")  # Construct the file path
    }),
    readFunc = function(filePath) {
      tryCatch(
        {
          readRDS(filePath)
        },
        error = function(e) {
          # Handle the error (e.g., log it, show a message, etc.)
          message("Error reading RDS file: ", e$message)
          NULL  # Return NULL or some default value
        }
      )
    }
  )
  
  # Observe and load training data
  observe({
    req(input$load_Training || input$train)
    req(file.exists(glue("{input$file_name_Training}.rds")))
    req(base_model())
    tryCatch({
    model_output <- base_model()$model_output
    shap_values(model_output$shap_values)
    
    ft_imp <- model_output$imp_plot$imp_shap$data$variable
    updateSelectInput(session, "SHAP_ft", choices = ft_imp)
    updateSelectInput(session, "SHAP_X_ft1", choices = ft_imp)
    updateSelectInput(session, "SHAP_X_ft2", choices = ft_imp)
    output$Shap_value_loaded <- renderText("Shap_Value loaded.")
    }, error = function(e) {
      message("Error in loading SHAP: ", e$message)
      return(NULL)
    })
  })
  
  ################### GLM #####################################
  
  # Initialize reactive values
  drawn_shapes <- reactiveVal(list())
  x_range <- reactiveVal(NULL)
  y_range <- reactiveVal(NULL)
  y2_range <- reactiveVal(NULL)
  

  
  # Event reactive for overlays
  overlays <- eventReactive({
    input$Fit
    input$Load_ave
    base_model()
  }, {
      if (input$ignore_base_pred == TRUE) {
        base_pred <- 1
      } else {
        req(base_model())
        base_pred <- base_model()$model_output$pred
      }
      req( drawn_shapes())
      fam <- model_spec[[input$model]]$fam
      splines_dt <- do.call(rbind, drawn_shapes())
      glm_train <- train[train[[config()$weight]] > 0] %>% select(unique(splines_dt$feature))
      
      if (nrow(splines_dt) > 0 && !is.null(splines_dt)) {
        glm_fit(glm_train, splines_dt, config()$train_y, base_pred, config()$train_weight, fam)
      } else {
        return(NULL)
      }

  })
  
  # Observe and update UI elements based on data
  observe({
      req(input$ft)
      data <- train[train[[config()$weight]] > 0][[input$ft]]
      unique_values <- length(unique(data))
      
      if (unique_values <= 20) {
        updateCheckboxInput(session, "band_ft", value = FALSE)
        hide("band_ft")
        hide("glm_band_method")
        hide("overlay_nbreaks")
      } else if (unique_values > 150) {
        updateCheckboxInput(session, "band_ft", value = TRUE)
        hide("band_ft")
        show("glm_band_method")
        show("overlay_nbreaks")
      } else {
        show("band_ft")
        show("glm_band_method")
        show("overlay_nbreaks")
      }

  })
  
  # Reactive expression to generate fit plot
  fit_plot <- reactive({
      # Determine base prediction
      if (input$ignore_base_pred == TRUE) {
        base_pred <- 1
      } else {
        req(input$load_Training || input$train)
        req(base_model())
        base_pred <- base_model()$model_output$pred
      }
      
      # Determine challenger and individual effects
      if (is.null(overlays())) {
        challenger <- base_pred
        indiv_eff <- 1
      } else {
        challenger <- base_pred * overlays()$adj
        indiv_eff <- if (input$ft %in% names(overlays()$indiv_eff)) {
          overlays()$indiv_eff[[input$ft]]
        } else {
          1
        }
      }
      
      # Determine if feature should be banded
      ft <- train[train[[config()$weight]] > 0][[input$ft]]
      band_ft <- if (length(unique(ft)) > 150) TRUE else input$band_ft
      
      # Generate fit plot
      plot_fit(
        ft = ft,
        actual = config()$train_y * config()$train_weight,
        pred = base_pred,
        challenger = challenger,
        weight = config()$train_weight,
        rebase = TRUE,
        point_size = input$size_pt,
        lwd = input$size_line,
        fit_lines = input$fit_lines,
        ft_name = input$ft,
        band_ft = band_ft,
        nbreaks = input$overlay_nbreaks,
        indiv_eff = indiv_eff,
        band_method = input$glm_band_method
      )

  })
  
  # Observe event to export lookup tables in PMML format
  observeEvent(input$lookup_pmml_export, {
      req(overlays())
      exp_path <- glue("{getwd()}/{input$glm_overlay_out}_GLMpmml")
      if (!file.exists(exp_path)) {
        dir.create(exp_path)
      } else {
        files <- list.files(exp_path, full.names = TRUE)
        file.remove(files)
      }
      print("Exporting lookup table in PMML format")
      KT_Export_tables_to_pmml(overlays()$lookup_tables, input$glm_overlay_out, export_path = exp_path)
      KT_export_to_excel(overlays()$band_logic_for_rdr, glue("{exp_path}/band_logic_for_rdr.xlsx"), withcolnames = FALSE)
  })
  
  # Render Plotly overlay plot
  output$overlay_plot <- renderPlotly({
    tryCatch({
      req(fit_plot())
      p <- ggplotly(fit_plot()) %>% layout(
        legend = list(
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.2
        )
      )
      shapes <- drawn_shapes()[[input$ft]]
      
      if (!is.null(shapes) && input$show_splines) {
        p <- p %>%
          add_segments(
            data = shapes,
            x = ~x0, xend = ~x1, y = ~y0, yend = ~y1,
            line = list(color = "red"),
            showlegend = FALSE
          )
      }
      
      p <- p %>%
        layout(
          dragmode = if (input$draw_mode) "drawline" else "pan",
          newshape = list(line = list(color = "red")),
          yaxis2 = list(overlaying = "y", side = "right", range = y2_range() %||% range(p$data$value))
        ) %>%
        event_register("plotly_relayout")
      
      isolate({
        if (is.null(x_range())) x_range(range(p$data$ft))
        if (is.null(y_range())) y_range(range(p$data$value))
        if (is.null(y2_range())) y2_range(range(p$data$value))
      })
      
      p
    }, error = function(e) {
      message("Error in overlay_plot: ", e$message)
      return(NULL)
    })
  })
  
  
  # Observe event for Plotly relayout
  observeEvent(event_data("plotly_relayout"), {
    tryCatch({
      dtype <- class(train[[input$ft]])
      relayout_data <- event_data("plotly_relayout")
      
      if (!is.null(relayout_data)) {
        if (!is.null(relayout_data[["xaxis.range[0]"]])) x_range(c(relayout_data[["xaxis.range[0]"]], relayout_data[["xaxis.range[1]"]]))
        if (!is.null(relayout_data[["yaxis.range[0]"]])) y_range(c(relayout_data[["yaxis.range[0]"]], relayout_data[["yaxis.range[1]"]]))
        if (!is.null(relayout_data[["yaxis2.range[0]"]])) y2_range(c(relayout_data[["yaxis2.range[0]"]], relayout_data[["yaxis.range[1]"]]))
        
        if (!is.null(relayout_data$shapes)) {
          shapes <- relayout_data$shapes
          if (is.data.frame(shapes) && nrow(shapes) > 0) {
            new_shapes <- data.frame(x0 = numeric(0), y0 = numeric(0), x1 = numeric(0), y1 = numeric(0), id = character(), feature = character(), range = character())
            
            for (i in seq_along(shapes$x0)) {
              if (!(shapes$x0[i] == 0 && shapes$y0[i] == 0 && shapes$x1[i] == 1 && shapes$y1[i] == 1)) {
                if (shapes$x0[i] > shapes$x1[i]) {
                  temp <- shapes$x0[i]
                  shapes$x0[i] <- shapes$x1[i]
                  shapes$x1[i] <- temp
                  
                  temp <- shapes$y0[i]
                  shapes$y0[i] <- shapes$y1[i]
                  shapes$y1[i] <- temp
                }
                
                existing_shapes <- drawn_shapes()[[input$ft]]
                overlap <- any(existing_shapes$x0 <= shapes$x1[i] & existing_shapes$x1 >= shapes$x0[i])
                
                if (overlap) {
                  existing_shape <- existing_shapes[existing_shapes$x1 >= shapes$x0[i], ]
                  y_intercept <- existing_shape$y1
                  shapes$y0[i] <- y_intercept
                  shapes$x0[i] <- existing_shape$x1
                  
                  new_shapes <- rbind(new_shapes, data.frame(
                    x0 = existing_shape$x0,
                    y0 = existing_shape$y0, 
                    x1 = existing_shape$x1,
                    y1 = existing_shape$y1, 
                    id = glue("{input$ft}_{round(existing_shape$x0, 0)}to{round(existing_shape$x1, 0)}"),
                    feature = input$ft, 
                    range = glue("{round(existing_shape$x0, 0)}to{round(existing_shape$x1, 0)}"), 
                    overlap = overlap,
                    x0_lvl_name = existing_shape$x0,
                    x1_lvl_name = existing_shape$x1, 
                    dtype = dtype
                  ))
                  
                  existing_shapes <- existing_shapes[existing_shapes$x1 < shapes$x0[i] | existing_shapes$x0 > shapes$x1[i], ]
                }
                
                new_shapes <- rbind(new_shapes, data.frame(
                  x0 = shapes$x0[i],
                  y0 = shapes$y0[i], 
                  x1 = shapes$x1[i], 
                  y1 = shapes$y1[i], 
                  id = glue("{input$ft}_{round(shapes$x0[i], 0)}to{round(shapes$x1[i], 0)}"), 
                  feature = input$ft, 
                  range = glue("{round(shapes$x0[i], 0)}to{round(shapes$x1[i], 0)}"),
                  overlap = overlap,
                  x0_lvl_name = shapes$x0[i],
                  x1_lvl_name = shapes$x1[i],
                  dtype = dtype
                ))
              }
            }
            
            all_shapes <- drawn_shapes()
            
            all_shapes[[input$ft]] <- if (input$band_ft == TRUE) {
              fit_plot()$data$ft -> lvl_name
              data.table(lvl_name = lvl_name, idx = 1:length(lvl_name), ft = input$ft) -> lvl_name
              shape_data <- rbind(existing_shapes, new_shapes) %>% mutate(x0 = round(x0), x1 = round(x1)) %>% distinct(id, .keep_all = TRUE)
              
              if (NA %in% lvl_name$lvl_name) {
                print("Cannot fit to NA level")
                NA_idx <- lvl_name %>% filter(is.na(lvl_name)) %>% select(idx) %>% pull
                shape_data <- shape_data %>% filter(!x1 %in% NA_idx)
              }
              
              shape_data <- shape_data %>% 
                left_join(lvl_name, by = c(x0 = "idx", "feature" = "ft")) %>%
                mutate(x0_lvl_name = ifelse(overlap == FALSE, as.numeric(sub("\\(([^,]+),.*", "\\1", lvl_name)), as.numeric(sub(".*,([^,]+)\\]", "\\1", lvl_name)))) %>% select(-lvl_name) %>%
                left_join(lvl_name, by = c(x1 = "idx", "feature" = "ft")) %>%
                mutate(x1_lvl_name = as.numeric(sub(".*,([^,]+)\\]", "\\1", lvl_name))) %>% select(-lvl_name) %>%
                mutate_at(vars(c("x1_lvl_name", "x0_lvl_name")), ~ ifelse(dtype == "integer", round(.x), custom_round(.x, 2))) %>%
                mutate(id = glue("{input$ft}_{x0_lvl_name}to{x1_lvl_name}"))
            } else {
              rbind(existing_shapes, new_shapes) %>% 
                mutate(x0_lvl_name = ifelse(dtype == "integer", round(x0), custom_round(x0, 2)),
                       x1_lvl_name = ifelse(dtype == "integer", round(x1), custom_round(x1, 2)),
                       id = glue("{input$ft}_{x0_lvl_name}to{x1_lvl_name}"))
            }
            
            drawn_shapes(all_shapes)
            updateCheckboxGroupInput(session, "undo_shapes", choices = do.call(rbind, all_shapes)$id)
          }
        }
      }
    }, error = function(e) {
      message("Error in observeEvent for plotly_relayout: ", e$message)
    })
  })
  
  
  
  # Observe changes in input feature type and update UI elements accordingly
  observe({

      if (!is.numeric(train[[input$ft]])) {
        updateCheckboxInput(session = session, "draw_mode", value = FALSE)
        hide("draw_mode")
        hide("reset")
      } else {
        updateCheckboxInput(session = session, "draw_mode", value = TRUE)
        show("draw_mode")
        show("reset")
      }
  })
  
  # Observe event to undo shapes
  observeEvent(input$undo, {
      req(input$undo_shapes)
      all_shapes <- drawn_shapes()
      for (ft in names(all_shapes)) {
        shapes <- all_shapes[[ft]]
        shapes <- shapes[!shapes$id %in% input$undo_shapes, ]
        all_shapes[[ft]] <- shapes
      }
      drawn_shapes(all_shapes)
      updateCheckboxGroupInput(session, "undo_shapes", choices = sort(do.call(rbind, all_shapes)$id))
  })
  
  # Render table for GLM fit
  output$glm_fit <- renderTable({
      all_shapes <- drawn_shapes()
      if (is.null(overlays())) {
        do.call(rbind, all_shapes) %>% select(-x0, -x1, -y0, -y1, -x1_lvl_name, -x0_lvl_name) %>% arrange(id)
      } else {
        do.call(rbind, all_shapes) %>% left_join(overlays()$fit, by = "id") %>% select(-x0, -x1, -y0, -y1, -x1_lvl_name, -x0_lvl_name) %>% arrange(id, estimate)
      }
  })
  
  # Render summary for GLM model
  output$glm_summary <- renderPrint({
      req(overlays())
      summary(overlays()$model)
  })
  
  # Observe event to reset shapes
  observeEvent(input$reset, {
      drawn_shapes(list())
      updateCheckboxGroupInput(session, "undo_shapes", choices = c())
  })
  
  # Download handler for overlay fit
  output$overlayfit_download <- downloadHandler(
    filename = function() {
      paste("drawn_shapes", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        all_shapes <- drawn_shapes()
        combined_shapes <- do.call(rbind, all_shapes)
        write.csv(combined_shapes, file, row.names = FALSE)

    }
  )
  
  # Observe event to save GLM model
  observeEvent(input$save_glm, {
      req(glm_model_out())
      print("Saving GLM")
      saveRDS(glm_model_out(), file = paste0(input$glm_overlay_out, ".rds"))
      print("Done!")
  })
  
  # Reactive expression for GLM model output
  glm_model_out <- reactive({
      req(drawn_shapes(), overlays())
      base_pred <- if (input$ignore_base_pred == TRUE) {
        1
      } else {
        req(base_model())
        base_pred <- base_model()$model_output$pred
      }
      list(drawn_shapes = drawn_shapes(), undo_shapes = input$undo_shapes, pred = base_pred * overlays()$adj, glm_model_out = overlays())
  })
  
  # Observe event to load GLM model
  observeEvent(input$load_glm, {
      if (!file.exists(paste0(input$glm_overlay_out, ".rds"))) {
        print(glue("{input$glm_overlay_out} does not exist"))
      }
      req(file.exists(paste0(input$glm_overlay_out, ".rds")))
      loaded_data <- readRDS(paste0(input$glm_overlay_out, ".rds"))
      drawn_shapes(loaded_data$drawn_shapes)
      updateCheckboxGroupInput(session, "undo_shapes", choices = sort(do.call(rbind, loaded_data$drawn_shapes)$id), selected = loaded_data$undo_shapes)

  })
  
  
  # Observe event to show factor consistency input
  observeEvent(input$Load_ave, {
      show("factor_consistency")
  })
  
  # Observe changes and update select inputs
  observe({
      updateSelectInput(session, "ft", choices = glm_ft_list())
      updateSelectInput(session, "factor_consistency", choices = c("none", "rnd_factor", fts))
      updateSelectInput(session, "filter_feature", choices = c("none", fts))
      updateSelectInput(session, "secondary_filter_feature", choices = c("none", fts))
      updateSelectInput(session, "tertiary_filter_feature", choices = c("none", fts))
  })
  
  # Reactive expression to generate feature list
  glm_ft_list <- reactive({
    tryCatch(
      {
        default_lst <- train %>% select(starts_with(fts)) %>% names %>% sort
        if (input$ignore_base_pred == TRUE) {
          return(default_lst)
        } else {
          req(file.exists(glue("{input$file_name_Training}.rds")))
          req(base_model())
          gbm_fts <- train %>% select(starts_with(config()$selected_fts)) %>% names
          
          lab <- lapply(default_lst, function(x) 
            if (x %in% gbm_fts) { paste(x, "(xgb fitted)", " ") } else { x }) %>% unlist()
          
          return(lapply(default_lst, function(x) x) %>% setNames(., lab))
        }
      },
      error = function(e) {
        # Handle the error (e.g., log it, show a message, etc.)
        message("Error in glm_ft_list: ", e$message)
        NULL  # Return NULL or some default value
      }
    )
  })
  
  # Reactive expression for sampling
  sampling <- reactive({
      set.seed(1)
      
      base_pred <- if (input$ignore_base_pred == TRUE) {
        1
      } else {
        base_pred <- base_model()$model_output$pred
      }
      
      challenger <- if (is.null(overlays())) {
        base_pred
      } else {
        base_pred * overlays()$adj
      }
      
      df_sample <- train[train[[config()$weight]] > 0] %>% 
        select(starts_with(c(fts, config()$weight, config()$response))) %>%
        mutate(pred = challenger, none = "NA") %>% sample_frac(input$samplesize)
      
      return(list(df_sample = df_sample))
  })
  
  # Observe event to update filter UI based on selected feature
  observeEvent({
    input$filter_feature
    input$Load_ave
  }, {
      req(input$filter_feature)
      feature_data <- sampling()$df_sample[[input$filter_feature]]
      
      if (is.numeric(feature_data)) {
        output$filter_ui <- renderUI({
          sliderInput("feature_range", "Feature Range:", min = min(feature_data, na.rm = TRUE), max = max(feature_data, na.rm = TRUE), value = range(feature_data, na.rm = TRUE))
        })
      } else {
        output$filter_ui <- renderUI({
          checkboxGroupInput("feature_categories", "Select Categories:", choices = KT_dym_sort(unique(feature_data)), selected = KT_dym_sort(unique(feature_data)))
        })
      }
  })
  
  # Observe event to update secondary filter UI based on selected feature
  observeEvent({
    input$secondary_filter_feature
    input$Load_ave
  }, {
      req(input$secondary_filter_feature)
      feature_data <- sampling()$df_sample[[input$secondary_filter_feature]]
      
      if (is.numeric(feature_data)) {
        output$secondary_filter_ui <- renderUI({
          sliderInput("secondary_feature_range", "Secondary Feature Range:", min = min(feature_data, na.rm = TRUE), max = max(feature_data, na.rm = TRUE), value = range(feature_data, na.rm = TRUE))
        })
      } else {
        output$secondary_filter_ui <- renderUI({
          checkboxGroupInput("secondary_feature_categories", "Select Secondary Categories:", choices = KT_dym_sort(unique(feature_data)), selected = KT_dym_sort(unique(feature_data)))
        })
      }
  })
  
  # Observe event to update tertiary filter UI based on selected feature
  observeEvent({
    input$tertiary_filter_feature
    input$Load_ave
  }, {
      req(input$tertiary_filter_feature)
      feature_data <- sampling()$df_sample[[input$tertiary_filter_feature]]
      
      if (is.numeric(feature_data)) {
        output$tertiary_filter_ui <- renderUI({
          sliderInput("tertiary_feature_range", "Tertiary Feature Range:", min = min(feature_data, na.rm = TRUE), max = max(feature_data, na.rm = TRUE), value = range(feature_data, na.rm = TRUE))
        })
      } else {
        output$tertiary_filter_ui <- renderUI({
          checkboxGroupInput("tertiary_feature_categories", "Select Tertiary Categories:", choices = KT_dym_sort(unique(feature_data)), selected = KT_dym_sort(unique(feature_data)))
        })
      }
  })
  
  # Reactive expression to calculate average results
  ave_results <- reactive({
      df_sample <- sampling()$df_sample
      rnd_factor <- rep(1:4, as.integer(nrow(df_sample) / 4) + 1)
      df_sample$rnd_factor <- head(rnd_factor, nrow(df_sample))
      
      ft_data <- df_sample[[input$ft]]
      factor_consistency_data <- df_sample[[input$factor_consistency]]
      
      # Apply filters based on feature ranges or categories
      if (is.numeric(df_sample[[input$filter_feature]])) {
        filtered_data <- df_sample[df_sample[[input$filter_feature]] >= input$feature_range[1] & df_sample[[input$filter_feature]] <= input$feature_range[2], ]
      } else {
        filtered_data <- df_sample[df_sample[[input$filter_feature]] %in% input$feature_categories, ]
      }
      
      if (is.numeric(df_sample[[input$secondary_filter_feature]])) {
        filtered_data <- filtered_data[filtered_data[[input$secondary_filter_feature]] >= input$secondary_feature_range[1] & filtered_data[[input$secondary_filter_feature]] <= input$secondary_feature_range[2], ]
      } else {
        filtered_data <- filtered_data[filtered_data[[input$secondary_filter_feature]] %in% input$secondary_feature_categories, ]
      }
      
      if (is.numeric(df_sample[[input$tertiary_filter_feature]])) {
        filtered_data <- filtered_data[filtered_data[[input$tertiary_filter_feature]] >= input$tertiary_feature_range[1] & filtered_data[[input$tertiary_filter_feature]] <= input$tertiary_feature_range[2], ]
      } else {
        filtered_data <- filtered_data[filtered_data[[input$tertiary_filter_feature]] %in% input$tertiary_feature_categories, ]
      }
      
      print(glue("test {input$feature_categories}"))
      print(c(input$filter_feature, input$secondary_filter_feature, input$tertiary_filter_feature))
      
      filtered_data$actual <- filtered_data[[config()$response]] * filtered_data[[config()$weight]]
      filtered_data$weight <- filtered_data[[config()$weight]]
      
      ft <- filtered_data[[input$ft]]
      band_ft <- if (length(unique(ft)) > 150) TRUE else input$band_ft
      
      suppressWarnings(calc_ave(ft = ft, 
                                actual = filtered_data$actual, 
                                pred = filtered_data$pred, 
                                weight = filtered_data$weight, 
                                factor_consistency = filtered_data[[input$factor_consistency]], 
                                rebase = input$rebase,
                                ft_name = input$ft,
                                band_ft = band_ft,
                                nbreaks = input$overlay_nbreaks,
                                band_method = input$glm_band_method))
  })
  
  # Reactive expression for cosmetic changes to the plot
  cosmetic <- reactive({
      cosmetic_changes(p = ave_results()$ave_plot,
                       alpha_pt = input$alpha_pt,
                       alpha_line = input$alpha_line,
                       size_pt = input$size_pt,
                       size_line = input$size_line,
                       fit_loess = input$fitloess,
                       smooth_strength = input$smooth_strength,
                       control_yaxis = input$y_lim, 
                       upper_lim = input$y_interval[2],
                       lower_lim = input$y_interval[1])
  })
  
  
  # Reactive expression for cosmetic changes to the plot
  cosmetic <- reactive({
      cosmetic_changes(p = ave_results()$ave_plot,
                       alpha_pt = input$alpha_pt,
                       alpha_line = input$alpha_line,
                       size_pt = input$size_pt,
                       size_line = input$size_line,
                       fit_loess = input$fitloess,
                       smooth_strength = input$smooth_strength,
                       control_yaxis = input$y_lim, 
                       upper_lim = input$y_interval[2],
                       lower_lim = input$y_interval[1])
  })
  
  # Render Plotly plot for average results
  output$avePlot <- renderPlotly({
      plot <- cosmetic()$ave_plot
      if (is.null(plot)) {
        return(NULL)
      }
      plot
  })
  
  # Download handler for average data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ave_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
        writexl::write_xlsx(list(ave_data = ave_results()$ave_df, smoothed_data = cosmetic()$smooth_data), file)
    })

  ################### Performance ##############################
  
  # Event reactive to calculate model performance
  Performance <- eventReactive(input$performance, {
      gc()
      print("Calc model performance")
      
      if (is.null(overlays())) {
        train_overlays_adj <- 1
        test_overlays_adj <- 1
      } else {
        train_overlays_adj <- overlays()$adj
        test_overlays_adj <- glm_spline_predict(glm_model_out(), test[test[[config()$weight]] > 0])
      }
      
      if (input$ignore_base_pred == FALSE) {
        kfold <- if (base_model()$train_kfold == TRUE) base_model()$train_kfold_val else 0
        early_stopping_rounds <- if (base_model()$train_use_early_stopping_rounds == TRUE) 5 else NULL
        
        print("run gbm stability test")
        
        gbm_train_pred_w_diff_seed <- train_model(
          fts = base_model()$model_output$model$feature_names,
          model = input$model,
          train = train[train[[config()$weight]] > 0] %>% select(c(base_model()$model_output$model$feature_names, config()$weight, config()$response)),
          kfold = kfold,
          train_validate_ratio = base_model()$train_Ratio,
          eta = base_model()$train_eta,
          max_depth = base_model()$train_max_depth,
          min_child_weight = base_model()$train_min_child_weight,
          subsample = base_model()$train_subsample,
          colsample_bytree = base_model()$train_colsample_bytree,
          lambda = base_model()$train_lambda,
          alpha = base_model()$train_alpha,
          nrounds = base_model()$train_nrounds,
          gamma = base_model()$train_gamma,
          parallel = TRUE,
          interaction_constraints = base_model()$model_output$model$params$interaction_constraints,
          monotone_constraints = base_model()$model_output$model$params$monotone_constraints,
          early_stopping_rounds = early_stopping_rounds,
          return_pred_only = TRUE,
          seed = 123
        )
        
        gbm_model <- base_model()$model_output$model
        gbm_train_pred <- base_model()$model_output$pred
        pred_diff <- gbm_train_pred_w_diff_seed / gbm_train_pred
        
        print("make test predictions")
        stability_hist <- ggplot(data.table(diff = pred_diff), aes(x = diff)) + geom_histogram(bins = 100) + theme_light(base_size = 18)
        
        lapply(seq(0.01,0.5,0.01), function(x) ifelse(abs(pred_diff -1 ) < x ,1,0  )) %>% 
          setNames(., as.character(seq(0.01,0.5,0.01))) %>% as.data.table() %>% summarise_all( list(mean)) %>% 
          melt -> stability_test 
        
        colnames(stability_test) <- c("variable" , "value")
        stability_test %>% rename(threshold = variable) %>% ggplot(.,aes(x = threshold, y= value , group = 1)) + geom_line() + geom_point() +
          theme_light(base_size = 18)+
          theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=0.9)) + ylab("Proportion of trained predictions matched") -> stability_threshold
        
        gbm_test_pred <- predict(gbm_model, newdata = as.matrix(test[test[[config()$weight]] > 0]%>% select(config()$selected_fts) %>% mltools::one_hot(.) %>% select(gbm_model$feature_names) ), type = "response")
        
        train_pred <- gbm_train_pred * train_overlays_adj
        test_pred <- gbm_test_pred * test_overlays_adj
      } else {
        train_pred <- train_overlays_adj
        test_pred <- test_overlays_adj
        stability_threshold <- ggplot() + theme_void()
        stability_hist <- ggplot() + theme_void()
        stability_test <- data.table(variable = seq(0.01, 0.5, 0.01), rep(1, length(seq(0.01, 0.5, 0.01))))
      }
      
      print("calc train performance")
      
      gini_train <- KT_resample_gini(
        n = input$n_resample,
        actual = config()$train_y * config()$train_weight,
        weight = config()$train_weight,
        predicted = train_pred,
        normalize = TRUE
      )
      
      lift_train <- KT_plot_lift(
        n = input$n_resample,
        pred = train_pred,
        actual = config()$train_y * config()$train_weight,
        weight = config()$train_weight,
        nbin = input$lift_plot_bin,
        title = "lift plot train"
      )$plot$lift_plot + theme_light(base_size = 18)
      
      print("calc test performance")
      
      gini_test <- KT_resample_gini(
        n = input$n_resample,
        actual = config()$test_y * config()$test_weight,
        weight = config()$test_weight,
        predicted = test_pred,
        normalize = TRUE
      )
      
      lift_test <- KT_plot_lift(
        n = input$n_resample,
        pred = test_pred,
        actual = config()$test_y * config()$test_weight,
        weight = config()$test_weight,
        nbin = input$lift_plot_bin,
        title = "lift plot test"
      )$plot$lift_plot + theme_light(base_size = 18)
      
      gini <- data.table(gini_train = gini_train, gini_test = gini_test) %>%
        melt() %>% ggplot(aes(x = value, group = variable, fill = variable)) + geom_density(alpha = 0.5) + theme_light(base_size = 18)
      
      validation_input <- list(
        test_pred = test_pred,
        stability_test = stability_test,
        test_weight = config()$test_weight,
        test_response = config()$test_y
      )
      
      saveRDS(validation_input, glue("{input$file_name_Training}_validation_input.rds"))
      
      return(list(
        gini = gini,
        lift_train = ggplotly(lift_train),
        lift_test = ggplotly(lift_test),
        stability_hist = stability_hist,
        stability_threshold = stability_threshold
      ))
  })
  
  # Render Gini plot
  output$gini <- renderPlot({
      Performance()$gini
  })
  
  # Render lift plot for training data
  output$lift_train <- renderPlotly({
      Performance()$lift_train
  })
  
  # Render lift plot for test data
  output$lift_test <- renderPlotly({
      Performance()$lift_test
  })
  
  # Render stability histogram
  output$stability1 <- renderPlot({
      Performance()$stability_hist
  })
  
  # Render stability threshold plot
  output$stability2 <- renderPlotly({
      ggplotly(Performance()$stability_threshold)
  })
  
  
  ################### Model Comparison ##############################
  # Event reactive to load model files
  load_model_file <- eventReactive(input$Run_comparison, {
      list(
        base = list(
          validation = readRDS(glue("{input$base_file}_validation_input.rds")),
          train_result = readRDS(glue("{input$base_file}.rds"))
        ),
        challenger = list(
          validation = readRDS(glue("{input$challenger_file}_validation_input.rds")),
          train_result = readRDS(glue("{input$challenger_file}.rds"))
        )
      )
  })
  
  # Observe and update select input for common features
  observe({
      req(load_model_file())
      
      same_fts <- intersect(load_model_file()$base$train_result$model_output$model$feature_names, 
                            load_model_file()$challenger$train_result$model_output$model$feature_names)
      updateSelectInput(session, "SHAP_common_ft", choices = same_fts)
  })
  
  # Observe and generate SHAP comparison plot
  observe({
      print("Running SHAP comparison")
      req(input$SHAP_common_ft)
      
      SHAP_comp_plot <- KT_plot_compare_shap(
        sv_base = load_model_file()$base$train_result$model_output$shap_values$main_effect$shap_main_effect[[input$SHAP_common_ft]],
        sv_challenger = load_model_file()$challenger$train_result$model_output$shap_values$main_effect$shap_main_effect[[input$SHAP_common_ft]],
        base_ft = load_model_file()$base$train_result$model_output$shap_values$main_effect$pred_data_main_effect[, input$SHAP_common_ft],
        challenger_ft = load_model_file()$challenger$train_result$model_output$shap_values$main_effect$pred_data_main_effect[, input$SHAP_common_ft],
        ft_name = input$SHAP_common_ft,
        loess_strength = input$SHAP_comp_smooth_strength
      )
      
      output$shap_model_comparison <- renderPlot({
        if (is.null(SHAP_comp_plot)) {
          showNotification("Plot generation failed. Please check the inputs and try again.", type = "error")
        } else {
          SHAP_comp_plot
        }
      })
  })
  
  # Observe and compare hyperparameters
  observe({
      print("Running SHAP comparison")
      req(load_model_file())
      
      base_hp <- load_model_file()$base$train_result[c("train_eta", "train_min_child_weight", "train_max_depth", "train_alpha", 
                                                       "train_lambda", "train_colsample_bytree", "train_subsample", "train_gamma", "train_nrounds")] %>% 
        as.data.table() %>% melt %>% mutate(scenario = "base")
      challenger_hp <- load_model_file()$challenger$train_result[c("train_eta", "train_min_child_weight", "train_max_depth", "train_alpha", 
                                                                   "train_lambda", "train_colsample_bytree", "train_subsample", "train_gamma", "train_nrounds")] %>% 
        as.data.table() %>% melt %>% mutate(scenario = "challenger")
      compare_hp <- rbind(base_hp, challenger_hp) %>% pivot_wider(names_from = "scenario", values_from = "value") %>% 
        mutate(diff = challenger / base)
      
      output$compare_hp <- renderDataTable({
        compare_hp
      })
      
      print("Running Importance comparison")
      compare_gain_imp <- KT_plot_compare_ft_imp(load_model_file()$base$train_result$model_output$imp_plot$imp_gain$data$Feature,
                                                 load_model_file()$challenger$train_result$model_output$imp_plot$imp_gain$data$Feature)
      compare_shap_imp <- KT_plot_compare_ft_imp(load_model_file()$base$train_result$model_output$imp_plot$imp_shap$data$variable,
                                                 load_model_file()$challenger$train_result$model_output$imp_plot$imp_shap$data$variable)
      
      output$SHAP_imp_comparison <- renderPlot({
        compare_shap_imp
      })
      output$gain_imp_comparison <- renderPlot({
        compare_gain_imp
      })
      
      print("Running stability comparison")
      compare_stability <- rbind(load_model_file()$base$validation$stability_test %>% mutate(scenario = "base"),
                                 load_model_file()$challenger$validation$stability_test %>% mutate(scenario = "challenger")) %>%
        ggplot(aes(x = variable, y = value, group = scenario, color = scenario)) + geom_line() + geom_point()
      
      output$stability_comparison <- renderPlot({
        compare_stability
      })
  })
  
  # Observe and run double lift comparison
  observe({
      print("Running double lift comparison")
      req(load_model_file())
      
      double_lift <- KT_plot_dl(
        n = input$dl_resample_size,
        actual = load_model_file()$base$validation$test_weight * load_model_file()$base$validation$test_response,
        weight = load_model_file()$base$validation$test_weight,
        base = load_model_file()$base$validation$test_pred,
        challenger = load_model_file()$challenger$validation$test_pred,
        nbin = input$nbin
      )$dl_rb_plot
      
      output$dl <- renderPlot({
        double_lift
      })
  })
  
  # Observe and run Gini comparison
  observe({
      print("Running gini comparison")
      req(load_model_file())
      
      compare_gini <- KT_plot_compare_gini(
        n = input$gini_resample_size,
        actual = load_model_file()$base$validation$test_weight * load_model_file()$base$validation$test_response,
        weight = load_model_file()$base$validation$test_weight,
        base = load_model_file()$base$validation$test_pred,
        challenger = load_model_file()$challenger$validation$test_pred,
        normalize = TRUE
      )
      
      output$gini_comparison <- renderPlot({
        compare_gini
      })
  })
  

    
}


shinyApp(ui = ui, server = server)

