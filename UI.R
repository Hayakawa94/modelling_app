library(shiny)
library(rhandsontable)
library(plotly)
library(DT)
library(DiagrammeR)
ui <- fluidPage(useShinyjs(),
  tags$head(
    tags$style(HTML("
      .full-width-btn {
        width: 100%;
      }
      .global-inputs {
        display: flex;
        align-items: center;
        background-color: #f0f0f0;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .global-inputs > div {
        margin-right: 20px;
      }
      .red-btn {
        background-color: green;
        color: white;
      }
      .tune-btn-container {
        display: flex;
        justify-content: flex-end;
        margin-bottom: 20px;
      }
      .sidebar {
        width: 300px !important;
        flex-shrink: 0 !important;
      }

    "))
  ),
  
  titlePanel("GBM Modelling"),
  
  # Global inputs with background effect and horizontal alignment
  div(class = "global-inputs",
      div(selectInput("model", "Select Model", choices = names(model_spec) , selected = selected_model)),
      div(checkboxInput("Distribute_Computation", "Distribute Computation", value = TRUE)),
      div(sliderInput("n_core", "n core", min = 1,max = detectCores(),value = max(floor(parallel::detectCores()*2/3),1),step = 1))
  ),
  
  tabsetPanel(
    tabPanel("Feature Summary",
             sidebarLayout(
               sidebarPanel(
                 textInput("file_name_feature", "File Name", value = "feature_spec"),
                 actionButton("load_feature", "Load Feature Spec", class = "full-width-btn"),
                 actionButton("save_feature", "Save Feature Spec", class = "full-width-btn"),
                 actionButton("reset_ft_selection", "reset feature selection", class = "full-width-btn")
               ),
               mainPanel(
                 # div(class = "tune-btn-container",
                 #     actionButton("summarise_data", "summarise_data", class = "full-width-btn red-btn")
                 # ),
                 tabsetPanel(
                   tabPanel("Feature_Selection", rHandsontableOutput("ft_table")),
                   tabPanel("Data", DT::dataTableOutput("dt_sum")),
                   tabPanel("Response", DT::dataTableOutput("Claim")),
                   tabPanel("Correlation" , uiOutput("corr_topn_slider"),plotlyOutput("corr_plot", width = "1000", height = "1000") )
                 )
               )
             )
    ),
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("eda_ft", "Feature (ft):", choices = c("none"), selected = "none"),
                 selectInput("eda_interaction", "Interaction:", choices = c("none"), selected = "none"),
                 tags$hr(),
                 sliderInput("ft_nbreaks", "Feature Breaks:", min = 5, max = 100, value = 30, step = 1),
                 sliderInput("interaction_nbreaks", "Interaction Breaks:", min = 2, max = 100, value = 30, step = 1),
                 tags$hr(),
                 selectInput("ft_band_type", "Feature Band Type:", choices = c("equal", "quantile"), selected = "equal"),
                 selectInput("interaction_band_type", "Interaction Band Type:", choices = c("equal", "quantile"), selected = "quantile"),
                 tags$hr(),
                 
                
                 selectInput("filter1", "Filter 1:", choices = c("none"), selected = "none"),
                 uiOutput("filter1_ui"),
                 actionButton("select_all_filter1", "Select All Filter 1"),
                 actionButton("clear_filter1", "Clear Filter 1"),
                 selectInput("filter2", "Filter 2:", choices = c("none"), selected = "none"),
                 uiOutput("filter2_ui"),
                 actionButton("select_all_filter2", "Select All Filter 2"),
                 actionButton("clear_filter2", "Clear Filter 2"),
                 selectInput("filter3", "Filter 3:", choices = c("none") , selected = "none"),
                 uiOutput("filter3_ui"),
                 actionButton("select_all_filter3", "Select All Filter 3"),
                 actionButton("clear_filter3", "Clear Filter 3")
                 
                 
    
 
               ),
               mainPanel( 
                 div(class = "tune-btn-container",
                     actionButton("refresh_eda", "refresh chart", class = "full-width-btn red-btn")
                 ),
                 fluidRow(
                 column(2, sliderInput("bar_alpha", "Bar Alpha:", min = 0, max = 1, value = 0.1, step = 0.1)),
                 column(2, sliderInput("lwd", "Line Width:", min = 0.5, max = 5, value = 0.65, step = 0.01)),
                 column(2, sliderInput("point_size", "Point Size:", min = 0.5, max = 5, value = 1.5, step = 0.01)),
                 column(2, sliderInput("line_alpha", "Line Alpha:", min = 0, max = 1, value = 0.6, step = 0.1)),
                 column(2, sliderInput("point_alpha", "Point Alpha:", min = 0, max = 1, value = 1, step = 0.1))
                 
               ),
               fluidRow(
                 column(2, checkboxInput("eda_fit_loess", "fit loess", value = TRUE)),
                 column(2, sliderInput("eda_smooth_strength", "smooth strength:", min = 0, max = 1, value = 1, step = 0.1))
               ),
               plotlyOutput("edaPlot", height = "800px")
               )
             )
    ),
    tabPanel("Boruta Feature selection",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_boruta", "Load Result"),
                 # actionButton("save_boruta", "Save Result"),
                 textInput("file_name_boruta", "File Name", value = "boruta_out"),
                 
                 actionButton("Boruta_Select_Params", "Select Params", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.Boruta_Select_Params % 2 == 1",
                   sliderInput("Boruta_max_run", "Max Run):", min = 30, max = 1000, value =50, step = 1),
                   sliderInput("Boruta_eta", "Learning Rate (eta):", min = 0.001, max = 0.3, value = 0.1, step = 0.001),
                   sliderInput("Boruta_max_depth", "Max Depth:", min = 1, max = 15, value =  5, step = 1),
                   sliderInput("Boruta_nrounds", "Number of Rounds:", min = 10, max = 2000, value = 100, step = 1)
                 ), 
                 hidden(actionButton("Update_ft_spec", "Update Feature_spec", class = "full-width-btn")) ,
                 hidden(sliderInput("boruta_top_selected_fts", "Select top n fts",min = 1,max = 100,step = 1 , value = 10)) 
               ),
               mainPanel(
                 div(class = "tune-btn-container",
                     actionButton("Boruta_run", "Boruta run", class = "full-width-btn red-btn")
                 ),
                 tabsetPanel(
                   tabPanel("Boruta imp", plotOutput("Boruta_imp", width = "1000", height = "1000")),
                   tabPanel("Boruta shap imp", 
                            plotOutput("Boruta_shap_imp", width = "1000", height = "1000")),
                                  )
             )
             )
    ),
    tabPanel("Tuning",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_tuning", "Load Tuning"),
                 # actionButton("save_tuning", "Save Tuning"),
                 textInput("file_name_tuning", "File Name", value = "tuning"),
                 tags$hr(),
                 actionButton("Sampling", "Sampling", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.Sampling % 2 == 1",
                   checkboxInput("Trainvalidate", "Train Validate Split", value = TRUE),
                   sliderInput("Ratio", "Ratio:", min = 0.5, max = 1, value = 0.8, step = 0.001),
                   checkboxInput("kfold", "kfold Cross Validate", value = FALSE),
                   sliderInput("kfold_val", "Select number of folds:", min = 2, max = 10, value = 5, step = 1)
                 ),
                 tags$hr(),
                 actionButton("Select_HP_bounds", "HP Bounds", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.Select_HP_bounds % 2 == 1",
                   sliderInput("eta", "Learning Rate (eta):", min = 0.001, max = 0.3, value = c(0.001, 0.1), step = 0.001),
                   sliderInput("min_child_weight", "Minimum Child Weight:", min = 0.0001, max = 0.1, value = c(0.0001, 0.01), step = 0.0001),
                   sliderInput("max_depth", "Max Depth:", min = 1, max = 15, value = c(1, 5), step = 1),
                   sliderInput("alpha", "alpha (L1 regularization):", min = 0, max = 1, value = c(0.001, 0.2), step = 0.001),
                   sliderInput("lambda", "lambda (L2 regularization):", min = 0, max = 10, value = c(0.001, 2), step = 0.01),
                   sliderInput("colsample_bytree", "colsample bytree:", min = 0.1, max = 1, value = c(0.2, 0.4), step = 0.01),
                   sliderInput("subsample", "Subsample:", min = 0.1, max = 1, value = c(0.2, 0.4), step = 0.01),
                   sliderInput("gamma", "Min Split Loss:", min = 0, max = 100, value = c(0, 0), step = 0.001),
                   sliderInput("nrounds", "Number of Rounds:", min = 10, max = 2000, value = 100, step = 1)
                 )
               ),
               mainPanel(
                 div(class = "tune-btn-container",
                     actionButton("tune", "Tune Model", class = "full-width-btn red-btn")
                 ),
                 tabsetPanel(
                   tabPanel("tune_iteration", plotlyOutput("tune_iteration_plot")),
                   tabPanel("eta", plotlyOutput("eta_plot")),
                   tabPanel("max_depth", plotlyOutput("max_depth_plot")),
                   tabPanel("min_child_weight", plotlyOutput("min_child_weight_plot")),
                   tabPanel("colsample_bytree", plotlyOutput("colsample_bytree_plot")),
                   tabPanel("Subsample", plotlyOutput("subsample_plot")),
                   tabPanel("lambda", plotlyOutput("lambda_plot")),
                   tabPanel("alpha", plotlyOutput("alpha_plot")),
                   tabPanel("Min Split Loss", plotlyOutput("gamma_plot")),
                   tabPanel("Tune_result", DT::dataTableOutput("opt_result_plot"))
                 ),
                 textOutput("action_message_tuning")
               )
             )
    ),
    tabPanel("Training",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_Training", "Load Experiment"),
                 # actionButton("save_Training", "Save Training"),
                 textInput("file_name_Training", "File Name", value = "Training"),
                 tags$hr(),
                 actionButton("load_tuning_best_param", "Load Tuned HPs"),
                 # textInput("file_name_tuned", "File Name", value = "tuning"),
                 tags$hr(),
                 actionButton("train_Sampling", "Sampling", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.train_Sampling % 2 == 1",
                   checkboxInput("train_Trainvalidate", "Train Validate Split", value = TRUE),
                   sliderInput("train_Ratio", "Ratio:", min = 0.5, max = 1, value = 0.8, step = 0.001),
                   checkboxInput("train_kfold", "kfold Cross Validate", value = FALSE),
                   sliderInput("train_kfold_val", "Select number of folds:", min = 2, max = 10, value = 5, step = 1)
                 ),
                 tags$hr(),
                 actionButton("train_Select_HP", "HPs", class = "full-width-btn"),
                 checkboxInput("use_early_stopping_rounds", "use early stopping rounds", value = F),
                 conditionalPanel(
                   condition = "input.train_Select_HP % 2 == 1",
                   
                   sliderInput("train_eta", "Learning Rate (eta):", min = 0.001, max = 0.3, value = 0.1, step = 0.001),
                   sliderInput("train_min_child_weight", "Minimum Child Weight:", min = 0.0001, max = 0.1, value =  0.01, step = 0.0001),
                   sliderInput("train_max_depth", "Max Depth:", min = 1, max = 15, value =  5, step = 1),
                   sliderInput("train_alpha", "alpha (L1 regularization):", min = 0, max = 1, value =  0.2, step = 0.001),
                   sliderInput("train_lambda", "lambda (L2 regularization):", min = 0, max = 10, value =  2, step = 0.01),
                   sliderInput("train_colsample_bytree", "colsample bytree:", min = 0.1, max = 1, value =  0.75, step = 0.01),
                   sliderInput("train_subsample", "Subsample:", min = 0.3, max = 1, value =  1, step = 0.01),
                   sliderInput("train_gamma", "Min Split Loss:", min = 0, max = 100, value = 0, step = 0.001),
                   sliderInput("train_nrounds", "Number of Rounds:", min = 10, max = 2000, value = 100, step = 1)
                 )
               ),
               mainPanel(
                 div(class = "tune-btn-container",
                     actionButton("train", "Train Model", class = "full-width-btn red-btn")
                 ),
                 tabsetPanel(
                   tabPanel("SHAP imp vs Gain imp", plotOutput("imp_comparison",width = "1000", height = "1000")),
                   tabPanel("Feature Importance (SHAPley value contribution)", plotlyOutput("SHAP_imp_plot")),
                   tabPanel("SHAP_Interaction_matrix", 
                            sliderInput("top_shap_X", "Top X", min = 1, max = 40, value = 8, step = 1),
                            plotlyOutput("SHAP_Interaction_matrix", width = "1000", height = "1000")),
                   tabPanel("Feature Importance (Gain)", plotlyOutput("Gain_imp_plot")),
                   
                   tabPanel("gain",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            plotOutput("gain", width = "1000", height = "1000")
                   ),
                   tabPanel("gain2",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            selectInput("y_axis", "y axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") ,selected =  "sumCover"),
                            selectInput("x_axis", "x axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") , selected = "meanGain" ),
                            plotOutput("gain2", width = "1000", height = "1000")
                   ),
                   
                   tabPanel("Interaction_matrix", 
                            sliderInput("top_gain_X", "Top X", min = 1, max = 40, value = 8, step = 1),
                            plotlyOutput("Interaction_matrix", width = "1000", height = "1000")),

                   
                   tabPanel("Interaction_gain",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            plotOutput("Interaction_gain", width = "1000", height = "1000")
                   ),
                   tabPanel("Interaction_gain2",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            selectInput("y_axis", "y axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") ,selected =  "sumCover"),
                            selectInput("x_axis", "x axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") , selected = "meanGain" ),
                            plotOutput("Interaction_gain2", width = "1000", height = "1000")
                   ),
                  
                   tabPanel("Tree Plot", sliderInput("tree_index", "Tree Index", min = 0, max = 0, value = 0, step = 1) , 
                            grVizOutput("tree_plot", width = "100%", height = "900"))
                 ),
                 textOutput("action_message_training")
               )
             )
    ),
    tabPanel("Explain",
             sidebarLayout(
               sidebarPanel(
                 # actionButton("load_trained_model", "Load model"),
                 # textInput("file_name_SHAP", "File Name", value = "Training"),
                 # tags$hr(),
                 sliderInput("SHAP_sample_size", "Sample Size", min = 0.001, max = 1, value = 1, step = .0001),
                 sliderInput("SHAP_smooth_strength", "Smooth Strength", min = 0, max = 1, value = 0.9, step = .001),
                 sliderInput("SHAP_pt_size", "Point Size", min = 0, max = 4, value = 1, step = .0001),
                 sliderInput("SHAP_alpha", "Alpha", min = 0, max = 1, value = 1, step = .0001),
                 
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("SHAP", 
                            # div(class = "tune-btn-container",
                            #     actionButton("Update_SHAP_chart", "Update Chart", class = "full-width-btn red-btn")
                            # ),
                            # checkboxInput("Fit_loess", "Fit Loess", value = T),
                            selectInput("SHAP_ft", "Choose Feature", choices = NULL),
                            plotOutput("SHAP_plot")),
                   
                   tabPanel("SHAP X", 
                            # div(class = "tune-btn-container",
                            #     actionButton("Update_SHAP_X_chart", "Update Chart", class = "full-width-btn red-btn")
                            # ),
                            checkboxInput("SHAP_X_Fit_loess", "Fit Loess", value = F),
                            selectInput("SHAP_X_ft1", "Choose Feature", choices = NULL),
                            selectInput("SHAP_X_ft2", "Choose Interaction", choices = NULL),
                            plotOutput("SHAP_X_plot"))
                 ),
                 textOutput("Shap_value_loaded")
               )
             )
    ),
    tabPanel("Overlays",
             sidebarLayout(
               sidebarPanel(
                 
                 # textInput("Base_pred_path", "Base Model", value = "Training"),
                 # tags$hr(),
                 actionButton("save_glm", "Save Splines"),
                 actionButton("load_glm", "Load Splines"),
                 textInput("glm_overlay_out", "glm_overlay_out_path", value = "glm_overlays"),
                 tags$hr(),
                 checkboxInput("draw_mode", "Enable Drawing Mode", value = FALSE),
                 actionButton("reset", "Reset Drawing"),
                 tags$hr(),
                 selectInput("ft", "Select Feature", choices = sort(fts)),
                 hidden(selectInput("factor_consistency", "Select Interaction:", choices = fts)) ,
                 checkboxGroupInput("undo_shapes", "Select Spline Undo", choices = NULL),
                 actionButton("undo", "Undo"),
                 tags$hr(),
                 checkboxInput("band_ft", "band feature" , value = T),
                 selectInput("glm_band_method" ,"band_method",   choices = c("equal" , "quantile" ) , selected ="equal" ),
                 sliderInput("overlay_nbreaks" , "nbreaks",min = 4,max = 100, value = 10,step = 1),
                 
                 checkboxInput("ignore_base_pred", "ignore_base_pred" , value = F),
                 actionButton("lookup_pmml_export", "Export_lookup_table")

               ),
               mainPanel(
                 fluidRow(
                   column(8,  # Adjust the width as needed
                          div(class = "tune-btn-container",
                              actionButton("Fit", "Fit", class = "full-width-btn red-btn")
                          ),
                          tabsetPanel(
                            tabPanel("GLM fit",
                                     tableOutput("glm_fit"),
                                     checkboxGroupInput("fit_lines", "",
                                                        choices = c("CA_base", "CA_challenger", "obs", "CU_base", "CU_challenger" , "CM"),
                                                        selected = c("CA_challenger", "obs", "CU_challenger", "CM"),
                                                        inline = TRUE
                                     ),
                                     checkboxInput("show_splines", "Show Splines", value = TRUE),
                                     plotlyOutput("overlay_plot"),
                                     plotlyOutput("avePlot")  # Placing avePlot below overlay_plot
                                     
                            ),
                            tabPanel("Model Summary",
                                     verbatimTextOutput("glm_summary")
                            )
                          )
                   ),
                   column(4,  # Adjust the width as needed
                          actionButton("Load_ave", "Load AvE"),
                          sliderInput("samplesize", "Sample Size:", value = 1, min = 0.01, max = 1),
                          checkboxInput("rebase", "Rebase:", value = TRUE),
                          selectInput("filter_feature", "Select Feature to Filter:", choices = NULL),
                          uiOutput("filter_ui"),
                          selectInput("secondary_filter_feature", "Select Secondary Feature to Filter:", choices = NULL),
                          uiOutput("secondary_filter_ui"),
                          selectInput("tertiary_filter_feature", "Select tertiary Feature to Filter:", choices = NULL),
                          uiOutput("tertiary_filter_ui"),
                          
                          checkboxInput("fitloess", "Fit LOESS", value = FALSE),
                          sliderInput("smooth_strength", "Smooth Strength:", value = 0.75, min = 0, max = 1),
                          actionButton("Chart_Cosmetic","AvE analysis",  class = "full-width-btn"),
                          conditionalPanel(condition = "input.Chart_Cosmetic % 2 == 1",
                                           
                                           sliderInput("alpha_line", "AVE line alpha:", value = 0.3, min = 0, max = 1, step = 0.001),
                                           sliderInput("alpha_pt", "AVE point alpha:", value = 1, min = 0, max = 1, step = 0.001),
                                           sliderInput("size_line", "AVE line size:", value = 0.6, min = 0.001, max = 4, step = 0.01),
                                           sliderInput("size_pt", "AVE point size:", value = 1.5, min = 0.001, max = 3.5, step = 0.001),
                                           checkboxInput("y_lim", "Control y lim", value = TRUE),
                                           sliderInput("y_interval", "y axis lim:", value = c(0, 2), min = 0, max = 5, step = 0.0001),
                                           downloadButton("overlayfit_download", "Download Data")
                          )
                   )
                 ),
                 tableOutput("aveTable")
               )
             )
    ),
    tabPanel("Model Performance",
             sidebarLayout(
               sidebarPanel(
                 actionButton("performance", "Run_performance", class = "full-width-btn"),
                 sliderInput("n_resample" , "resample n "  , value = 10  ,min = 5 , max = 200 , step = 1),
                 sliderInput("lift_plot_bin" , "bin "  , value = 10  ,min = 5 , max = 500 , step = 1)),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Lift", plotlyOutput("lift_train"),plotlyOutput("lift_test")),
                   tabPanel("Gini", plotOutput("gini")),
                   tabPanel("Stability", plotOutput("stability1"),
                            plotlyOutput("stability2"))
                 )
               )
             )
    ),
    tabPanel("Model Comparison",
             sidebarLayout(
               sidebarPanel(textInput("base_file", "Base File Name", value = "Training"),
                            textInput("challenger_file", "Challenger File Name", value = "Training"),
                 actionButton("Run_comparison", "Run_comparison", class = "full-width-btn")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("SHAP trend", 
                            selectInput("SHAP_common_ft", "Select feature:", choices = NULL),
                            sliderInput("SHAP_comp_smooth_strength", "Smooth Strength:", value = 0.75, min = 0, max = 1),
                            plotOutput("shap_model_comparison")),
                   tabPanel("Hyperparameters", DT::dataTableOutput("compare_hp")),
                   tabPanel("SHAP importance", plotOutput("SHAP_imp_comparison",width = "1000", height = "1000")),
                   tabPanel("Gain importance", plotOutput("gain_imp_comparison",width = "1000", height = "1000")),
                   tabPanel("Stability", plotOutput("stability_comparison")),
                   tabPanel("Gini", 
                            sliderInput("gini_resample_size", "Resample size", value = 10, min = 5, max = 120, step = 1),
                            plotOutput("gini_comparison")),
                   tabPanel("double lift", 
                            sliderInput("dl_resample_size", "Resample size", value = 20, min = 15, max = 120, step = 1),
                            sliderInput("nbin", "nbin", value = 20, min = 10, max = 120, step = 1),
                            plotOutput("dl"))
                   
                 )
               )
             )
    )
  )
)

options(shiny.reactlog = TRUE)
library(reactlog)