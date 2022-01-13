# hi
# hi again
##################################
#####         EasyMap         ####
#####                         ####
##### Created by: Ehud Dahan  ####
##### From Yassour Lab        ####
##### Contact me at:          #### 
##### ehud.dahan@mail.huji.ac.il #
##################################


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(graphics)
library(ggplot2)
library(ggpubr)
library(tidyverse) 
library(dplyr)
library(shinycssloaders)
# BiocManager::install("Maaslin2")
options(repos = BiocManager::repositories(version = "3.12"))
library(BiocManager)
library(Maaslin2)


if(1){
  try(setwd("~/GMAP/shiny/general/"), silent = T)
  try(setwd("/Volumes/ehudda/GMAP/shiny/general/"), silent = T)
  try(setwd("/vol/sci/bio/data/moran.yassour/lab/Projects/GMAP/shiny/general/"), silent = T)
}


ui <- dashboardPage(header = dashboardHeader(title = "EasyMap"                                             ), 
                    sidebar = dashboardSidebar({
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Upload Files", tabName = "upload", icon = icon("upload")),
                        menuItem("Run Analysis", tabName = "run", icon = icon("play")  
                        ),
                        menuItem("Heatmap", tabName = "heatmap", icon = icon("th")
                        ),
                        menuItem("About Us", icon = icon("users"), href = "https://www.yassourlab.com/")
                        
                      )
                    }), 
                    body = dashboardBody({
                      tabItems(
                        tabItem(tabName = "upload",
                                h2("Upload Tab"),
                                h4("Please choose data file. For brief view of the tool options one can use the example. just click below"),
                                fluidPage(fluidRow(
                                                 column(9, fileInput("file1", "Upload data here:",
                                                           multiple = FALSE,
                                                           accept = c(".tsv",
                                                                      ".csv"
                                                           )),
                                                        align = "left"),
                                                 column(2, tags$br(), 
                                                        uiOutput('example_file_ui'), align = "right")
                                   )),
                                      uiOutput('edit_file_ui'),
                                      uiOutput("table_view")
                        ),
                        tabItem(tabName = "run", 
                                h2("Run Analysis"),
                                h5("Here you can define the variables and the models"),
                                uiOutput('run_tab_ui')
                        ),
                        tabItem(tabName = "heatmap", h2("MaAsLin2 output results"),
                                h5("Click everywhere on the heatmap to get a box plot"),
                                box(withSpinner(uiOutput("heatmap_filters")),
                                    title = "Heatmap Filters",
                                    width = 2),
                                box(uiOutput('heatmap_ui'),
                                    textOutput(outputId = 'heatmapTextOutput'),
                                    width = 10),
                                box(uiOutput("plot_filters"),title = "Plot Filters",width = 2),
                                box(plotOutput('click_plot'), width = 10),
                                downloadButton(outputId = "download",label = "Download PDF", width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        )
                      )
                    }), 
                    skin = "purple")

server <- function(input, output, session) { 
  EXAMPLE_FILE <- "example_Files/GMAP_example_file.tsv"
  COL_PREVIEW <- 7  # number of column in file preview
  MAX_CATEGORICAL_VALUES <- 10
  MIN_NUMERICAL_VALUES <- 3
  EXCLUDE_TEXT <- "Exclude this variable"
  INCLUDE_TEXT <- "Include this variable"
  
  output$example_status <- renderText({h5("example")})
  react_values_list <- reactiveValues(num_y = -1,
                                      num_x = -1,
                                      p1 = -1, 
                                      p2 = -1,
                                      duplicated_columns = c(1),
                                      define_columns_approved = F,
                                      analysis_finish = F,
                                      fixed_vars = c(),
                                      continous_vars = c(),
                                      height_str = "500px",
                                      all_sets_are_valid = T
  )
  
  
  FromMaaslinInputToMaaslinOutput <- function(input_data = NULL,input_metadata = NULL,input_fixed_effects = NULL,input_random_effects = NULL,input_reference = NULL){
    fit_data <- Maaslin2::Maaslin2(input_data = input_data,
                                   input_metadata = input_metadata,
                                   output = "www",
                                   transform = "AST",
                                   plot_heatmap = F,
                                   plot_scatter = F,
                                   random_effects = input_random_effects,
                                   fixed_effects = input_fixed_effects,
                                   reference = input_reference
    )
    return(fit_data)
  }
  draw_heatmap <- function(data, qval_thr){
    ggplot(data, aes(y = feature, x = metadata, fill = qval)) +
      geom_tile() +
      facet_grid(~set,drop = T,
                 scales = "free",space = "free",
                 labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
      scale_fill_gradientn(colours = c("red", "grey", "white"),
                           breaks = c(0, qval_thr, 1),
                           limits = c(0, 1), #"#E2E5DE", "#B2BEB5"
                           values = c(0, qval_thr, 1)
      )+
      theme_linedraw()+
      theme(panel.grid.major = element_blank(), panel.background = element_rect(colour = "grey"),
            strip.placement = "outside",
            legend.position = "bottom",legend.justification = "right",
            axis.text.x = element_text(angle = 90, hjust = 1))
    
  }
  x_is_continous <- function(vec){
    if (is.numeric(vec)) {
      if (length(unique(vec)) > MAX_CATEGORICAL_VALUES){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  stat_table_from_maaslin_output <- function(data = NULL, maaslin_output = NULL,references = NULL,effect = NULL, feature = NULL, set = NULL, fixed_vars = NULL, continous_vars = NULL){
    stat_table <- NULL
    
    if (!is.null(effect) & effect %in% input$fixed){
      filter_maaslin_output <- maaslin_output[which(maaslin_output$feature == feature & 
                                                      maaslin_output$set == set &
                                                      maaslin_output$metadata == effect),]
      
      filter_maaslin_output$reference <- references[filter_maaslin_output$metadata]
      
      # stat_table <- filter_maaslin_output[,c("value", "reference", "qval")]
      # names(stat_table) <- c("group1","group2","qval")
      # stat_table$y.position <- max(data[[feature]]) + 0.1
      # stat_table$Qvalue <- paste0("Q=", round(stat_table$qval, 5))
      stat_table <- filter_maaslin_output[,c("value", "reference", "qval", "coef")]
      names(stat_table) <- c("group1","group2","qval", "coef")
      stat_table$y.position <- max(data[[feature]]) + 0.1
      stat_table$Qvalue <- paste0("Q=", round(stat_table$qval, 5), " C=", round(stat_table$coef, 5))
      
    } else{
      # take care of cases when x is continous
    }
    
    return(stat_table)
  }
  # draw_click_plot <- function(data = NULL, 
  #                             effect = NULL, 
  #                             feature = NULL, 
  #                             set = NULL, 
  #                             facet = "NA", 
  #                             color = "NA", 
  #                             label = "NA", 
  #                             stat_table = NULL, 
  #                             fixed_vars = NULL, 
  #                             continous_vars = NULL, 
  #                             transformation = "AST"){

  draw_click_plot_continous <- function(    data, 
                                            effect, 
                                            feature, 
                                            set, 
                                            facet, 
                                            color, 
                                            label, 
                                            stat_table, 
                                            fixed_vars, 
                                            continous_vars, 
                                            transformation = "AST"){
    
    g <- ggplot(data = data, aes_string(x = effect, y = feature)) +
        geom_smooth(method = "glm", se = F) +
        geom_point(alpha = 0.8, size = 2)


    if (color != "NA"){
      g2 <- g + 
        aes_string(color = color, fill = color) +
        geom_point(alpha = 0.8, size = 2, position = position_jitterdodge(jitter.width = 0.3, seed = 1))
    } else {
      g2 <- g + 
        aes_string(color = effect, fill = effect) +
        geom_point(alpha = 0.8, size = 2, position = position_jitterdodge(jitter.width = 0.3, seed = 1))
    }
    if(label != "NA"){
      g2 <- g2 + 
        geom_text(aes_string(label = label), position = position_jitterdodge(jitter.width = 0.3, seed = 1))
    }
    
      if (transformation == "AST") {
        g3 <- g2 +
          labs(title = as.character(set),
               y = paste0(feature, " (AST)")) +
          theme_classic()
      } else {
        g3 <- g2 +
          labs("title",
               y = paste0(feature, "")) +
          theme_classic()
      }

    
    if (facet != "NA" | is.null(stat_table)){
      g4 <- g3 + 
        facet_grid(~get(facet)) + stat_compare_means(method = "t.test", step.increase = max(data$feature)/10) +
        labs(subtitle = "P-Values calculated by T test, (MaAsLin2 results not found)")
    }
    if (facet == "NA"){ # & !is.null(stat_table)){
      g4 <- g3 + 
        labs(subtitle = "Q-Values calculated by MaAsLin2",
             caption = stat_table$Qvalue)
    }

    return(g4)
  }
  
      

  draw_click_plot <- function(data, 
                              effect, 
                              feature, 
                              set, 
                              facet, 
                              color, 
                              label, 
                              stat_table, 
                              fixed_vars, 
                              continous_vars, 
                              transformation = "AST"){
    
    if (effect %in% continous_vars){
      return(draw_click_plot_continous(data, 
                                              effect, 
                                              feature, 
                                              set, 
                                              facet, 
                                              color, 
                                              label, 
                                              stat_table, 
                                              fixed_vars, 
                                              continous_vars, 
                                              transformation = "AST"))}
    
    

    if (color == "NA"){
      g2 <- ggplot(data = data) + 
        aes_string(x = effect, y = feature, color = effect) +
        geom_boxplot(alpha = 0.8, outlier.size = 0, outlier.alpha = 0) +
      
        # aes_string(color = as.character(effect), fill = as.character(effect)) +
        geom_point(alpha = 0.8, size = 2, position = position_jitterdodge(jitter.width = 0.3, seed = 1))
    } else {
      g2 <- ggplot(data = data) + 
        aes_string(x = effect, y = feature, color = color) +
        geom_boxplot(alpha = 0.8, outlier.size = 0, outlier.alpha = 0) +
        # aes_string(color = color, fill = color) +
        geom_point(alpha = 0.8, size = 4, position = position_jitterdodge(jitter.width = 0.3, seed = 1))
    }
    if(label != "NA"){
        g2 <- g2 + 
          geom_text(aes_string(label = label), size = 2, position = position_jitterdodge(jitter.width = 0.3, seed = 1))
        }
    # if (label %in% names(data)){g2 <- g2 + geom_text(aes_string(label = label), position = position_jitterdodge(jitter.width = 0.8, seed = 1))}
    
    # if (facet != "NA"){g2 <- g2 + }
    
    
    if (transformation == "AST") {
      g3 <- g2 +
        labs(title = as.character(set),
             y = paste0(feature, " (AST)")) +
        theme_classic()
    } else {
      g3 <- g2 +
        labs("title",
             y = paste0(feature, "")) +
        theme_classic()
    }
    
    # Add stats
    # if (is.null(stat_table)){
    if (facet != "NA" | is.null(stat_table)){
      g4 <- g3 + 
        facet_grid(~get(facet)) + stat_compare_means(method = "t.test", step.increase = max(data$feature)/5) +
        labs(subtitle = "P-Values calculated by T test, (MaAsLin2 results not found)")
    }
    if (facet == "NA"){ # & !is.null(stat_table)){
      g4 <- g3 + 
        labs(subtitle = "Q-Values calculated by MaAsLin2")+
        stat_pvalue_manual(stat_table, label = "Qvalue",step.increase = 0.1)
    }
    # browser()
    
    return(g4)
  }
  
  PIXEL_PER_FEATURE <- 8
  START_HEIGHT <- 200
  output$heatmap_ui <- renderUI({
    return(plotOutput('heatmap',click = "click_on_heatmap",hover = "hover_on_heatmap", width = "100%", height = react_values_list$height_str))
  })
  
  observeEvent(input$update_heatmap, {
    react_values_list$height_str <- isolate(paste0(START_HEIGHT + (PIXEL_PER_FEATURE * length(input$features_filter)), "px"))
  })
  
  FromDataToPlotData <- function(maaslin_data = NULL, features_data = NULL, p1 = NULL, p2 = NULL, num_x = NULL, num_y = NULL, metadata = NULL){
    
    all_feature <- unique(as.character(maaslin_data[,"feature"]))
    all_features_ordered <-  all_feature %>%  factor() %>% levels()
    feature <- all_features_ordered[round(num_y)] %>% as.character()
    
    all_measured_effects <- unique(maaslin_data[,"metadata"])
    all_measured_ordered <-  sort(all_measured_effects,decreasing = F)
    effect <- all_measured_ordered[round(num_x)] %>% as.character()
    
    
    
    # filter features data by user filters
    plot_data <- features_data %>% select(c(feature, input$sampleID, input$random, input$fixed, input$fixed_numeric))
    
    # convert bacteria columns to AST
    plot_data$feature <- sign(plot_data[[feature]]) * asin(sqrt(abs(plot_data[[feature]])))
    
    res <- list(
      "plot_data" = plot_data,
      "maaslin_data" = maaslin_data,
      "panel_x" = p1,
      "panel_y" = p2,
      "feature" = feature,
      "effect" = effect)
    
    return(res)
    
  }
  
  num_of_sets <- reactiveValues(current = 1)
  exports_plots <- reactiveValues(heatmap_export_1 = ggplot(), plot_export_1 = ggplot(),plot_export_2 = ggplot(),plot_export_3 = ggplot())
  
  output$run_tab_ui <- renderUI({
    if (file_is_approved$approved){
      return(list(
        box(uiOutput("run_filters"), width = "100%", title = "Columns define"),
        uiOutput('define_sets_ui')
      ))
    } else {
      return(h5("Please upload file of your own or use our example"))
    }
  })
  
  observeEvent(input$define_columns_approved, {
    react_values_list$fixed_vars <- input$fixed
    react_values_list$continous_vars <- input$fixed_numeric
    
    output$run_filters <- renderUI({
      return(list(
        tagList(
        h5("In each category below choose all relevant variables in your data. note that only relevant columns will appear in the list, but still your responsibilty is to chooce an apropriate column to enable analysis work"),
        tags$h5("Sample ID:"), 
        tags$b(paste(input$sampleID, collapse = ", ")),
        tags$hr(),
        tags$b("Random effects:"), 
        tags$b(paste(input$random, collapse = ", ")),
        tags$hr(),
        tags$h5("Categorial potencialy fixed effects:"), 
        tags$b(paste(input$fixed, collapse = ", ")),
        tags$hr(),
        tags$h5("Continous potencialy fixed effects:"), 
        tags$b(paste(input$fixed_numeric, collapse = ", ")),
        tags$hr()),
        uiOutput('remain_variables_preview')
      ))
    })
    
    output$define_sets_ui <- renderUI({
      return(list(
        tags$hr(),
        uiOutput("references_1"),
        tags$hr(),
        actionButton(inputId = 'add_new_variables_set',label = "Add new variables set",width = "100%"),
        uiOutput('run_button_ui'),
        withSpinner(uiOutput("go_to_heatmap_ui"))
      ))
    })
  })
  
  
  output$run_button_ui <- renderUI({
    actionButton(inputId = 'run',label = HTML("<b>Run analysis</b>"),width = "100%",
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    # if (react_values_list$all_sets_are_valid == T){
    #   return(actionButton(inputId = 'run',label = HTML("<b>Run analysis</b>"),width = "100%",
    #                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    # } else{
    #   return(list(
    #     h5("Please include atlist 1 variable in each set"),
    #     actionButton(inputId = 'run',label = HTML("<b>Run analysis</b>"),width = "100%",
    #                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    #     ))
    #   
    # }
  })
  
  file_is_approved <- reactiveValues(approved = F)
  output$example_file_ui <- renderUI({
    actionButton(inputId = 'example_file',label = "Example", width = "100%")
  })
  output$file_checks <- renderUI({
    file_is_approved$approved <- F
    warning_list <- tagList(tags$b('We make a few file checks...'))
    
    # check if file not empty
    
    if (length(input$file1) != 0) {
      correct <- tags$p(icon("check"),  " Empty file check - success")
      warning_list <- tagList(warning_list, correct)
    } else{
      error <- tags$p(icon("remove"), " Your file is empty, please upload new file")
      warning_list <- tagList(warning_list, error)
      return(warning_list)
    }
    
    # check more than 2 columns
    if (!is.null(reactive_contents())) {
      correct <- tags$p(icon("check"), " File format is correct - success")
      warning_list <- tagList(warning_list, correct)
    } else{
      error <- tags$p(icon("remove"), " Please provide a correct format file")
      warning_list <- tagList(warning_list, error)
      return(warning_list)
    }
    
    if (ncol(reactive_contents()) > 2) {
      correct <- tags$p(icon("check"), paste0(as.character(ncol(reactive_contents())), " columns where identify in your file - succes. "))
      warning_list <- tagList(warning_list, correct)
    } else{
      error <- tags$p(icon("remove"), " Please upload file with more than 2 columns. Try change the 'seperator' or the 'Header' on the left panel. The head of your file is shown here:")
      warning_list <- tagList(warning_list, error)
      return(list(warning_list, uiOutput('uploaded_file_view')))
    }
    
    
    # check first column has unique names for all rows (samples)
    if (nrow(unique(reactive_contents()[1])) == nrow(reactive_contents())) {
      correct <- tags$p(icon("check"), " First column Check has success")
      warning_list <- tagList(warning_list, correct)
    } else{
      error <- tags$p(icon("remove"), " Please provide first column with unique name for each row (sample)")
      warning_list <- tagList(warning_list, error)
      return(warning_list)
    }
    
    
    # check column with 'mix' values
    warning_list <- tagList(warning_list, tags$b('Go over each column in your file:'))
    for (i in 1:ncol(reactive_contents())){
      empty_cells <- which(unique(reactive_contents()[[i]]) == "")
      if (length(unique(reactive_contents()[[i]])[empty_cells]) > 0){
        error <- tags$p(icon("remove"), paste0(" The column ",names(reactive_contents())[i]," has empty cells"))
        warning_list <- tagList(warning_list, error)
        return(warning_list)
      }
      correct <- tags$p(icon("check"), paste0(" Column ", names(reactive_contents())[i], " success"))
      warning_list <- tagList(warning_list, correct)
    }
    
    file_is_approved$approved <- T
    
    
    return(warning_list)
  })
  
  output$upload_file_button <- renderUI({
    succes_message <- HTML("Your file is approved, you can continue next step")
    continue_button <- actionButton(inputId = 'upload_file',label = HTML("<b>Submit</b>"),width = "100%",
                                    # icon("paper-plane"), 
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    
    if (file_is_approved$approved) {
      return(list(
        succes_message,
        continue_button
      ))
    } else {
    }
  })
  
  output$edit_file_ui <- renderUI({
    req(input$file1)
    if (file_is_approved$approved){
      uiOutput('upload_file_button')
    } else {
      return(list(
        HTML("Your file has some issues. Please check the file checks below. You can try fix it by change the header button or change the separator"),
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Tab = "\t",
                                 Comma = ",",
                                 Semicolon = ";"),
                     selected = "\t")
      ))
    }
  })
  
  observeEvent(input$upload_file, {
    updateTabsetPanel(inputId = "tabs",selected = "run")
  })
  observeEvent(input$go_heatmap_tab, {
    updateTabsetPanel(inputId = "tabs",selected = "heatmap")
  })
  
  observeEvent(input$stop_example_file, {
    output$example_file_ui <- renderUI({
      actionButton(inputId = 'example_file',label = "example", width = "100%")
    })
  })
  observeEvent(input$example_file, {
    output$example_message_upload_tab <- renderUI({h5("Running")})
    file_is_approved$approved <- T
    output$example_file_ui <- renderUI({
      actionButton(inputId = 'stop_example_file',label = " ", width = "40%", icon = icon("pause"))
    })
    
    output$edit_file_ui <- renderUI({
      succes_message <- HTML("Your file is approved, you can continue next step")
      continue_button <- actionButton(inputId = 'upload_file',label = HTML("<b>Submit</b>"),width = "100%",
                                      # icon("paper-plane"), 
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    
  })
  output$uploaded_file_view <- renderTable(head(reactive_contents()))
  output$example_file_preview <- renderTable(head(reactive_contents()))
  output$table_view <- renderUI({
    req(input$file1)
    if (length(input$file1) != 0){
      return(uiOutput('file_checks'))
    } else if (input$example_file) {
      uiOutput('example_file_preview')   
    }
    
  })
  
  output$contents <- renderDataTable(expr = {reactive_contents()[,1:min(COL_PREVIEW, ncol(reactive_contents()))]}
  )
  
  output$run_filters     <- renderUI({
    contentsHeader <- names(req(reactive_contents()))
    
    relevant_sample_ids_columns <- c()
    relevant_random_effects_columns <- c()
    relevant_categorical_effects_columns <- c()
    relevant_numerical_effects_columns <- c()
    
    for (col_name in contentsHeader) {
      if (length(unique(reactive_contents()[[col_name]])) == nrow(reactive_contents())){
        relevant_sample_ids_columns <- c(relevant_sample_ids_columns, col_name)
      }
      if (length(unique(reactive_contents()[[col_name]])) < nrow(reactive_contents())){
        relevant_random_effects_columns <- c(relevant_random_effects_columns, col_name)
      }
      if (length(unique(reactive_contents()[[col_name]])) <= MAX_CATEGORICAL_VALUES){
        relevant_categorical_effects_columns <- c(relevant_categorical_effects_columns, col_name)
      }
      if (length(unique(reactive_contents()[[col_name]])) >= MIN_NUMERICAL_VALUES & is.numeric(reactive_contents()[[col_name]])){
        relevant_numerical_effects_columns <- c(relevant_numerical_effects_columns, col_name)
      }
    }
    
    def_selected = list()
    def_selected$sampleID <- c()
    def_selected$random <- c()
    def_selected$fixed <- c()
    def_selected$fixed_numeric <- c()
    
    if (input$example_file){
      def_selected$sampleID <- c(1)
      def_selected$random <- c(1)
      def_selected$fixed <- c(1:5)
      def_selected$fixed_numeric <- c(2)
    }
    
    return(list(
      h5("In each category below choose all relevant variables in your data. note that only relevant columns will appear in the list, but it is still your responsibilty to choose an appropriate category for each column."),
      # sample ID
      pickerInput(inputId = "sampleID",
                  label = tagList(tags$b("Sample ID:"), tags$h5("Choose a column with unique identifier per row. (for example: sample_ID)")),
                  choices = relevant_sample_ids_columns,
                  selected = relevant_sample_ids_columns[def_selected$sampleID],
                  options = list(`actions-box` = TRUE)
      ),
      # random effects
      pickerInput(inputId = "random",
                  label = tagList(tags$b("Random effects:"), tags$h5("Remove the influence of random effects (e.g individual IDs in repeated measurments)")),
                  choices = relevant_random_effects_columns,
                  selected = relevant_random_effects_columns[def_selected$random],
                  options = list(`actions-box` = TRUE),
                  multiple = T
      ),
      # categorical Fixed effects
      pickerInput(inputId = "fixed",
                  label = tagList(tags$b("Categorial potentially fixed effects:"), 
                                  tags$h5("Predefining influencing parameters of data (environmental, inherent, clinical etc.)")),
                  choices = relevant_categorical_effects_columns,
                  selected = relevant_categorical_effects_columns[def_selected$fixed],
                  options = list(`actions-box` = TRUE),
                  multiple = T
      ),
      
      # numerical Fixed effects
      pickerInput(inputId = "fixed_numeric",
                  label = tagList(tags$b("Continous potentially fixed effects:"), 
                                  tags$h5("Choose variables that have continous values")),
                  choices = relevant_numerical_effects_columns,
                  selected = relevant_numerical_effects_columns[def_selected$fixed_numeric],
                  options = list(`actions-box` = TRUE),
                  multiple = T
      ),
      uiOutput('remain_variables_preview')
      
    ))
  })
  
  output$remain_variables_preview <- renderUI({
    all_input_choices <- c(input$sampleID, input$random, input$fixed, input$fixed_numeric)
    react_values_list$duplicated_columns <- c(all_input_choices[duplicated(all_input_choices)])
    
    if(length(react_values_list$duplicated_columns) > 0) {
      react_values_list$define_columns_approved <- F
      return(list(
        h5(icon("remove")," These columns "),
        tags$b(paste(react_values_list$duplicated_columns, collapse = ", ")),
        h5("fall under multiple categories. Please choose only one category for each column")
      ))
    }
    
    if(length(input$sampleID) == 0 | length(c(input$fixed)) == 0) {
      react_values_list$define_columns_approved <- F
      return(list(
        h5(icon("remove")," Please choose at list one column as fixed or continous effect")
      ))
    }
    
    
      react_values_list$define_columns_approved <- T
      return(list(
        h4("Remain variables:"),
        h5(paste(setdiff(names(reactive_contents()), unique(all_input_choices)), collapse = ", ")),
        actionButton(inputId = 'define_columns_approved', label = " Approved", icon = icon("check"), width = "100%",
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
      )
    
  })
  
  output$heatmap_filters <- renderUI({
    if (react_values_list$analysis_finish){
      variables_sets <- sapply(1:num_of_sets$current, function(i) input[[paste0("set_name_#", i)]])
      features <- sort(unique(reactive_maaslin_output()[["feature"]]))
      return(list(
        # Variables Set
        pickerInput(inputId = "vars_set_filter",
                    label = "Choose variable sets:",
                    choices = variables_sets,
                    selected = variables_sets,
                    options = list(`actions-box` = TRUE),
                    multiple = T
        ),
        pickerInput(inputId = "features_filter",
                    label = "Features:",
                    choices = features,
                    selected = features,
                    options = list(`actions-box` = TRUE),
                    multiple = T
        ),
        numericInput(inputId = "qval_thr", label = "Q value threshold",min = 0, max = 1, step = 0.001, width = "100%", value = 0.2),
        actionButton(inputId = 'update_heatmap',label = HTML("<b>update</b>"),width = "100%")
      ))
    } else {
      return(h5("Waiting for finish analysis step"))
    }
  })
  output$references_1 <- renderUI({
    list_reference_1 <- create_reference_ui(num_set = 1)
    return(list(list_reference_1))
  })
  # output$maaslin_text_output <- renderText(reactive_maaslin_stdout())
  plot_export_reactive <- reactive({
    heatmap <- reactive_heatmap()
    plot <- reactive_update_plot()
    
    # heatmap <- isolate(reactive_heatmap())
    # plot <- isolate(reactive_update_plot())
    return(ggarrange(heatmap, plot, ncol = 1))
  })
  
  PLOT_HEIGHT_PDF <- 8
  INIT_HEATMAP_HEIGHT_PDF <- 5
  FEATURE_HEATMAP_PIXELS <- 0.2
  output$download <- downloadHandler(filename = function(){paste0(gsub(pattern = " |:", replacement = "-", Sys.time()), "-output.pdf")},content = function(file){
    ggsave(filename = file, 
           device = "pdf",
           width = 14,
           height = 21,
           plot = plot_export_reactive())
  })
  output$download_maaslin_output <- downloadHandler(filename = "maaslin2.log", content = function(file){file.copy("www/maaslin2.log", file)})
  
  output$plot_filters <- renderUI({
    req(input$run)
    return(list(
      pickerInput(inputId = "plot_color",
                  label = "Color by",
                  choices = c(NA, input$fixed),
                  selected = NA,
                  options = list(`actions-box` = TRUE),
                  multiple = F
      ),
      pickerInput(inputId = "plot_facet",
                  label = "Facet by",
                  choices = c(NA, input$fixed),
                  selected = NA,
                  options = list(`actions-box` = TRUE),
                  multiple = F
      ),
      pickerInput(inputId = "plot_label",
                  label = "Label by",
                  choices = c(NA, input$random),
                  selected = NA,
                  options = list(`actions-box` = TRUE),
                  multiple = F
      ), 
      actionButton(inputId = "update_plot",
                   label = "Update Plot"
      )
      
    ))
  })
  
  
  reactive_contents <- reactive({
    # return the file from the user
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if (input$example_file){
      # if (input$example_file){
      df <- read.delim(EXAMPLE_FILE,
                       header = T,
                       as.is = T,
                       sep = "\t",
                       stringsAsFactors = T)
      
    } else {
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.delim(input$file1$datapath,
                           header = input$header,
                           as.is = T,
                           sep = input$sep,
                           stringsAsFactors = T,
                           # quote = input$quote,
          )
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
    }
    
    # if (input$abundance_transpose){ df <- t(df)}
    
    if (!is.null(input$file2)){
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          metadata <- read.delim(input$file2$datapath,
                                 header = input$header,
                                 as.is = T,
                                 sep = input$sep,
                                 stringsAsFactors = T,
                                 # quote = input$quote,
          )
          
          df <- left_join(metadata, df)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
    }
    # req(input$file1)
    
    
    return(df)
  })
  reactive_maaslin_input <- reactive({
    req(reactive_contents(), 
        input$sampleID,
        input$fixed,
        # input$fixed_numeric,
        # input$random, 
        input$run)
    
    
    contents <- reactive_contents()
    sampleID <- input$sampleID
    
    inputs_list <- list()
    
    
    for (num_set in 1:num_of_sets$current) {
      
      input_fixed_effects_in_curr_set <- c()
      chosen_references_vars <- c()
      for (i in 1:length(input$fixed)) {
        if (input[[paste0("set_",num_set,"_","var_", i,"_include")]] == T){
          
        # }
        # if(input[[paste0("set_",num_set,"_","var_", i)]] != EXCLUDE_TEXT){
          # determine which variables were included in this set
          input_fixed_effects_in_curr_set <- c(input_fixed_effects_in_curr_set, input$fixed[i])
          
          #determine the reference vector according to variables included in data set
          # create a pattern: "metadata_var_A;reference_for_A,metadata_var_B;reference_for_B". real example: "case_id;AP Case,symptoms;Control"
          chosen_references_vars <- c(chosen_references_vars, input[[paste0("set_",num_set,"_","var_", i)]])
        }
      }
      
      for (i in 1:length(input$fixed_numeric)) {
        if (input[[paste0("set_",num_set,"_","num_var_", i,"_include")]] == T){
          
        # }
        # if(input[[paste0("set_",num_set,"_","con_var_", i)]] != EXCLUDE_TEXT){
          # determine which variables were included in this set
          input_fixed_effects_in_curr_set <- c(input_fixed_effects_in_curr_set, input$fixed_numeric[i])
          
          #determine the reference vector according to variables included in data set
          # create a pattern: "metadata_var_A;reference_for_A,metadata_var_B;reference_for_B". real example: "case_id;AP Case,symptoms;Control"
          # chosen_references_vars <- c(chosen_references_vars, input[[paste0("set_",num_set,"_","var_", i)]])
        }
      }
      
      
      input_reference_in_curr_set <- paste0(input_fixed_effects_in_curr_set,";",chosen_references_vars, collapse = ",")
      
      # Define random effects
      input_random_effects <- c()
      if (length(unique(reactive_contents()[,input$random])) < nrow(reactive_contents())) {
        input_random_effects <- input$random
      }
      
      # input_metadata
      input_metadata <- reactive_contents()[,c(sampleID, input_fixed_effects_in_curr_set, input_random_effects)] %>% column_to_rownames(sampleID)
      
      # input_data
      input_data_columns <- setdiff(names(reactive_contents()), c(input$fixed, input$fixed_numeric, input$random))
      input_data <- reactive_contents()[,input_data_columns] %>% column_to_rownames(sampleID)
      
      
      inputs_list_to_maaslin <- list(
        "input_data" = input_data, 
        "input_metadata" = input_metadata, 
        "input_fixed_effects" = input_fixed_effects_in_curr_set, 
        "input_random_effects" = input_random_effects, 
        "input_reference" = input_reference_in_curr_set)
      # message(inputs_list_to_maaslin)
      inputs_list[[num_set]] <- inputs_list_to_maaslin
      
    }
    
    return(inputs_list)
    
  })
  # reactive_maaslin_stdout <- reactive(paste(tail(readLines("www/maaslin2.log")), sep = "<br/>"))
  reactive_maaslin_output <- reactive({
    req(reactive_maaslin_input())
    inputs_list <- reactive_maaslin_input()
    
    tab <- data.frame()
    # go over new sets and run maaslin for each of them, then add the results to 'tab'
    
    for (num_set in 1:num_of_sets$current) {
      
      fit_data_from_one_set <- FromMaaslinInputToMaaslinOutput(
        input_data = inputs_list[[num_set]]$input_data,
        input_metadata = inputs_list[[num_set]]$input_metadata,
        input_random_effects = inputs_list[[num_set]]$input_random_effects,
        input_fixed_effects = inputs_list[[num_set]]$input_fixed_effects,
        input_reference = inputs_list[[num_set]]$input_reference
      )
      
      
      fit_data_from_one_set[["results"]][["set"]] <- input[[paste0("set_name_#", num_set)]]  # Add the set name in the 'set' column
      
      if(num_set == 1){
        tab <- fit_data_from_one_set[["results"]]
      } else{
        tab <- rbind(tab, fit_data_from_one_set[["results"]]) 
      }
    }
    
    
    return(tab)
    
  })
  reactive_data <- reactive({
    req(reactive_maaslin_output(),
        input$vars_set_filter,
        input$features_filter)
    
    reactive_maaslin_output() %>%
      filter(set %in% input$vars_set_filter) %>%
      filter(feature %in% input$features_filter) # %>% 
      # filter(qval < input$qval_thr)
  })
  reactive_heatmap <-  reactive({
    draw_heatmap(reactive_data(), input$qval_thr)
  })
  
  reactive_hover_text <- reactive({
    # message("line 649")
    FromDataToPlotData(
      maaslin_data = reactive_data(),
      features_data = reactive_contents(),
      p1 = input$hover_on_heatmap$panelvar1,
      p2 = input$hover_on_heatmap$panelvar2 ,
      num_x = input$hover_on_heatmap$x,
      num_y = input$hover_on_heatmap$y
      
    )
  })
  
  reactive_references_list_by_chosen_set <- reactive({
    # return vector of the refeences variable and the medadata in the names
    # metadata: case_id   Diet
    # variable: No AP   Formula
    req(reactive_data(), 
        reactive_contents(),
        input$click_on_heatmap)
    
    
    curr_set <- react_values_list$p1
    input_fixed_effects_in_curr_set <- c()
    chosen_references_vars <- c()
    
    
    for (num_set in 1:num_of_sets$current) {
      if (input[[paste0("set_name_#", num_set)]] == curr_set){
        for (i in 1:length(input$fixed)) {
          if(input[[paste0("set_",num_set,"_","var_", i)]] != EXCLUDE_TEXT){
            # determine which variables were included in this set
            input_fixed_effects_in_curr_set <- c(input_fixed_effects_in_curr_set, input$fixed[i])
            
            #determine the reference vector according to variables included in data set
            # create a pattern: "metadata_var_A;reference_for_A,metadata_var_B;reference_for_B". real example: "case_id;AP Case,symptoms;Control"
            chosen_references_vars <- c(chosen_references_vars, input[[paste0("set_",num_set,"_","var_", i)]])
          }
        }
        
      }
      
      
      names(chosen_references_vars) <- input_fixed_effects_in_curr_set
    }
    return(chosen_references_vars)
  })
  reactive_plot_data_list <- reactive({
    return(FromDataToPlotData(maaslin_data = reactive_data(),
                              features_data = reactive_contents(),
                              p1 = react_values_list$p1,
                              p2 = react_values_list$p2,
                              num_x = react_values_list$num_x,
                              num_y = react_values_list$num_y
    ))
  })
  reactive_update_plot <- reactive({
    plot_data_list <- reactive_plot_data_list()
    
    # stat_table <- NULL
    if(input$plot_facet == "NA"){
      # browser()
      stat_table <- stat_table_from_maaslin_output(data = plot_data_list$plot_data,
                                                   maaslin_output = reactive_maaslin_output(),
                                                   references = reactive_references_list_by_chosen_set(),
                                                   effect = plot_data_list$effect, 
                                                   feature = plot_data_list$feature, 
                                                   set = plot_data_list$panel_x,
                                                   fixed_vars = react_values_list$fixed_vars,
                                                   continous_vars = react_values_list$continous_vars)
    } else{
      stat_table <- NULL
    }
    
    
    
    gg <- draw_click_plot(data = plot_data_list$plot_data,
                          effect = plot_data_list$effect, 
                          feature = plot_data_list$feature, 
                          set = plot_data_list$panel_x, 
                          facet = input$plot_facet,
                          color = input$plot_color,
                          label = input$plot_label,
                          stat_table = stat_table,
                          fixed_vars = react_values_list$fixed_vars, 
                          continous_vars = react_values_list$continous_vars,
                          transformation = "AST")  # ADDD INPUT BUTTON TO CHOOSE TRANFORMATION HERE
    return(gg)
    
  })
  reactive_click_plot <- reactive({
    
    plot_data_list <- reactive_plot_data_list()
    stat_table <- stat_table_from_maaslin_output(data = plot_data_list$plot_data,
                                                 maaslin_output = reactive_maaslin_output(),
                                                 references = reactive_references_list_by_chosen_set(),
                                                 effect = plot_data_list$effect, 
                                                 feature = plot_data_list$feature, 
                                                 set = plot_data_list$panel_x)
    
    
    
    # browser()
    draw_click_plot(data = plot_data_list$plot_data,
                    effect = plot_data_list$effect , 
                    feature = plot_data_list$feature, 
                    set = plot_data_list$panel_x, 
                    facet = input$plot_facet,
                    color = input$plot_color,
                    label = input$plot_label,
                    stat_table = stat_table,
                    fixed_vars = react_values_list$fixed_vars, 
                    continous_vars = react_values_list$continous_vars, 
                    transformation = "AST")
  })
  
  output$go_to_heatmap_ui <-renderUI({h5("")})
  observeEvent(input$add_new_variables_set, {
    num_of_sets$current <- num_of_sets$current + 1
    insertUI(
      selector = "#add_new_variables_set",
      where    = "beforeBegin",
      ui       =  create_reference_ui(num_set = num_of_sets$current))
  })
  
  observe({
    req(input[[paste0("set_name_#",num_of_sets$current)]])
    react_values_list$all_sets_are_valid <- T
    for (num_set in 1:num_of_sets$current) {
      input_fixed_effects_in_curr_set <- c()
      for (i in 1:length(input$fixed)) {
        if (input[[paste0("set_",num_set,"_","var_", i,"_include")]] == T){
          input_fixed_effects_in_curr_set <- c(input_fixed_effects_in_curr_set, input$fixed[i])
        }
      }
      for (i in 1:length(input$fixed_numeric)) {
        if (input[[paste0("set_",num_set,"_","num_var_", i,"_include")]] == T){
          input_fixed_effects_in_curr_set <- c(input_fixed_effects_in_curr_set, input$fixed_numeric[i])
        }
      }
      if (length(input_fixed_effects_in_curr_set) == 0){
        react_values_list$all_sets_are_valid <- F 
        break
      }
    }
    
    if (!react_values_list$all_sets_are_valid){
      sendSweetAlert(
        session = getDefaultReactiveDomain(),
        title = "Note",
        text = "Please include atlist 1 variable in the model",
        type = "warning",
        btn_labels = "brown",
        # btn_colors = "#3085d6",
        html = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
      )
    }
    
  })
  
  
  create_reference_ui <- function(num_set){
    
    if (num_set == 1){
      included_fixed <- sapply(seq_along(input$fixed), function(i){T})
      references_fixed <- sapply(seq_along(input$fixed), function(i){input$fixed[i]})
      included_fixed_numeric <- sapply(seq_along(input$fixed), function(i){T})
# 
#       included_fixed <- c(input$fixed)
#       references_fixed <- rep_along(included_fixed, T)
#       included_fixed_numeric <- c(input$fixed_numeric)
    } else{
      included_fixed <- sapply(seq_along(input$fixed), function(i){
        ifelse(input[[paste0("set_",num_set - 1,"_","var_", i,"_include")]]==T, T, F)
      })
      references_fixed <- sapply(seq_along(input$fixed), function(i){
        input[[paste0("set_",num_set - 1,"_","var_", i)]]
      })
      included_fixed_numeric <- sapply(seq_along(input$fixed), function(i){
        ifelse(input[[paste0("set_",num_set - 1,"_","num_var_", i,"_include")]]==T, T, F)
      })
    }
    
    
    model_name <- textInput(inputId = paste0("set_name_#", num_set), 
                            label = paste0("Model name:"), 
                            value = paste0("#", num_set), 
                            width = NULL, placeholder = NULL)
    
    
    # included_variables_checkbox <-  checkboxGroupInput(inputId = paste0("set_", num_set, "_vars"),
    #                        label = "Included these variables in model:",
    #                        choices = all_vars)
    
    # browser()
    fixed_button <- lapply(seq_along(input$fixed), function(i){
      return(list(
        h5(input$fixed[i]),
      checkboxInput(inputId = paste0("set_",num_set,"_","var_", i,"_include"),
                    label = "Include", value = ifelse(included_fixed[i]==T, T, F), width = "20%"),
      pickerInput(inputId = paste0("set_",num_set,"_","var_", i), 
                  label = "reference",
                  # choices = c(EXCLUDE_TEXT, unique(as.character(reactive_contents()[[input$fixed[i]]]))),
                  choices = unique(as.character(reactive_contents()[[input$fixed[i]]])),
                  selected = references_fixed[i],
                  options = list(`actions-box` = TRUE),
                  multiple = F,
                  width = 'fit')
      ))})
    
    fixed_numeric_button <- lapply(seq_along(input$fixed_numeric), function(i){
      checkboxInput(inputId = paste0("set_",num_set,"_","num_var_", i,"_include"),
                    label = input$fixed_numeric[i], value = ifelse(included_fixed_numeric[i]==T, T, F), width = "20%")
      # pickerInput(inputId = paste0("set_",num_set,"_","con_var_", i), # When i add the option for few model - chnage "1" to model number
      #             label = paste0(input$fixed_numeric[i], ":"),
      #             choices = c(EXCLUDE_TEXT, INCLUDE_TEXT),
      #             selected = INCLUDE_TEXT,
      #             options = list(`actions-box` = TRUE),
      #             multiple = F)
      })

    return(box(list(
      model_name,
      # included_variables_checkbox,
      # HTML(paste0(input[[paste0("set_", num_set, "_vars")]], collapse = ", ")),
      # h5("Categorical variables:"),
      fixed_button,
      h5("For the next numerical or continuos variables it is not possible to choose reference. decide if you want to include them:"),
      fixed_numeric_button #,
      # actionButton(inputId = paste0("set_", num_set, "_","OK"), label = "OK", icon = icon("check"), width = "100%")
    ), width = "100%"))
  }  
  
  
  observeEvent(input$run, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Your job request accepted !!",
      text = "Analysis would take a few minutes...",
      type = "success",
      btn_labels = "OK",
      btn_colors = "green",
      html = FALSE,
      closeOnClickOutside = TRUE,
      showCloseButton = FALSE,
      width = NULL
    )
      output$go_to_heatmap_ui <-renderUI({
        req(reactive_maaslin_output())
        box(downloadButton(outputId = "download_maaslin_output",label = "Download log file", width = "30%"), 
            # sweet_alert,
            actionButton(inputId = 'go_heatmap_tab',label = HTML("<b>Go next</b>"),width = "70%",
                         # icon("paper-plane"), 
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            width = "100%")
      })
      reactive_maaslin_output() 
      react_values_list$analysis_finish <- T
      # output$botton_download_maaslin_output <- renderUI(downloadButton(outputId = "download_maaslin_output",label = "Download log file"))
    
  })
  output$heatmap <- renderPlot({
    if (react_values_list$analysis_finish){
      isolate(reactive_heatmap())  
    }
  })
  observeEvent(input$update_heatmap, {
    req(reactive_maaslin_output())
    
    output$heatmap <- renderPlot(isolate(reactive_heatmap()))
    
    output$ui_export <- renderUI({
      return(list(
        actionButton(inputId = 'add_heatmap_1',label = "Add heatmap here",width = "50%"),
        plotOutput("heatmap_export_1")
      ))
    })
    
  })
  observeEvent(input$hover_on_heatmap, {
    res <- reactive_hover_text()
    output$heatmapTextOutput <- renderText({
      return(HTML(c("feature = ",res$feature,
                    " | Measured effect =", res$effect,
                    " | Panel
                    = ", res$panel_x)))})
    feature_df <- res$maaslin_data[which(res$maaslin_data$feature == res$feature & 
                                           res$maaslin_data$set == res$panel_x &
                                           res$maaslin_data$metadata == res$effect),]
    
    # Add lines to heatmap to sign the mouse location
    output$heatmap <- renderPlot(isolate(reactive_heatmap() + 
                                           # labs(caption = "try") + 
                                           geom_tile(data=feature_df, 
                                                     inherit.aes = T,
                                                     color="blue",
                                                     size = 2)
    ))
  })
  observeEvent(input$click_on_heatmap, {
    
    react_values_list$p1 <- input$click_on_heatmap$panelvar1
    react_values_list$p2 <- input$click_on_heatmap$panelvar2
    react_values_list$num_x <- input$click_on_heatmap$x
    react_values_list$num_y <- input$click_on_heatmap$y
    
    
    # output$click_plot <- renderPlot(isolate(reactive_click_plot()))
    output$click_plot <- renderPlot(isolate(reactive_update_plot()))
    features_values <- reactive_plot_data_list()$plot_data[[1]]
    # output$ui_plot_export <- renderUI({
    #   return(list(
    #     actionButton(inputId = 'add_plot_1',label = "Add plot here",width = "50%"),
    #     plotOutput("plot_export_1"),
    #     actionButton(inputId = 'add_plot_2',label = "Add plot here",width = "50%"),
    #     plotOutput("plot_export_2"),
    #     actionButton(inputId = 'add_plot_3',label = "Add plot here",width = "50%"),
    #     plotOutput("plot_export_3")
    #   ))
    # })
    
  })
  observeEvent(input$update_plot, {
    output$click_plot <- renderPlot(isolate(reactive_update_plot()))
    exports_plots[["heatmap_export_1"]] <- isolate(reactive_heatmap())
    exports_plots[["plot_export_1"]] <- isolate(reactive_update_plot())
  })
  
  # observeEvent(input$add_heatmap_1, {
  #   req(input$update_heatmap)
  #   # insertUI(selector = "#add_heatmap_1", where = "afterEnd", ui = list(plotOutput(outputId = "heatmap_export_1")))
  #   output[["heatmap_export_1"]] <- renderPlot(isolate(reactive_heatmap()))
  #   exports_plots[["heatmap_export_1"]] <- isolate(reactive_heatmap())
  # })
  # observeEvent(input$add_plot_1, {
  #   output[["plot_export_1"]] <- renderPlot(isolate(reactive_update_plot()))
  #   exports_plots[["plot_export_1"]] <- isolate(reactive_update_plot())
  # })
}

shinyApp(ui, server)
