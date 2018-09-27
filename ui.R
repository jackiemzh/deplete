library(shiny)
library(xlsx)
library(markdown)
library(DT)
#library(ReporteRs)
#library(shinydashboard)
library(shinythemes)
# library(shinyjs)

library(shinycssloaders)
#



## ui.R ##
shinyUI(fluidPage(theme = shinytheme("sandstone"),
                
                titlePanel("DEPLETE"),
                
                # display loading when busy
                tags$head(tags$style(type="text/css", "
                                     #loadmessage {
                                     position: fixed;
                                     top: 1px;
                                     left: 0px;
                                     width: 100%;
                                     padding: 5px 0px 5px 0px;
                                     text-align: center;
                                     font-weight: bold;
                                     font-size: 100%;
                                     color: #000000;
                                     background-color: #f9f4b3;
                                     z-index: 105;
                                     }")),
                navbarPage("Menu",
                           
                           #################### classic model display tab
                           tabPanel("Classic model",
                                    
                                    tabsetPanel(type = "tabs",
                                                
                                                id = "classic_panels",
                                                
                                                tabPanel(h5("Datasets"), icon = icon("table"),
                                                         
                                                         h4("Upload data from Excel.
                                                            When you uploded your data file, please"),
                                                         
                                                         actionLink("link_to_tabpanel_set_tab", h4("continue to Settings")),
                                                         
                                                         # Input: upload dataset ----
                                                         fileInput("upload_data_classic", 
                                                                   h4("Input data file"),
                                                                   accept = c(
                                                                     'text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv',
                                                                     '.xlsx')
                                                                  ),
                                                         
                                                         # Input: Checkbox if file has header ----
                                                         checkboxInput("header", h4("Check if file has headers"), TRUE),
                                                         
                                                         # Output: preview data ++++
                                                         wellPanel(h4("Preview dataset"),
                                                                   DT::dataTableOutput("data1")
                                                                   ),
                                                         
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Settings"), icon = icon("cog", lib = "glyphicon"), 
                                                         
                                                         value="setting-panel",
                                                         
                                                         h4("Modify your model settings."),
                                                         
                                                         h4("When you finish with the settings, please"),
                                                         
                                                         actionLink("link_to_tabpanel_ana_tab", h4("continue to Analysing")),
                                                         
                                                         # Output: data summary ++++
                                                         wellPanel(h4("Data summary"),
                                                                   tableOutput("data_summary")
                                                         ),
                                                         
                                                         # potential models output
                                                         wellPanel(h4("A list of models to be fitted."),
                                                                    tableOutput("all_model_table")
                                                         ),
                                                         
                                                         
                                                         
                                                         # Input: data column number ----
                                                         numericInput("data_colnum", 
                                                                      h4("Indicate data column number"), 
                                                                      value = 2),
                                                         
                                                         # Input: covariates available or not? ----
                                                         selectInput("cov_yn", 
                                                                     h4("Covariates available?"), c("No", "Yes")
                                                         ),
                                                         
                                                         
                                                         conditionalPanel( condition = "output.cov_yn_select",
                                                                           
                                                                           # Input: Specification of range within an interval ----
                                                                           sliderInput("cov_col_range", "Indicate covariate column numbers:",
                                                                                       min = 0, max = 20, # *** customise column range?
                                                                                       value = c(7,10)
                                                                           )
                                                                           
                                                                          #selectInput("cov_num", 
                                                                          #             h4("How many covariates considered?"), c("1", "2")
                                                                          # )
                                                                          
                                                                          # *** conditional options for classic model
                                                                           
                                                                           
                                                                          ),
                                                         
                                                         # reset inputs to default
                                                         
                                                         #uiOutput('resetable_input'),
                                                         #tags$hr(),
                                                  
                                                         actionButton("reset_input", "Reset to default",
                                                                      icon("refresh"), # (paper-plan, refresh)
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"
                                                                      ),
                                                         
                                                         tags$hr()
                                                         
                                                         # *** add more options in settings for classic model
                                                         
                                                         # submitButton("Apply")
                                                         ),
                                                
                                                tabPanel(h5("Analysing"),icon = icon("laptop", lib = "font-awesome"),
                                                         
                                                         #shinyjs::useShinyjs(), # from shinyjs library
                                                         value = "analysing-panel", # name this tab
                                                         
                                                         h4("Analyse your dataset."),
                                                         
                                                         h4("This may take several minutes depending on the size of your data, 
                                                            customised settings."),
                                                         
                                                         
                                                         h4("To avoid local maximum in the maximum likelihood estimation, we suggest run each model multiple times.
                                                             We suggest at least two maximum are found to make sure the algorithm is optimised.
                                                             Please specify the number of iterations used for each model (default is five). 
                                                             "),
                                                         
                                                         # updating potential models situation (done, running or waiting) while once hit the run button
                                                         
                                                         wellPanel(h4("Model summary"),
                                                                   
                                                                   tableOutput("all_ana_model_table")
                                                                   
                                                         ),
                                                         
                                                         wellPanel(h4("Check maximum log-likelihood outputs"),
                                                                   
                                                                   verbatimTextOutput("check_mle")
                                                         ),
                                                         
                                                         
                                                         # Test output
                                                          tableOutput("out2"),
                                                         
                                                         # Input: data column number ----

                                                         numericInput("num_iteration", 
                                                                      h4("Indicate the number of iterations"), 
                                                                      value = 5),
                                                         
                                                         # notification icon exlamation-circle
                                                         
                                                         actionButton("run_ana_button","Run analysis",
                                                                      icon = icon("chevron-circle-right", lib = "font-awesome"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                         
                                                         actionButton("reset_ana_button","Reset to default",
                                                                      icon = icon("refresh"),
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"),
                                                         
                                                         
                                                         h4("The dataset has been analysed, please"),
                                                         
                                                         actionLink("link_to_tabpanel_results_tab", h4("continue to Results")),
                                                         
                                                         tags$hr()
                                                        ),
                                                tabPanel(h5("Results"), icon = icon("stats", lib = "glyphicon"),
                                                         
                                                         
                                                         
                                                         # shinyjs::useShinyjs(), # from shinyjs library
                                                         value= "results-panel", # name this tab
                                                         
                                                         h4("Results are summarised."),
                                                         
                                                         # Classic model results tab ----
                                                         tabsetPanel(type = "pills",
                                                                     
                                                                     tabPanel("Plot results", icon = icon("bar-chart-o"),
                                                                              
                                                                              # Input: customise species name ----
                                                                              textInput("species_name",h6("Species name"),value=("individuals")),
                                                                              
                                                                              # Output: classic model plot ++++
                                                                              withSpinner(plotOutput("plot")),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPlot", "Download plot",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPredicted", "Download fitted dataset",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              
                                                                              tags$hr()
                                                                              
                                                                              ),
                                                                     
                                                                     tabPanel("Model_comparison", icon = icon("sort-by-attributes", lib = "glyphicon"),
                                                                              #icon = icon("list-alt"),
                                                                              
                                                                              # Output: classic model comparison ++++
                                                                              tableOutput("fit_table"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadModelCom", "Download results",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                              )
                                                                     
                                                                     #tabPanel("Estimates", icon = icon("th-list", lib = "font-awesome"),
                                                                              # "table"
                                                                              # Output: classic model estimates ++++
                                                                    #          tableOutput("estimates"),
                                                                              
                                                                              
                                                                              # Button
                                                                    #          downloadButton("downloadEstimates", "Download estiamtes",
                                                                    #                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                    #          tags$hr()
                                                                    #          )
                                                                     
                                                                     #tabPanel("Predicted data", 
                                                                              
                                                                              # Output: predicted values from a classic model ++++
                                                                    #          tableOutput("predicted_table")
                                                                              
                                                                              
                                                                     #         ) 
                                                                     )
                                                        )
                                                  ),      
                                     
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     tags$div("Loading...",id="loadmessage")
                                                    )   
                                    ),

                           #################### RD model display tab
                           tabPanel("Robust design model",
                                    
                                    tabsetPanel(type = "tabs",
                                                
                                                id = "RD_panels",
                                                
                                                tabPanel(h5("Datasets"), icon = icon("table"),
                                                         
                                                         h4("Upload data from Excel.
                                                            When you uploded your data file, please"),
                                                         
                                                         actionLink("link_to_tabpanel_set_tab_RD", h4("continue to Settings.")),
                                                         
                                                         # Input: upload dataset ----
                                                         fileInput("upload_data_RD", 
                                                                   h4("Input data file"),
                                                                   accept = c(
                                                                     'text/csv',
                                                                     'text/comma-separated-values,text/plain',
                                                                     '.csv',
                                                                     '.xlsx')
                                                         ),
                                                         
                                                         # Input: Checkbox if file has header ----
                                                         checkboxInput("header", h4("Check if file has headers"), TRUE),
                                                         
                                                         # Output: preview (RD) data ++++
                                                         wellPanel(h4("Preview dataset"),
                                                                   DT::dataTableOutput("data1_RD")
                                                         ),
                                                         
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Settings"), icon = icon("cog", lib = "glyphicon"), 
                                                         
                                                         value="setting_RD-panel",
                                                         
                                                         h4("Modify your model settings.
                                                            When you finish with the settings, please"),
                                                         
                                                         actionLink("link_to_tabpanel_ana_tab_RD", h4("continue to Analysing")),
                                                         
                                                         # Output: data summary ++++
                                                         wellPanel(h4("Data summary"),
                                                                   tableOutput("data_summary_RD")
                                                         ),
                                                         
                                                         
                                                         tableOutput("out3_RD"),
                                                         
                                                         # potential models output
                                                         wellPanel(h4("A list of models to be fitted."),
                                                                   tableOutput("all_model_table_RD")
                                                                   ),
                                                        
                                                         
                                                         fluidRow(
                                                           column(3,
                                                                  
                                                                  numericInput("data_colnum_RD", 
                                                                               h5("Indicate data column number"), 
                                                                               value = 7),
                                                                  
                                                                  # Input: does the dataset have equal no. of secondary samples? ----
                                                                  
                                                                  selectInput("equ_secondary_num_yn", 
                                                                              h5("Equal number of secondary samples withinn each primary period?"), c("","Yes", "No")
                                                                              ),
                                                                  
                                                                  conditionalPanel( condition = "output.equ_secondary_num_null",
                              
                                                                                    NULL
                                                                                    
                                                                                   ),
                                                                  
                                                                  conditionalPanel( condition = "output.equ_secondary_num_yes",
                                                                                    
                                                                                    # Input: give k_i if is equal across the study ----
                                                                                    
                                                                                    numericInput("no_secondary_occasion", 
                                                                                                 h6("  Indicate the number of secondary samples"), 
                                                                                                 value = 2)
                                                                                    
                                                                                   ),
                                                                  
                                                                  conditionalPanel( condition = "output.equ_secondary_num_no",
                                                                                    
                                                                                    # Input: give k_i index if not equal ----
                                                                                    
                                                                                    numericInput("RD_index", 
                                                                                                 h6("  Robust design index column number"), 
                                                                                                 value = 2)
                                                                                   )
                                                                                    
                                                                                    ## *** end of secondary sample conditional options for RD model
                                                                  ),
                                                                  
                                                                  
                                                                  
                                                           
                                                           column(4, offset = 1,
                                                                  
                                                                  # Input: covariates available or not? ----
                                                                  selectInput("cov_yn_RD", 
                                                                              h4("Covariates available?"), c("Yes", "No")
                                                                  ),
                                                                  
                                                                  conditionalPanel( condition = "output.cov_yn_select_RD",
                                                                                    
                                                                                    # Input: Specification of range within an interval ----
                                                                                    sliderInput("cov_col_range_RD", "Indicate covariate column numbers:",
                                                                                                min = 0, max = 20, # *** customise column range?
                                                                                                value = c(12,18)
                                                                                                )
                                                                                    
                                                                                    ## end of conditional options for RD model
                                                                                   )
                                                                  
                                                                  
                                                                  # *** add more options in settings for RD model
                                                                  
                                                                  # submitButton("Apply")
                                                                  
                                                                  ),
                                                           
                                                           column(4,
                                                                  
                                                                  # h4("options for p and phi?"),
                                                                  
                                                                  # Input: phi type ---- options conditional cov_yn
                                                                  selectInput("phi_type_RD", 
                                                                              h4("Define transition probability type"), 
                                                                              c("","All","Constant", "Covariates","Time-varying")
                                                                              ),
                                                                  
                                                                  # Input: p type  ----  options conditional cov_yn
                                                                  selectInput("p_type_RD", 
                                                                              h4("Define capture probability type"), 
                                                                              c("","All","Constant", "Covariates")
                                                                              )
                                                                  
                                                                  
                                                                   )
                                                           
                                                           # end of three column page
                                                         ),
                                                         
                                                         # reset inputs to default
                                                         
                                                         #uiOutput('resetable_input'),
                                                         #tags$hr(),
                                                         
                                                         
                                                         
                                                         actionButton("reset_input_RD", "Reset to default",
                                                                      icon("refresh"), # (paper-plan, refresh)
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"
                                                                      ),
                                                         
                                                        
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Analysing"),icon = icon("laptop", lib = "font-awesome"),
                                                         
                                                         #shinyjs::useShinyjs(), # from shinyjs library
                                                         value = "analysis_RD-panel", # name this tab
                                                         
                                                         h4("Analyse your dataset."),
                                                         
                                                         h4("This may take several minutes depending on the size of your data, 
                                                            customised settings."),
                                                         
                                                         
                                                         h4("To avoid local maximum in the maximum likelihood estimation, we suggest run each model multiple times.
                                                             We suggest at least two maximum are found to make sure the algorithm is optimised.
                                                             Please specify the number of iterations used for each model (default is five). 
                                                             "),
                                                         
                                                       
                                                         # updating potential models situation (done, running or waiting) while once hit the run button
                                                         
                                                         wellPanel(h4("Model summary"),
                                                                   
                                                                   tableOutput("all_ana_model_table_RD")
                                                                   ),
                                                                   
                                                         wellPanel(h4("Check maximum log-likelihood outputs"),
                                                                   
                                                                   # MLE outputs
                                                                   
                                                                   verbatimTextOutput("check_mle_RD")
                                                         ),
                                                                  
                                                        

                                                         # Input: (RD) data column number ----
                                                         
                                                         numericInput("num_iteration_RD", 
                                                                      h4("Indicate the number of iterations"), 
                                                                      value = 5),
                                                         
                                                         # notification icon exlamation-circle
                                                         
                                                         actionButton("run_ana_button_RD","Run analysis",
                                                                      icon = icon("chevron-circle-right", lib = "font-awesome"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                         
                                                         actionButton("reset_ana_button_RD","Reset to default",
                                                                      icon = icon("refresh"),
                                                                      style="color: #fff; background-color: #74aad8; border-color: #2e6da4"),
                                                         
                                                         hr(),
                                                         
                                                         
                                                         # current process
                                                         
                                                         h4("The dataset has been analysed, please"),
                                                         
                                                         actionLink("link_to_tabpanel_results_tab_RD", h4("continue to Results.")),
                                                         
                                                         tags$hr()
                                                         
                                                         ),
                                                
                                                tabPanel(h5("Results"), icon = icon("stats", lib = "glyphicon"),
                                                         
                                                         #shinyjs::useShinyjs(), # from shinyjs library
                                                         value= "results_RD-panel", # name this tab
                                                         
                                                         h4("Results are summarised."),
                                                         
                                                         # (RD) model results tab ----
                                                         tabsetPanel(type = "pills",
                                                                     
                                                                     tabPanel("Plot results", icon = icon("bar-chart-o"),
                                                                              
                                                                              # Input: customise species name ----
                                                                              textInput("species_name_RD",h6("Species name"),value=("individuals")),
                                                                              
                                                                              # Output: (RD) model plot ++++
                                                                              withSpinner(plotOutput("plot_RD")),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPlot_RD", "Download plot",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadPredicted_RD", "Download fitted dataset",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                              
                                                                     ),
                                                                     
                                                                     tabPanel("Model_comparison", icon = icon("sort-by-attributes", lib = "glyphicon"),
                                                                              #icon = icon("list-alt"),
                                                                              
                                                                              # Output: (RD) model comparison ++++
                                                                              tableOutput("fit_table_RD"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadModelCom_RD", "Download results",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                     ),
                                                                     
                                                                     tabPanel("Estimates", icon = icon("th-list", lib = "font-awesome"),
                                                                              # "table"
                                                                              # Output: (RD) model estimates ++++
                                                                              tableOutput("estimates_RD"),
                                                                              
                                                                              
                                                                              # Button
                                                                              downloadButton("downloadEstimates_RD", "Download estiamtes",
                                                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                              tags$hr()
                                                                     )
                                                                     
                                                                     #tabPanel("Predicted data", 
                                                                     
                                                                     # Output: predicted values from a RD model ++++
                                                                     #          tableOutput("predicted_table")
                                                                     
                                                                     
                                                                     #         ) 
                                                         )
                                                )
                                                ),      
                                    
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     tags$div("Loading...",id="loadmessage")
                                    )   
                           ),        
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           ################### Help and about us
                           navbarMenu("More",
                                      tabPanel("Help!",
                                               h3("Coming soon..")
                                               ),
                                      tabPanel("About us",
                                               h3("Add infom..")
                                               )
                                      ),
                           
                           # add navbarPage() options
                           fluid = TRUE
                            )
               # add fluidPage() options
  
  
  
))
