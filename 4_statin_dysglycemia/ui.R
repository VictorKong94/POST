library(shiny)
library(shinythemes)

fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  titlePanel(paste("Examination of Statin Effects on Dysglycemia",
                   "in a Large Population Based Cohort")),
  sidebarLayout(
    sidebarPanel(
      
      # Select Input -> Cohort
      selectInput(inputId = "cohort",
                  label = "Select Cohort",
                  choices = c("Analysis Cohort",
                              "Changed Statin Type Cohort",
                              "No BMIs Cohort",
                              "No Change in Statin Type Cohort")
      ),
      
      # Radio Buttons -> See 'Demographics' or 'Test Results'?
      radioButtons(inputId = "show",
                   label = "What would you like to see?",
                   choices = c("Demographics",
                               "Test Results")
      ),
      
      # If the user wants Test Results, need more information
      conditionalPanel(
        condition = "input.show == 'Test Results'",
        
        # Select Input -> Select Response Variable
        selectInput(inputId = "response",
                    label = "Select Response Variable",
                    choices = c("Change in FG",
                                "Change in LDL",
                                "Diabetes Development",
                                "Survival"),
                    selected = "Diabetes Development"
        ),
        
        # Select Input -> Select Predictor Variable
        selectInput(inputId = "predictor",
                    label = "Select Predictor Variable",
                    choices = c("Pre-statin FG",
                                "Change in FG",
                                "Pre-statin BMI",
                                "Change in BMI",
                                "Sex",
                                "Age",
                                "Race",
                                "Hispanic",
                                "Main Statin Used",
                                "Changed Statin Type",
                                "PDD/DDD",
                                "Pre-statin LDL",
                                "Change in LDL",
                                "Met LDL<100 Goal",
                                "TSH Level")
        ),
        
        # Numeric Input -> Number of Tests in Data Table
        numericInput(inputId = "nTest",
                     label = "Number of Tests",
                     value = 1,
                     min = 1
        ),
        
        # UI Output -> Appropriate Number of Selectize Inputs for Covariates
        uiOutput(outputId = "covariates")
        
      ),
      
      # Download Button -> Download Tables Displayed
      downloadButton(outputId = "download",
                     label = "Download Results",
                     class = "center")
      
    ),
    mainPanel(
      
      # Show Demographics
      conditionalPanel(
        condition = "input.show == 'Demographics'",
        tabsetPanel(type = "tabs",
                    tabPanel("Complete",
                             tableOutput(outputId = "complete_demographics")),
                    tabPanel("Hispanic, not Multiracial",
                             tableOutput(outputId = "hispanic_not_multiracial"))
        )
      ),
      
      # Show Test Results
      conditionalPanel(
        condition = "input.show == 'Test Results'",
        
        # Use tabs if response is diabetes development
        conditionalPanel(
          condition = "input.response == 'Diabetes Development'",
          tabsetPanel(type = "tabs",
                      tabPanel("FG>126",
                               tableOutput(outputId = "diabetes_fg")),
                      tabPanel("ICD",
                               tableOutput(outputId = "diabetes_icd")),
                      tabPanel("Either",
                               tableOutput(outputId = "diabetes_either"))
          )
        ),
        
        # If not, use the standard table
        conditionalPanel(
          condition = "input.response != 'Diabetes Development'",
          tableOutput(outputId = "test_results")
        )
        
      )
    )
  )
)