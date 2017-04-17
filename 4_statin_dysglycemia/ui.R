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
                              "Analysis Cohort - Males",
                              "Analysis Cohort - Females",
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
                                "Change in Log LDL",
                                "Diabetes Development",
                                "Survival"),
                    selected = "Diabetes Development"
        ),
        
        # Select Input -> Select Predictor Variable
        selectInput(inputId = "predictor",
                    label = "Select Predictor Variable",
                    choices = NULL
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
        tableOutput(outputId = "test_results")
      )
      
    )
  )
)
