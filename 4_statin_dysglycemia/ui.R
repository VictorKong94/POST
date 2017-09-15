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
                  choices = c("Case Cohort",
                              "Case Cohort - Females",
                              "Case Cohort - Lovastatin (Exclusive)",
                              "Case Cohort - Males",
                              "Case Cohort - Simvastatin (Exclusive)",
                              "Control Cohort")
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
                    choices = c("Change in Median HDL",
                                "Change in Median LDL",
                                "Change in Median Total Cholesterol",
                                "Change in Median Triglycerides",
                                "Diabetes Development",
                                "Max-Delta Change in FG",
                                "Maximum Post-Statin Triglycerides",
                                "Percent Change in Median LDL"),
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
        tableOutput(outputId = "demographics")
      ),
      
      # Show Test Results
      conditionalPanel(
        condition = "input.show == 'Test Results'",
        tableOutput(outputId = "test_results")
      )
      
    )
  )
)
