library(shiny)

source("helpers.R")

function(input, output, session) {
  
  ###############
  # DATA IMPORT #
  ###############
  
  df = reactive({
    csv = switch(input$cohort,
                 "Analysis Cohort" = "data/analysis_cohort.csv",
                 "Changed Statin Type Cohort" = "data/delta_type.csv",
                 "No BMIs Cohort" = "data/no_bmi.csv",
                 "No Change in Statin Type Cohort" = "data/no_delta_type.csv")
    csv = read.csv(csv)
    csv$sex = relevel(csv$sex, ref = "F")
    csv$race = relevel(csv$race, ref = "WH")
    csv$statin_type = relevel(csv$statin_type, ref = "Simva")
    csv
  })
  
  
  #######################
  # DEMOGRAPHICS TABLES #
  #######################
  
  # Compose demographics table: entire population
  complete_demographics = reactive({
    demographics(df())
  })
  
  # Compose demographics table: identify as Hispanic but not Multiracial
  hispanic_not_multiracial = reactive({
    demographics(subset(df(), hispanic == "Y" & race != "MU"))
  })
  
  # Render the tables in Shiny
  output$complete_demographics = renderTable(complete_demographics())
  output$hispanic_not_multiracial = renderTable(hispanic_not_multiracial())
  
  
  #####################################
  # INPUTS FOR ADDITIONAL INFORMATION #
  #####################################
  
  # Get the desired number of tests to perform
  nTest = reactive({
    as.integer(input$nTest)
  })
  
  # We don't want the same variable for a predictor as our response
  predictor_choices = reactive({
    setdiff(c("Pre-statin FG",
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
              "TSH Level"),
            input$response)
  })
  
  # Update Select Input for predictor choices
  observe({
    updateSelectInput(session,
                      inputId = "predictor",
                      choices = predictor_choices())
  })
  
  # We don't want the same variable for a covariate as our predictor
  covariate_choices = reactive({
    setdiff(predictor_choices(), input$predictor)
  })
  
  # Create a Selectize Input for each row of covariates
  output$covariates = renderUI({
    if (nTest() > 1) {
      lapply(2:nTest(), function(i) {
        selectizeInput(inputId = paste0("covariates", i),
                       label = paste0(i, ") Select Covariate(s)"),
                       choices = covariate_choices(),
                       multiple = TRUE)
      })
    }
  })
  
  # Get the intended response variable
  response = reactive({
    convert_variable_names(input$response)
  })
  
  # Get the intended predictor variable
  predictor = reactive({
    convert_variable_names(input$predictor)
  })
  
  
  ################
  # TEST RESULTS #
  ################
  
  # Create table of test results where diabetes (FG>126) is the response
  diabetes_fg = reactive({
    test("diabFG", predictor(), nTest(), df(), input)
  })
  
  # Create table of test results where diabetes (ICD) is the response
  diabetes_icd = reactive({
    test("diabICD", predictor(), nTest(), df(), input)
  })
  
  # Create table of test results where diabetes (either) is the response
  diabetes_either = reactive({
    test("diabComb", predictor(), nTest(), df(), input)
  })
  
  # Create table of test results with some other response
  test_results = reactive({
    test(response(), predictor(), nTest(), df(), input)
  })
  
  # Render the tables in Shiny
  output$diabetes_fg = renderTable(diabetes_fg(), digits = 8)
  output$diabetes_icd = renderTable(diabetes_icd(), digits = 8)
  output$diabetes_either = renderTable(diabetes_either(), digits = 8)
  output$test_results = renderTable(test_results(), digits = 8)

  
  ####################
  # DOWNLOAD RESULTS #
  ####################
  
  output$download = downloadHandler(
    filename = function(con) {
      if (input$show == "Demographics") {
        paste0("Demographics (", input$cohort, ").zip")
      } else if (input$response == "Diabetes Development") {
        paste0(input$response, " ~ ", input$predictor, ".zip")
      } else {
        paste0(input$response, " ~ ", input$predictor, ".csv")
      }
    },
    content = function(con) {
      if (input$show == "Demographics") {
        files = paste0(c("Complete", "Hispanic, not Multiracial"),
                       ".csv")
        tmpdir =  tempdir()
        setwd(tempdir())
        write.csv(complete_demographics(), file = files[1], row.names = F)
        write.csv(hispanic_not_multiracial(), file = files[2], row.names = F)
        zip(zipfile = con, files = files)
      } else if (input$response == "Diabetes Development") {
        files = paste0("Diabetes Development",
                       c("FG>126", "ICD", "Either"),
                       " ~ ",
                       input$predictor,
                       ".csv")
        tmpdir = tempdir()
        setwd(tempdir())
        write.csv(diabetes_fg(), file = files[1], row.names = F)
        write.csv(diabetes_icd(), file = files[2], row.names = F)
        write.csv(diabetes_either(), file = files[3], row.names = F)
        zip(zipfile = con, files = files)
      } else {
        write.csv(test_results(), con)
      }
    }
  )
  
}
