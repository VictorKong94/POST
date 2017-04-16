library(shiny)

source("helpers.R")

function(input, output, session) {
  
  ###############
  # DATA IMPORT #
  ###############
  
  df = reactive({
    csv = switch(input$cohort,
                 "Analysis Cohort" = "data/analysis_cohort.csv",
                 "Analysis Cohort - Males" = "data/analysis_cohort_M.csv",
                 "Analysis Cohort - Females" = "data/analysis_cohort_F.csv",
                 "Changed Statin Type Cohort" = "data/delta_type.csv",
                 "No BMIs Cohort" = "data/no_bmi.csv",
                 "No Change in Statin Type Cohort" = "data/no_delta_type.csv")
    csv = read.csv(csv)
    if (input$cohort == "Analysis Cohort - Males") {
      csv$sex = factor(csv$sex, levels = c("F", "M"))
    } else if (input$cohort == "Analysis Cohort - Females") {
      csv$sex = "F"
      csv$sex = factor(csv$sex, levels = c("F", "M"))
    } else {
      csv$sex = relevel(csv$sex, ref = "F")
    }
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
  output$complete_demographics = renderTable(complete_demographics(),
                                             hover = T,
                                             na = "")
  output$hispanic_not_multiracial = renderTable(hispanic_not_multiracial(),
                                                hover = T,
                                                na = "")
  
  
  #####################################
  # INPUTS FOR ADDITIONAL INFORMATION #
  #####################################
  
  # Get the desired number of tests to perform
  nTest = reactive({
    as.integer(input$nTest)
  })
  
  # We don't want the same variable for a predictor as our response
  predictor_choices = reactive({
    leave_out_sex = switch(input$cohort,
                           "Analysis Cohort - Males" = "Sex",
                           "Analysis Cohort - Females" = "Sex",
                           NA)
    setdiff(c("Age",
              "Change in BMI",
              "Change in FG",
              "Change in LDL",
              "Change in Log LDL",
              "Changed Statin Type",
              "Hispanic",
              "Log Pre-Statin LDL",
              "Main Statin Used (Days)",
              "Met LDL<100 Goal",
              "PDD per DDD",
              "PDD per DDD (Lovastatin)",
              "PDD per DDD (Other)",
              "Pre-Statin BMI",
              "Pre-Statin FG",
              "Pre-Statin LDL",
              "Race",
              "Sex",
              "TSH Level"),
            c(input$response,
              leave_out_sex))
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
  
  # Create table of test results with some other response
  test_results = reactive({
    if (input$response == "Diabetes Development") {
      diab = test("diabFG", predictor(), nTest(), df(), input)
      diab = cbind(diab,
                   test("diabICD", predictor(), nTest(), df(), input
                        )[, c("Beta", "P")])
      diab = cbind(diab,
                   test("diabComb", predictor(), nTest(), df(), input
                        )[, c("Beta", "P")])
      if (input$predictor %in% c("Main Statin Used (Days)", "Race")) {
        colnames(diab) = c("Covariates", "Level",
                           "Beta (FG>126)", "P (FG>126)",
                           "Beta (ICD)", "P (ICD)",
                           "Beta (Either)", "P (Either)")
      } else {
        colnames(diab) = c("Covariates",
                           "Beta (FG>126)", "P (FG>126)",
                           "Beta (ICD)", "P (ICD)",
                           "Beta (Either)", "P (Either)")
      }
      diab
    } else {
      test(response(), predictor(), nTest(), df(), input)
    }
  })
  
  # Render the tables in Shiny
  output$test_results = renderTable(test_results(),
                                   hover = T,
                                   digits = 8,
                                   na = "")

  
  ####################
  # DOWNLOAD RESULTS #
  ####################
  
  output$download = downloadHandler(
    filename = function(con) {
      if (input$show == "Demographics") {
        paste0("Demographics (", input$cohort, ").zip")
      } else {
        paste0(input$response, " - ", input$predictor, ".csv")
      }
    },
    content = function(con) {
      if (input$show == "Demographics") {
        files = paste0(c("Complete", "Hispanic, not Multiracial"),
                       ".csv")
        tmpdir =  tempdir()
        setwd(tempdir())
        write.csv(complete_demographics(), files[1], row.names = F, na = "")
        write.csv(hispanic_not_multiracial(), files[2], row.names = F, na = "")
        zip(zipfile = con, files = files)
      } else {
        write.csv(test_results(), con, row.names = F, na = "")
      }
    }
  )
  
}
