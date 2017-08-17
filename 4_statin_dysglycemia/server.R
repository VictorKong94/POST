library(shiny)

source("helpers.R")

function(input, output, session) {
  
  ###############
  # DATA IMPORT #
  ###############
  
  df = reactive({
    csv = switch(input$cohort,
                 "Case Cohort" = "data/case.csv",
                 "Case Cohort - Females" = "data/case_f.csv",
                 "Case Cohort - Lovastatin (Exclusive)" = "data/case_lo.csv",
                 "Case Cohort - Males" = "data/case_m.csv",
                 "Case Cohort - Simvastatin (Exclusive)" = "data/case_si.csv",
                 "Control Cohort" = "data/control.csv")
    csv = read.csv(csv, na.strings = "")
    if (input$cohort == "Case Cohort - Females") {
      csv$sex = "F"
      csv$sex = factor(csv$sex, levels = c("F", "M"))
    } else if (input$cohort == "Case Cohort - Lovastatin (Exclusive)") {
      csv$type_pdd = factor(csv$type_pdd, levels = c("at", "lo", "pr", "si"))
    } else if (input$cohort == "Case Cohort - Males") {
      csv$sex = factor(csv$sex, levels = c("F", "M"))
    } else if (input$cohort == "Case Cohort - Simvastatin (Exclusive)") {
      csv$type_pdd = factor(csv$type_pdd, levels = c("at", "lo", "pr", "si"))
    }
    csv$race = relevel(csv$race, ref = "WH")
    csv$type_pdd = relevel(csv$type_pdd, ref = "si")
    csv
  })
  
  
  #######################
  # DEMOGRAPHICS TABLES #
  #######################
      
  # Compose demographics table
  demographics = reactive({
    demog(df())
  })
  
  # Render the tables in Shiny
  output$demographics = renderTable(demographics(),
                                    colnames = F,
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
    drop_var = switch(
      input$cohort,
      "Case Cohort - Females" = "Sex",
      "Case Cohort - Lovastatin (Exclusive)" = c("Changed Statin Type",
                                                 "Date Changed Statin Type",
                                                 "Initial Statin Type",
                                                 "Statin Type (from mgs)",
                                                 "Statin Type (from PDD/DDD)"),
      "Case Cohort - Males" = "Sex",
      "Case Cohort - Simvastatin (Exclusive)" = c("Changed Statin Type",
                                                  "Date Changed Statin Type",
                                                  "Initial Statin Type",
                                                  "Statin Type (from mgs)",
                                                  "Statin Type (from PDD/DDD)"),
      NA)
    setdiff(c("Age",
              "Average Daily Exposure (mgs)",
              "Average Daily Exposure (PDD/DDD)",
              "Change in BMI",
              "Change in Median CK",
              "Change in Median HDL",
              "Change in Median LDL",
              "Change in Median Total Cholesterol",
              "Change in Median Triglycerides",
              "Changed Statin Type",
              "Date Changed Statin Type",
              "Hispanic",
              "Initial Statin Type",
              "Max-Delta Change in FG",
              "Maximum Post-statin Triglycerides",
              "Median Post-statin LDL < 100",
              "Median Pre-Statin CK",
              "Median Pre-Statin FG",
              "Median Pre-Statin HDL",
              "Median Pre-Statin LDL",
              "Median Pre-Statin Total Cholesterol",
              "Median Pre-Statin Triglycerides",
              "Median Post-Statin CK",
              "Median Post-Statin HDL",
              "Median Post-Statin LDL",
              "Median Post-Statin Total Cholesterol",
              "Median Post-Statin Triglycerides",
              "Number of CK Readings",
              "Number of Post-statin CK Readings",
              "Number of Pre-statin CK Readings",
              "Post-Statin BMI",
              "Post-Statin FG",
              "Pre-Statin BMI",
              "Race",
              "Sex",
              "Statin Type (from mgs)",
              "Statin Type (from PDD/DDD)",
              "Survival",
              "Total Exposure (from mgs)",
              "Total Exposure (from PDD/DDD)",
              "TSH Level" = "tsh"),
            c(input$response,
              drop_var))
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
      diab = cbind(
        test("diabFG", predictor(), nTest(), df(), input),
        test("diabICD", predictor(), nTest(), df(), input)[, c("Beta", "P")],
        test("diabRX", predictor(), nTest(), df(), input)[, c("Beta", "P")],
        test("diabANY", predictor(), nTest(), df(), input)[, c("Beta", "P")]
      )
      if (input$predictor %in% c("Statin Type", "Race")) {
        colnames(diab) = c("Covariates", "Level",
                           "Beta (FG>125)", "P (FG>125)",
                           "Beta (ICD)", "P (ICD)",
                           "Beta (Rx)", "P (Rx)",
                           "Beta (Any)", "P (Any)")
      } else {
        colnames(diab) = c("Covariates",
                           "Beta (FG>125)", "P (FG>125)",
                           "Beta (ICD)", "P (ICD)",
                           "Beta (Rx)", "P (Rx)",
                           "Beta (Any)", "P (Any)")
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
        "Demographics.csv"
      } else {
        paste0(input$response, " - ", input$predictor, ".csv")
      }
    },
    content = function(con) {
      if (input$show == "Demographics") {
        write.table(demographics(),
                    con,
                    col.names = F,
                    na = "",
                    sep = ",",
                    row.names = F)
      } else {
        write.table(test_results(),
                    con,
                    na = "",
                    sep = ",",
                    row.names = F)
      }
    }
  )
  
}
