# Dictionary of names for predictor variables
dictionary = c("Diabetes Development" = "",
               "Survival" = "survival",
               "Pre-statin FG" = "pre_fg",
               "Change in FG" = "delta_fg",
               "Pre-statin BMI" = "pre_bmi",
               "Change in BMI" = "delta_fg",
               "Sex" = "sex",
               "Age" = "age",
               "Race" = "race",
               "Hispanic" = "hispanic",
               "Main Statin Used" = "statin_type",
               "Changed Statin Type" = "changed_statin_type",
               "PDD/DDD" = "pdd",
               "Pre-statin LDL" = "pre_ldl",
               "Change in LDL" = "delta_ldl",
               "Met LDL<100 Goal" = "met_ldl_goal",
               "TSH Level" = "tsh")

# Method to compute percentage breakdowns for a given variable
tab = function(variable, data = df) {
  table(data[, variable])[sort(levels(data[, variable]))]
}

# Method to compose a data table of demographics
demographics = function(data = df) {
  demog = data.frame(
    "Attribute" = c("Sex", "",
                    "Race", "", "", "",
                    "Hispanic", "",
                    "Primary Statin Used", "", "", "",
                    "Method of Diagnosis", "", "", "",
                    "Total"),
    "Demographic" = c("Female", "Male",
                      "Asian", "Black", "Multiracial", "White",
                      "No", "Yes",
                      "Atorvastatin", "Lovastatin", "Pravastatin",
                      "Simvastatin",
                      "FG>126", "ICD", "Either", "Both",
                      "")
  )
  demog$Count = c(
    tab("sex", data),
    tab("race", data),
    tab("hispanic", data),
    tab("statin_type", data),
    sum(data[, "diabFG"]), sum(data[, "diabICD"]), sum(data[, "diabComb"]),
    sum(data[, "diabFG"] & data[, "diabICD"]),
    nrow(data)
  )
  demog$Percentage = round(demog$Count / nrow(data) * 100, 2)
  demog
}

# Method to convert common variable names to R variable names 
convert_variable_names = function(variables) {
  dictionary[variables]
}

# Method to compose a formula
make_formula = function(response, predictor, covariates = "") {
  if (is.null(covariates[1])) {
    paste(response, "~", predictor)
  } else {
    covariates = paste(covariates, "+", collapse = " ")
    paste(response, "~", covariates, predictor)
  }
}

# Method to build, summarize, and assess our logistic regression model
log_reg = function(formula, data = df) {
  model = glm(formula, family = binomial(link = "logit"), data = data)
  list("model" = model,
       "summary" = summary(model)$coefficients[, c(1, 4)],
       "anova" = anova(model, test = "Chisq")[, 5])
}

# Method to assess relationship between relationship to survival time
lin_reg = function(formula, data = df) {
  model = lm(formula, data = data)
  list("model" = model,
       "summary" = summary(model)$coefficients[, c(1, 4)],
       "anova" = anova(model, test = "Chisq")[, 5])
}

# Method to compose a data table of test results
test = function(response, predictor, nTest, data, input) {
  
  # Decide what type of regression test to perform
  if (response %in% c("diabFG", "diabICD", "diabComb")) {
    reg = log_reg
  } else {
    reg = lin_reg
  }
  
  # Perform the test with no covariates
  no_covars = reg(make_formula(response, predictor), data)
  
  # Determine how many rows to set aside for each test
  if (predictor %in% c("race", "statin_type")) {
    
    # Determine what levels to list
    if (predictor == "race") {
      lvls = c("Asian", "Black", "Multiracial")
    } else {
      lvls = c("Atorvastatin", "Lovastatin", "Pravastatin")
    }
    
    # Save information from the test with no covariates
    X = data.frame("Covariates" = "",
                   "Level" = c(lvls, "(Overall ANOVA)"))
    X$Beta = c(tail(no_covars$summary[, 1], 3), NA)
    X$P = c(tail(no_covars$summary[, 2], 3),
            tail(no_covars$anova, 1))
    
    # Perform each subsequent test the user desires
    if (nTest > 1) {
      for (i in 2:nTest) {
        covariates = input[[paste0("covariates", i)]]
        covars = reg(make_formula(response,
                                  predictor,
                                  convert_variable_names(covariates)),
                     data)
        X = rbind(X,
                  data.frame(
                    "Covariates" = toString(covariates),
                    "Level" = lvls,
                    "Beta" = tail(covars$summary[, 1], 3),
                    "P" = tail(covars$summary[, 2], 3)
                  ),
                  data.frame(
                    "Covariates" = toString(covariates),
                    "Level" = "(Overall ANOVA)",
                    "Beta" = NA,
                    "P" = tail(covars$anova, 1)
                  ))
      }
    }
    
  } else {
    
    # Perform the test with no covariates
    X = data.frame(
      "Covariates" = "",
      "Beta" = tail(no_covars$summary[, 1], 1),
      "P" = tail(no_covars$summary[, 2], 1)
    )
    
    # Perform each subsequent test the user desires
    if (nTest > 1) {
      for (i in 2:nTest) {
        covariates = input[[paste0("covariates", i)]]
        covars = reg(make_formula(response,
                                  predictor,
                                  convert_variable_names(covariates)),
                     data)
        X = rbind(X,
                  data.frame(
                    "Covariates" = toString(covariates),
                    "Beta" = tail(covars$summary[, 1], 1),
                    "P" = tail(covars$summary[, 2], 1)
                  ))
      }
    }
    
  }
  
  # Print the completed data table
  X
}
