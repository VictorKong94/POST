# Dictionary of names for predictor variables
dictionary = c(
  
  # Demographics
  "Sex" = "sex",
  "Age" = "age",
  "Race" = "race",
  "Hispanic" = "hispanic",
  
  # Time range information
  "Survival" = "survival",
  
  # Pre-statin lab information
  "Median Pre-Statin CK" = "pre_ck",
  "Median Pre-Statin FG" = "pre_fg",
  "Median Pre-Statin HDL" = "pre_hdl",
  "Median Pre-Statin LDL" = "pre_ldl",
  "Median Pre-Statin Total Cholesterol" = "pre_tc",
  "Median Pre-Statin Triglycerides" = "pre_trig",
  "Pre-Statin BMI" = "pre_bmi",
  
  # Post-statin lab information
  "Median Post-Statin CK" = "post_ck",
  "Median Post-Statin HDL" = "post_hdl",
  "Median Post-Statin LDL" = "post_ldl",
  "Median Post-Statin Total Cholesterol" = "post_tc",
  "Median Post-Statin Triglycerides" = "post_trig",
  "Post-Statin BMI" = "post_bmi",
  "Post-Statin FG" = "post_fg",
  
  # Change in lab information
  "Change in BMI" = "delta_bmi",
  "Change in Median CK" = "delta_ck",
  "Change in Median HDL" = "delta_hdl",
  "Change in Median LDL" = "delta_ldl",
  "Change in Median Total Cholesterol" = "delta_tc",
  "Change in Median Triglycerides" = "delta_trig",
  "Max-Delta Change in FG" = "delta_fg",
  "Percent Change in Median LDL" = "pc_delta_ldl",
  
  # Other lab information
  "Maximum Post-Statin Triglycerides" = "post_max_trig",
  "Median Post-Statin LDL < 100" = "met_ldl_goal",
  "Number of CK Readings" = "num_ck",
  "Number of Post-Statin CK Readings" = "num_post_ck",
  "Number of Pre-Statin CK Readings" = "num_pre_ck",
  
  # Statin dose/type information
  "Average Daily Exposure (mgs)" = "avg_mgs",
  "Average Daily Exposure (PDD/DDD)" = "avg_pdd",
  "Changed Statin Type" = "type_change_date",
  "Date Changed Statin Type" = "type_change",
  "Initial Statin Type" = "type_init",
  "Statin Type (from mgs)" = "type_mgs",
  "Statin Type (from PDD/DDD)" = "type_pdd",
  "Total Exposure (from mgs)" = "tse_mgs",
  "Total Exposure (from PDD/DDD)" = "tse_pdd",
  
  # Thyroid-stimulating hormone
  "TSH Level" = "tsh"
  
)

# Method to compute medians and percentage breakdowns
compute = function(data, type, sub = NULL) {
  sub = if (!is.null(sub)) data[sub & !is.na(data)] else data[!is.na(data)]
  if (type == "count") {
    paste0(
      length(sub),
      " (",
      round(100 * length(sub) / length(data[!is.na(data)]), 1),
      ")"
    )
  } else if (type == "median") {
    paste0(
      round(median(sub, na.rm = T), 1),
      " (",
      round(quantile(sub, na.rm = T)[4] - quantile(sub, na.rm = T)[2], 1),
      ")"
    )
  } else {
    stop("Invalid type argument")
  }
}

# Method to compose a data table of demographics
demog = function(data) {
  demog = data.frame(
    "X1" = c(
      "Characteristics",
      "Age (at statin initiation)",
      "Race/Ethnicity (self-reported)", "", "", "", "",
      "BMI (kg/m2 at statin initiation)",
      "Cholesterol panel at baseline", "", "", "", "",
      "Diabetes panel at baseline", "",
      "Initial statin dispensed", "", "", "", "", "", "", "",
      "Primary statin type (mgs/day)", "", "", "", "", "", "",
      "Primary statin type (PDD/DDD)", "", "", "", "", "", "",
      "All on-treatment", "", "", "", "", ""
    ),
    "X2" = c(
      "",
      "",
      "", "White/European", "Black/African", "Hispanic/Latino", "East Asian",
      "",
      "", "TC", "LDL-C", "Triglycerides", "HDL-C",
      "", "Glucose",
      "", "Initial mgs/day",
          "Initial PDD/DDD",
          "Lovastatin",
          "Simvastatin",
          "Atorvastatin",
          "Pravastatin",
          "Other statin",
      "", "Average mgs/day",
          "Lovastatin",
          "Simvastatin",
          "Atorvastatin",
          "Pravastatin",
          "Other statin",
      "", "Average PDD/DDD",
          "Lovastatin",
          "Simvastatin",
          "Atorvastatin",
          "Pravastatin",
          "Other statin",
      "", "Max FG",
          "Number FG>125",
          "Number with a diabetes ICD9",
          "Number with a diabetes Rx",
          "Number with any diabetes diagnosis"
    )
  )
  tmp = data[data$sex == "F",]
  demog$X3 = c(
    paste0("Females (N=", nrow(tmp), ")"),
    compute(tmp$age, "median"),
    "", sapply(c("WH", "BA", "MU", "AS"),
               function(x) compute(tmp$race, "count", tmp$race == x)),
    compute(tmp$pre_bmi, "median"),
    "", sapply(c("pre_tc", "pre_ldl", "pre_trig", "pre_hdl"),
               function(x) compute(tmp[, x], "median")),
    "", compute(tmp$pre_fg, "median"),
    "", compute(tmp$mgs_init, "median"),
        compute(tmp$pdd_init, "median"),
        compute(tmp$type_init, "count", tmp$type_init == "lo"),
        compute(tmp$type_init, "count", tmp$type_init == "si"),
        compute(tmp$type_init, "count", tmp$type_init == "at"),
        compute(tmp$type_init, "count", tmp$type_init == "pr"),
        compute(tmp$type_init, "count",
                !(tmp$type_init %in% c("lo", "si", "at", "pr"))),
    "", compute(tmp$avg_mgs, "median"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "lo"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "si"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "at"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "pr"),
        compute(tmp$type_mgs, "count",
                !(tmp$type_mgs %in% c("lo", "si", "at", "pr"))),
    "", compute(tmp$avg_pdd, "median"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "lo"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "si"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "at"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "pr"),
        compute(tmp$type_pdd, "count",
                !(tmp$type_pdd %in% c("lo", "si", "at", "pr"))),
    "", compute(tmp$max_fg, "median"),
        sapply(c("diabFG", "diabICD", "diabRX", "diabANY"),
               function(x) compute(tmp[, x], "count", tmp[, x] == 1))
  )
  tmp = data[data$sex == "M",]
  demog$X4 = c(
    paste0("Males (N=", nrow(tmp), ")"),
    compute(tmp$age, "median"),
    "", sapply(c("WH", "BA", "MU", "AS"),
               function(x) compute(tmp$race, "count", tmp$race == x)),
    compute(tmp$pre_bmi, "median"),
    "", sapply(c("pre_tc", "pre_ldl", "pre_trig", "pre_hdl"),
               function(x) compute(tmp[, x], "median")),
    "", compute(tmp$pre_fg, "median"),
    "", compute(tmp$mgs_init, "median"),
        compute(tmp$pdd_init, "median"),
        compute(tmp$type_init, "count", tmp$type_init == "lo"),
        compute(tmp$type_init, "count", tmp$type_init == "si"),
        compute(tmp$type_init, "count", tmp$type_init == "at"),
        compute(tmp$type_init, "count", tmp$type_init == "pr"),
        compute(tmp$type_init, "count",
                !(tmp$type_init %in% c("lo", "si", "at", "pr"))),
    "", compute(tmp$avg_mgs, "median"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "lo"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "si"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "at"),
        compute(tmp$type_mgs, "count", tmp$type_mgs == "pr"),
        compute(tmp$type_mgs, "count",
                !(tmp$type_mgs %in% c("lo", "si", "at", "pr"))),
    "", compute(tmp$avg_pdd, "median"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "lo"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "si"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "at"),
        compute(tmp$type_pdd, "count", tmp$type_pdd == "pr"),
        compute(tmp$type_pdd, "count",
                !(tmp$type_pdd %in% c("lo", "si", "at", "pr"))),
    "", compute(tmp$max_fg, "median"),
    sapply(c("diabFG", "diabICD", "diabRX", "diabANY"),
           function(x) compute(tmp[, x], "count", tmp[, x] == 1))
  )
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
log_reg = function(formula, data) {
  model = glm(formula, family = binomial(link = "logit"), data = data)
  list("model" = model,
       "summary" = summary(model)$coefficients[, c(1, 4)],
       "anova" = anova(model, test = "Chisq")[2, 5])
}

# Method to assess relationship between relationship to survival time
lin_reg = function(formula, data) {
  model = lm(formula, data = data)
  list("model" = model,
       "summary" = summary(model)$coefficients[, c(1, 4)],
       "anova" = anova(model, test = "Chisq")[1, 5])
}

# Method to compose a data table of test results
test = function(response, predictor, nTest, data, input) {
  
  # Decide what type of regression test to perform
  if (response %in% c("diabFG", "diabICD", "diabRX", "diabANY")) {
    reg = log_reg
  } else {
    reg = lin_reg
  }
  
  # Perform the test with no covariates
  no_covars = reg(make_formula(response, predictor), data)
  
  # Determine how many rows to set aside for each test
  if (predictor %in% c("race", "type_mgs", "type_pdd")) {
    
    # Determine what levels to list
    if (predictor == "race") {
      lvls = c("Asian", "Black", "Multiracial")
    } else if (predictor == "type_mgs") {
      lvls = c("Atorvastatin",
               "Lovastatin",
               "Pravastatin",
               "Rosuvastatin")
    } else {
      lvls = c("Atorvastatin",
               "Lovastatin",
               "Pravastatin",
               "Rosuvastatin")
    }
    
    # Save information from the test with no covariates
    X = data.frame("Covariates" = "",
                   "Level" = c(lvls, "(Overall ANOVA)"))
    X$Beta = c(tail(no_covars$summary[, 1], length(lvls)), NA)
    X$P = c(tail(no_covars$summary[, 2], length(lvls)),
            no_covars$anova)
    
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
                    "Beta" = tail(covars$summary[, 1], length(lvls)),
                    "P" = tail(covars$summary[, 2], length(lvls))
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
