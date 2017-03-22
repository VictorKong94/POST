###############################
# DATA IMPORT AND PREPARATION #
###############################

df = read.csv("data.csv", na.strings = "")
# Note on columns:
# - diabflag: T2D diagnosis from fasting glucose measurements
# - diabdx: T2D diagnosis by ICD standards
# - diabcflag: combination T2D diagnosis (either method used)

# No individuals with strictly 5+ years of continuous statin use were determined
# to have developed T2D by any standards
apply(df[is.na(df$days36mo_tot) & !is.na(df$days60mo_tot),
         c("diabflag", "diabcflag", "diabdx")],
      2, sum, na.rm = T) # (0, 0, 0)

# We therefore remove them for the purposes of our analysis, leaving us with
# data on 15537 individuals
df = df[!(is.na(df$days36mo_tot) & !is.na(df$days60mo_tot)),]

# Next we'll drop individuals who:
# - did not disclose their sex
df = df[!is.na(df$patient_sex),]
# - did not disclose their race or whose race is unknown
df = df[!(df$kpnc_race_category %in% c(NA, "UN")),]
df$kpnc_race_category = droplevels(df$kpnc_race_category)
# - did not have apropriate LDL measurements
df = df[!is.na(df$ave_pre_ldl) & !is.na(df$ave_post_ldl),]

# We also need some new variables representing (post - pre) differences
df$delta_bmi = df$ave_post_bmi - df$ave_pre_bmi # Change in BMI
df$delta_ldl = df$ave_post_ldl - df$ave_pre_ldl # Change in LDL

# Time it took for diabetes individuals to develop new-onset diabetes
df$statin_date = as.Date(df$statin_date, format = "%m/%d/%y")
fg_date = df[, grep("postdts", colnames(df))]
diab_date_index = apply(df[, grep("postglu", colnames(df))] >= 126, 1,
                        function(x) match(TRUE, x))
diab_date = as.Date(fg_date[cbind(1:nrow(fg_date), diab_date_index)],
                    format = "%m/%d/%y")
df$survival = as.numeric(diab_date - df$statin_date)

# Whether a patient met their lipid lowering goals
df$met_ldl_goal = df$ave_post_ldl < 100

# Whether a patient changed statin types
df$changed_statin_type = apply(df[, grep("va$", colnames(df))], 1,
                               function(x) sum(x > 0)) > 1

# And we'll only keep variables important to our analysis
df = df[, c("id", # Patient identification
            "diabflag", "diabdx", "diabcflag", # Development of diabetes
            "survival", # Number of days after starting statins to develop T2D
            "avepre_gluresult", "maxdelta", # Fasting glucose
            "ave_pre_bmi", "delta_bmi", # BMI
            "patient_sex", # Sex
            "patient_age", # Age
            "kpnc_race_category", "kpnc_hispanic", # Race / Hispanic
            "Primary.Drug", "PDD.DDD", "changed_statin_type", # Statin-related
            "delta_ldl", "met_ldl_goal", # LDL
            "tshresult")] # TSH and hypothyroidism
colnames(df) = c("id",
                 "diabFG", "diabICD", "diabComb",
                 "survival",
                 "pre_fg", "delta_fg",
                 "pre_bmi", "delta_bmi",
                 "sex",
                 "age",
                 "race", "hispanic",
                 "statin_type", "pdd", "changed_statin_type",
                 "delta_ldl", "met_ldl_goal",
                 "tsh")

# Separate individuals without sufficient BMI data
data_without_bmi = df[is.na(df$pre_bmi) | is.na(df$delta_bmi),]
data_with_bmi = df[!is.na(df$pre_bmi) & !is.na(df$delta_bmi),]
rm(list = setdiff(ls(), c("data_with_bmi", "data_without_bmi")))

###################################
# METHOD AND PARAMETER DEFINITION #
###################################

require("ggplot2")
require("plyr")

# 
tab = function(variable, data = df) {
  round(table(data[, variable]) / nrow(data) * 100, 2)
}

# Method to build, summarize, and assess our logistic regression model
log_reg = function(formula) {
  model = glm(formula, family = binomial(link = "logit"), data = df)
  list("model" = model,
       "summary" = summary(model)$coefficients,
       "anova" = anova(model, test = "Chisq"))
}

# Method to assess relationship between relationship to survival time
lin_reg = function(formula) {
  model = lm(formula, data = df)
  list("model" = model,
       "anova" = anova(model, test = "Chisq"))
}

# Method to compute aggregate statistics quickly
agg = function(variable) {
  report = suppressWarnings(
    aggregate(df[setdiff(colnames(df), "id")],
              by = list(df[, variable]),
              FUN = function(x) round(mean(x, na.rm = T), 4))
  )
  colnames(report)[1] = variable
  report[, unique(colnames(report))]
}

# Method to make barplots from data
bar_plot = function(variable, group_by) {
  summarize = function(x, column) {
    sub = x[, column]
    c(mean = mean(sub, na.rm = T),
      se = sd(sub, na.rm = T) / sqrt(sum(!is.na(sub))))
  }
  ds = ddply(df, group_by, .fun = summarize, variable)
  ds = rename(ds, c("mean" = variable))
  ggplot(ds, aes(x = get(group_by), y = get(variable), fill = get(group_by))) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = get(variable) - se, ymax = get(variable) + se),
                  width = .1, position = position_dodge(.9)) +
    labs(x = group_by, y = variable) +
    theme(legend.position = "none")
}

# Method to make scatterplots from data
scatter_plot = function(response, predictor) {
  ggplot(df, aes(x = get(predictor),
                 y = get(response))) +
    geom_point(shape = 1) +
    geom_smooth(method = lm, se = F, fullrange = T) +
    labs(x = predictor, y = response)
}

# Make sure we're omitting missing values from individual analyses
options(na.action = "na.omit")