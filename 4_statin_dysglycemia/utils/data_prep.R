#########
# SETUP #
#########

# Define a function to help us replace blanks
replace_blanks = function(vector) {
  if (is.numeric(vector) | is.integer(vector)) {
    vector_wo_blanks = vector[!is.na(vector)]
    vector[is.na(vector)] = vector_wo_blanks[length(vector_wo_blanks)]
  } else {
    vector_wo_blanks = vector[vector != ""]
    vector[vector == ""] = vector_wo_blanks[length(vector_wo_blanks)]
  }
  return(vector)
}

#############
# DATA PREP #
#############

# Note on columns:
# - diabflag: T2D diagnosis from fasting glucose measurements
# - diabdx: T2D diagnosis by ICD standards
# - diabcflag: combination T2D diagnosis (either method used)
df = read.csv("../data/raw_data.csv",
              na.strings = "")
x = read.csv("../data/bleeding_data.csv",
             na.strings = "",
             stringsAsFactors = F)

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
# - fell under a race caegory other than Asian, Black, Multiracial, or White
df = df[(df$kpnc_race_category %in% c("AS", "BA", "MU", "WH")),]
# - did not have apropriate LDL measurements
df = df[!is.na(df$ave_pre_ldl) & !is.na(df$ave_post_ldl),]
# - did not use primarily use Atorva-, Lova-, Prava-, Simvastatin
df = df[df$Primary.Drug %in% c("Atorva", "Lova", "Prava", "Simva"),]

# We also need some new variables representing (post - pre) differences
df$delta_bmi = df$ave_post_bmi - df$ave_pre_bmi # Change in BMI
df$log_pre_ldl = log(df$ave_pre_ldl) # Log Pre-Statin LDL
df$delta_ldl = df$ave_post_ldl - df$ave_pre_ldl # Change in LDL
df$delta_log_ldl = log(df$ave_post_ldl) - df$log_pre_ldl # Change in Log LDL

# Time it took for diabetes individuals to develop new-onset diabetes
df$statin_date = as.Date(df$statin_date,
                         format = "%m/%d/%y",
                         origin = "01/01/00") # Statin start
fg_date = df[, grep("postdts", colnames(df))]
for (i in 1:ncol(fg_date)) {
  fg_date[, i] = as.Date(fg_date[, i],
                         format = "%m/%d/%y",
                         origin = "01/01/00")
}
fg_reading = df[, grep("postglu", colnames(df))]
diab_date_index = apply(fg_reading >= 126, 1,
                        function(x) match(TRUE, x))
diab_date = fg_date[cbind(1:nrow(fg_date), diab_date_index)]
diab_date = as.Date(diab_date,
                    format = "%Y-%m-%d",
                    origin = "01/01/00") # Date of first FG>126
df$survival = as.numeric(diab_date - df$statin_date)

# Whether a patient met their lipid lowering goals
df$met_ldl_goal = df$ave_post_ldl < 100

# Whether a patient changed statin types
df$changed_statin_type = apply(df[, grep("va$", colnames(df))], 1,
                               function(x) sum(x > 0)) > 1

# Time it took an individual to change Statin type
x = x[x$id %in% df$id[df$changed_statin_type],] # Isolate changed Statin type
# Figure out at which appointments individuals changed Statin types
gennm = x[, grep("gennm", colnames(x))]
gennm = apply(gennm, 1, replace_blanks)
changed = gennm[-1,] != gennm[-nrow(gennm),]
# Compute dates of individuals' appointments
x$statin_date = as.Date(x$statin_date,
                        format = "%m/%d/%y",
                        origin = "01/01/00")
days = x[, setdiff(grep("days", colnames(x)), grep("tot", colnames(x)))]
days = apply(days, 1, cumsum)
days = data.frame(apply(days, 2, replace_blanks))
appt_dates = data.frame("test_subject" = rep(x$statin_date[1], nrow(days) - 1))
for (i in 1:ncol(days)) {
  appt_dates[, colnames(days)[i]] = rep(x$statin_date[i], nrow(days) - 1)
}
appt_dates[, -1] = appt_dates[, -1] + days[-nrow(days),]
appt_dates$test_subject = NULL
# Figure out the exact dates individuals changed Statin types
x$date_statin_change = x$statin_date
for (i in 1:ncol(changed)) {
  x$date_statin_change[i] = appt_dates[,i][changed[,i]][1]
}
x$days_before_statin_change = x$date_statin_change - x$statin_date
df$days_before_statin_change[df$MRN %in% x$MRN] = x$days_before_statin_change

# Adjust maxdelta for those who changed their statin type
before_statin_change = fg_date[df$MRN %in% x$MRN,]
for (i in 1:ncol(before_statin_change)) {
  before_statin_change[, i] = before_statin_change[, i] < x$date_statin_change
}
before_statin_change[is.na(before_statin_change)] = F
x_fg_reading = fg_reading[df$MRN %in% x$MRN,]
adj_max_delta = x$maxdelta
for (i in 1:length(adj_max_delta)) {
  temp = as.numeric(x_fg_reading[i, as.logical(before_statin_change[i,])])
  adj_ave_post_glu = suppressWarnings(mean(head(sort(temp, decreasing = T), 2)))
  adj_max_delta[i] = adj_ave_post_glu - x$avepre_gluresult[i]
}
adj_max_delta[is.nan(adj_max_delta)] = NA
df$adj_max_delta[df$MRN %in% x$MRN] = adj_max_delta

# Variables to check whether PDD is different for users of different statins
df$lovastatin_pdd = df$PDD.DDD
df$lovastatin_pdd[df$Primary.Drug != "Lova"] = 0
df$other_pdd = df$PDD.DDD
df$other_pdd[df$Primary.Drug %in% c("Lova", "Simva")] = 0

# And we'll only keep variables important to our analysis
df = df[, c("id", # Patient identification
            "diabflag", "diabdx", "diabcflag", # Development of diabetes
            "survival", # Days after starting statins to develop T2D
            "avepre_gluresult", "maxdelta", "adj_max_delta", # Fasting glucose
            "ave_pre_bmi", "delta_bmi", # BMI
            "patient_sex", # Sex
            "patient_age", # Age
            "kpnc_race_category", "kpnc_hispanic", # Race / Hispanic
            "Primary.Drug", "changed_statin_type",
                            "days_before_statin_change", # Statin-related
            "PDD.DDD", "PDD.DDD.Factor", "Overall.Change.in.PDD.DDD", # PDD/DDD
            "lovastatin_pdd", "other_pdd",# PDD- and Statin-related
            "ave_pre_ldl", "log_pre_ldl", "delta_ldl", "delta_log_ldl",
                           "met_ldl_goal", # LDL
            "tshresult")] # TSH and hypothyroidism
colnames(df) = c("id",
                 "diabFG", "diabICD", "diabComb",
                 "survival",
                 "pre_fg", "delta_fg", "adj_delta_fg",
                 "pre_bmi", "delta_bmi",
                 "sex",
                 "age",
                 "race", "hispanic",
                 "statin_type", "changed_statin_type",
                                "days_before_statin_change",
                 "pdd", "pdd_group", "delta_pdd",
                 "lovastatin_pdd", "other_pdd",
                 "pre_ldl", "log_pre_ldl", "delta_ldl", "delta_log_ldl",
                            "met_ldl_goal",
                 "tsh")

# Separate individuals without sufficient BMI data
data_without_bmi = df[is.na(df$pre_bmi) | is.na(df$delta_bmi),]
data_with_bmi = df[!is.na(df$pre_bmi) & !is.na(df$delta_bmi),]
rm(list = setdiff(ls(), c("data_with_bmi", "data_without_bmi")))

# Separate individuals on whether they changed their statin type
data_delta_type = data_with_bmi[data_with_bmi$changed_statin_type,]
data_no_delta_type = data_with_bmi[!data_with_bmi$changed_statin_type,]

# Separate individuals by sex
data_females = data_with_bmi[data_with_bmi$sex == "F",]
data_males = data_with_bmi[data_with_bmi$sex == "M",]

write.csv(data_with_bmi,
          file = "../data/analysis_cohort.csv",
          row.names = F,
          na = "")
write.csv(data_without_bmi,
          file = "../data/no_bmi.csv",
          row.names = F,
          na = "")
write.csv(data_delta_type,
          file = "../data/delta_type.csv",
          row.names = F,
          na = "")
write.csv(data_no_delta_type,
          file = "../data/no_delta_type.csv",
          row.names = F,
          na = "")
write.csv(data_females,
          file = "../data/analysis_cohort_F.csv",
          row.names = F,
          na = "")
write.csv(data_males,
          file = "../data/analysis_cohort_M.csv",
          row.names = F,
          na = "")
