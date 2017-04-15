# Note on columns:
# - diabflag: T2D diagnosis from fasting glucose measurements
# - diabdx: T2D diagnosis by ICD standards
# - diabcflag: combination T2D diagnosis (either method used)
df = read.csv("../data/raw_data.csv", na.strings = "")

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

# Variables to check whether PDD is different for users of different statins
df$lovastatin_pdd = df$PDD.DDD
df$lovastatin_pdd[df$Primary.Drug != "Lova"] = 0
df$other_pdd = df$PDD.DDD
df$other_pdd[df$Primary.Drug %in% c("Lova", "Simva")] = 0

# And we'll only keep variables important to our analysis
df = df[, c("id", # Patient identification
            "diabflag", "diabdx", "diabcflag", # Development of diabetes
            "survival", # Days after starting statins to develop T2D
            "avepre_gluresult", "maxdelta", # Fasting glucose
            "ave_pre_bmi", "delta_bmi", # BMI
            "patient_sex", # Sex
            "patient_age", # Age
            "kpnc_race_category", "kpnc_hispanic", # Race / Hispanic
            "Primary.Drug", "changed_statin_type", # Statin-related
            "PDD.DDD", "PDD.DDD.Factor", # PDD-related
            "lovastatin_pdd", "other_pdd",# PDD- and Statin-related
            "ave_pre_ldl", "log_pre_ldl", "delta_ldl", "delta_log_ldl",
                           "met_ldl_goal", # LDL
            "tshresult")] # TSH and hypothyroidism
colnames(df) = c("id",
                 "diabFG", "diabICD", "diabComb",
                 "survival",
                 "pre_fg", "delta_fg",
                 "pre_bmi", "delta_bmi",
                 "sex",
                 "age",
                 "race", "hispanic",
                 "statin_type", "changed_statin_type",
                 "pdd", "pdd_group",
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

write.csv(data_with_bmi, file = "../data/analysis_cohort.csv", row.names = F)
write.csv(data_without_bmi, file = "../data/no_bmi.csv", row.names = F)
write.csv(data_delta_type, file = "../data/delta_type.csv", row.names = F)
write.csv(data_no_delta_type, file = "../data/no_delta_type.csv", row.names = F)
write.csv(data_females, file = "../data/analysis_cohort_F.csv", row.names = F)
write.csv(data_males, file = "../data/analysis_cohort_M.csv", row.names = F)