#########
# SETUP #
#########

# Do NOT assume strings belong to factor variables
options(stringsAsFactors = F)

# Import processed data
df = read.csv("../data/processed/data.csv",
              na.strings = "")


####################
# COHORT SELECTION #
####################

# Starting with all individuals we have data on (20052), keep individuals...
# - whose race is Asian, Black, Multiracial, or White (18462)
keep1 = df$race %in% c("AS", "BA", "MU", "WH") & !is.na(df$race)
# - who have both pre-statin and post-statin BMI information (8506)
keep2 = !is.na(df$pre_bmi) & !is.na(df$post_bmi)
# - who have both pre-statin and post-statin fasting glucose information (19900)
keep3 = !is.na(df$pre_fg) & !is.na(df$post_fg)
# There are a total of 7752 individuals who meet all three criteria
keep = keep1 & keep2 & keep3
df = df[keep,]


#################################
# DIVIDE THE COHORT INTO GROUPS #
#################################

# Those in the case cohort have IDs in the form 2XXXXX
case = df[df$id >= 200000,]
case_f = case[case$sex == "F",]
case_m = case[case$sex == "M",]

# We want to look into individuals who used exclusively Simvastatin/Lovastatin
case_lo = case[case$type_pdd == "lo" & !case$type_change,]
case_si = case[case$type_pdd == "si" & !case$type_change,]

# Those in the control cohort have IDs in the form 1XXXXX
control = df[df$id < 200000,]


#######################
# WRITE DATA TO FILES #
#######################

write.csv(case, file = "../data/case.csv", na = "", row.names = F)
write.csv(case_f, file = "../data/case_f.csv", na = "", row.names = F)
write.csv(case_m, file = "../data/case_m.csv", na = "", row.names = F)
write.csv(case_lo, file = "../data/case_lo.csv", na = "", row.names = F)
write.csv(case_si, file = "../data/case_si.csv", na = "", row.names = F)
write.csv(control, file = "../data/control.csv", na = "", row.names = F)

