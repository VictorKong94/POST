###############
# DATA IMPORT #
###############

source("setup.R")
df = data_with_bmi
rm(data_with_bmi, data_without_bmi)


#######################################
# DEMOGRAPHICS OF THE ANALYSIS COHORT #
#######################################

# Sex
tab("sex")
# SEX | PERCENTAGE
#   F |      55.30
#   M |      44.70

# Age
summary(df$age)
#           |   AGE
#      Min. | 40.00
#   1st Qu. | 54.00
#    Median | 61.00
#      Mean | 60.33
#   3rd Qu. | 67.00
#      Max. | 75.00

# Race
tab("race")
#                          RACE | PERCENTAGE
# ASIAN / PACIFIC ISLANDER (AP) |       0.06
#                    ASIAN (AS) |      21.10
#                    BLACK (BA) |       4.71
#                 ISLANDER (HP) |       0.13
#          NATIVE AMERICAN (IN) |       0.24
#              MULTIRACIAL (MU) |       7.13
#                    WHITE (WH) |      66.63

# Ethnicity (Hispanic or not)
tab("hispanic")
# HISPANIC | PERCENTAGE
#        N |     87.48
#        Y |      2.58

# Racial breakdown of individuals who identified as Hispanic but not MU
tab("race", data = subset(df, hispanic == "Y" & race != "MU"))
#                          RACE | PERCENTAGE
# ASIAN / PACIFIC ISLANDER (AP) |       0.00
#                    ASIAN (AS) |      15.07
#                    BLACK (BA) |       9.59
#                 ISLANDER (HP) |       0.00
#          NATIVE AMERICAN (IN) |       1.37
#              MULTIRACIAL (MU) |       0.00
#                    WHITE (WH) |      73.97


###############################################
# DEMOGRAPHIC DIFFERENCES AND RELATIONSHIP TO #
# DIABETES DEVELOPMENT (ICD-9 CODE, FG > 126) #
###############################################

# Average pre-statin BMI
log_reg(diabFG ~ pre_bmi)$summary # P = 1.62e-14
log_reg(diabICD ~ pre_bmi)$summary # P = 3.61e-07
log_reg(diabComb ~ pre_bmi)$summary # P = 1.95e-12

# Change in average BMI
log_reg(diabFG ~ delta_bmi)$summary # P = 1.51e-07
log_reg(diabICD ~ delta_bmi)$summary # P = 0.0009
log_reg(diabComb ~ delta_bmi)$summary # P = 7.44e-06

# PDD
log_reg(diabFG ~ delta_bmi + pdd) # P = 0.0595
log_reg(diabICD ~ delta_bmi + pdd) # P = 0.0093
log_reg(diabComb ~ delta_bmi + pdd) # P = 0.0031

# Sex
log_reg(diabFG ~ delta_bmi + pdd + sex) # P = 0.3501
bar_plot("diabFG", "sex")
log_reg(diabICD ~ delta_bmi + pdd + sex) # P = 0.0674
bar_plot("diabICD", "sex")
log_reg(diabComb ~ delta_bmi + pdd + sex) # P = 0.1325
bar_plot("diabComb", "sex")

# Race
log_reg(diabFG ~ delta_bmi + pdd + race) # P = 0.0425
bar_plot("diabFG", "race")
log_reg(diabICD ~ delta_bmi + pdd + race) # P = 9.71e-10
bar_plot("diabICD", "race")
log_reg(diabComb ~ delta_bmi + pdd + race) # P = 1.94e-06
bar_plot("diabComb", "race")

# Ethnicity
log_reg(diabFG ~ delta_bmi + pdd + hispanic) # P = 0.4261
log_reg(diabICD ~ delta_bmi + pdd + hispanic) # P = 0.0905
log_reg(diabComb ~ delta_bmi + pdd + hispanic) # P = 0.3468

# Statin type
log_reg(diabFG ~ delta_bmi + pdd + statin_type) # P = 0.2829
bar_plot("diabFG", "statin_type")
log_reg(diabICD ~ delta_bmi + pdd + statin_type) # P = 0.1123
bar_plot("diabICD", "statin_type")
log_reg(diabComb ~ delta_bmi + pdd + statin_type) # P = 0.0088
bar_plot("diabComb", "statin_type")

# Whether patient changed statin types
log_reg(diabFG ~ delta_bmi + pdd + changed_statin_type) # P = 0.0007
bar_plot("diabFG", "changed_statin_type")
log_reg(diabICD ~ delta_bmi + pdd + changed_statin_type) # P = 5.76e-05
bar_plot("diabICD", "changed_statin_type")
log_reg(diabComb ~ delta_bmi + pdd + changed_statin_type) # P = 1.37e-05
bar_plot("diabComb", "changed_statin_type")

# Relationship to LDLC lowering
log_reg(diabFG ~ delta_bmi + pdd + delta_ldl) # P = 0.9674
log_reg(diabICD ~ delta_bmi + pdd + delta_ldl) # P = 0.7247
log_reg(diabComb ~ delta_bmi + pdd + delta_ldl) # P = 0.8481

# Relationship (or lack of relationship to TSH)
log_reg(diabFG ~ delta_bmi + pdd + tsh) # P = 0.3155
log_reg(diabICD ~ delta_bmi + pdd + tsh) # P = 0.9984
log_reg(diabComb ~ delta_bmi + pdd + tsh) # P = 0.5817


#############################################################
# CHANGE IN LDL, TITRATION OF PDD, AND DIABETES DEVELOPMENT #
#############################################################

# LDL Change on PDD
lin_reg(delta_ldl ~ pdd) # P = 2.2e-16
lin_reg(delta_ldl ~ delta_bmi + pdd) # P = 2.2e-16

# Change in FG on whether a patient decreased LDL below 100
lin_reg(delta_fg ~ pre_bmi + pre_fg + met_ldl_goal) # P = 0.9257
bar_plot("delta_fg", "met_ldl_goal")

# Diabetes development on whether a patient decreased LDL below 100
log_reg(diabFG ~ delta_bmi + pdd + met_ldl_goal) # P = 0.8393
bar_plot("diabFG", "met_ldl_goal")
log_reg(diabICD ~ delta_bmi + pdd + met_ldl_goal) # P = 0.0084
bar_plot("diabICD", "met_ldl_goal")
log_reg(diabComb ~ delta_bmi + pdd + met_ldl_goal) # P = 0.1605
bar_plot("diabComb", "met_ldl_goal")


################################################################################
# SURVIVAL ANALYSIS - ARE ANY OF THE ABOVE ASSOCIATED WITH THE TIME TO DEVELOP #
#                     DIABETES AFTER THE START OF STATIN TREATMENT (ADJUSTING  #
#                     FOR PRE-STATIN BMI AND PRE-STATIN FASTING GLUCOSE)       #
################################################################################

# Sex
lin_reg(survival ~ pre_bmi + pre_fg + sex) # P = 0.7429
bar_plot("survival", "sex")

# Race
lin_reg(survival ~ pre_bmi + pre_fg + race) # P = 0.0352

# Ethnicity
lin_reg(survival ~ pre_bmi + pre_fg + hispanic) # P = 0.9803

# Statin type
lin_reg(survival ~ pre_bmi + pre_fg + statin_type) # P = 0.0182

# PDD
lin_reg(survival ~ pre_bmi + pre_fg + pdd) # P = 0.0118

# LDLC lowering
lin_reg(survival ~ pre_bmi + pre_fg + delta_ldl) # P = 0.5263

# TSH
lin_reg(survival ~ pre_bmi + pre_fg + tsh) # P = 0.1878


#########################################################################
# DEMOGRAPHIC DIFFERENCES AND RELATIONSHIP TO MAX DELTA FASTING GLUCOSE #
#########################################################################

# Sex
lin_reg(delta_fg ~ delta_bmi + pdd + sex) # P = 
bar_plot("delta_fg", "sex")

# Race
lin_reg(delta_fg ~ delta_bmi + pdd + race) # P = 
bar_plot("delta_fg", "race")

# Ethnicity
lin_reg(delta_fg ~ delta_bmi + pdd + hispanic) # P = 
bar_plot("delta_fg", "hispanic")

# Statin type
lin_reg(delta_fg ~ delta_bmi + pdd + statin_type) # P = 
bar_plot("delta_fg", "statin_type")

# PDD
lin_reg(delta_fg ~ delta_bmi + pdd) # P = 

# Relationship to LDLC lowering
lin_reg(delta_fg ~ delta_bmi + pdd + statin_type + delta_ldl) # P = 

# Relationship (or lack of relationship to TSH)
lin_reg(delta_fg ~ delta_bmi + pdd + statin_type + tsh) # P = 


####################
# DATA AGGREGATION #
####################

agg("sex")
# sex | diabFG | diabICD | diabComb | survival | pre_fg | delta_fg
#   F | 0.0231 |  0.0261 |   0.0398 |   630.21 |  94.14 |     7.36
#   M | 0.0205 |  0.0211 |   0.0349 |   617.10 |  95.84 |     7.78
#
# sex | pre_bmi | delta_bmi |   age |    pdd | delta_ldl |  tsh
#   F |   27.76 |    0.0999 | 61.01 | 0.5699 |    -52.72 | 2.34
#   M |   28.16 |    0.0451 | 59.48 | 0.6563 |    -49.72 | 2.20

agg("race")
# race | diabFG | diabICD | diabComb | survival | pre_fg | delta_fg
#   AP |        |         |          |          |  95.88 |    10.67
#   AS | 0.0249 |  0.0425 |   0.0540 |   569.46 |  95.18 |     8.19
#   BA | 0.0228 |  0.0311 |   0.0414 |   816.82 |  93.68 |     7.95
#   HP | 0.2308 |  0.0769 |   0.2308 |   621.00 |  99.44 |    13.46
#   IN |        |         |          |          |  95.65 |     6.48
#   MU | 0.0219 |  0.0328 |   0.0464 |   758.40 |  94.46 |     7.86
#   WH | 0.0206 |  0.0165 |   0.0310 |   616.34 |  94.94 |     7.28
#
# race | pre_bmi | delta_bmi |   age |    pdd | delta_ldl |  tsh
#   AP |   26.03 |   -0.4299 | 70.00 | 0.4519 |    -55.02 | 1.71
#   AS |   25.76 |   -0.0097 | 57.58 | 0.5732 |    -51.70 | 2.07
#   BA |   30.03 |    0.1509 | 58.30 | 0.6004 |    -52.74 | 1.80
#   HP |   28.17 |    0.0984 | 56.62 | 0.6591 |    -50.27 | 1.84
#   IN |   27.91 |    0.2965 | 54.68 | 0.5728 |    -49.15 | 1.94
#   MU |   27.85 |    0.0713 | 60.42 | 0.5911 |    -49.81 | 2.21
#   WH |   28.49 |    0.0970 | 61.35 | 0.6224 |    -51.36 | 2.39

agg("hispanic")
# hispanic | diabFG | diabICD | diabComb | survival | pre_fg | delta_fg
#        N | 0.0218 |  0.0236 |   0.0372 |   635.39 |  94.93 |     7.55
#        Y | 0.0151 |  0.0415 |   0.0491 |   625.50 |  94.88 |     7.38
#
# hispanic | pre_bmi | delta_bmi |   age |    pdd | delta_ldl |  tsh
#        N |   27.91 |    0.0551 | 60.98 | 0.6021 |    -51.36 | 2.28
#        Y |   28.45 |    0.0939 | 60.16 | 0.6047 |    -48.69 | 2.48

agg("statin_type")

agg("changed_statin_type")

agg("diabFG")
# diabFG | diabICD | diabComb | survival | pre_fg | delta_fg | pre_bmi
#      0 |  0.0160 |   0.0160 |          |  94.81 |     6.68 |   27.89
#      1 |  0.3733 |   1.0000 |   624.61 |  99.05 |    46.39 |   30.25
#
# diabFG | delta_bmi  |   age |    pdd | delta_ldl |  tsh
#      0 |     0.0635 | 60.36 | 0.6072 |    -51.37 | 2.29
#      1 |     0.6066 | 58.81 | 0.6688 |    -51.71 | 2.12

agg("diabICD")
# diabICD | diabFG | diabComb | survival | pre_fg | delta_fg | pre_bmi
#       0 | 0.0141 |   0.0141 |   639.01 |  94.82 |     6.98 |   27.90
#       1 | 0.3429 |   1.0000 |   600.37 |  98.27 |    30.90 |   29.39
#
# diabICD | delta_bmi |   age |    pdd | delta_ldl |  tsh
#       0 |    0.0676 | 60.34 | 0.6066 |    -51.35 | 2.28
#       1 |    0.3956 | 59.63 | 0.6868 |    -52.54 | 2.29

agg("diabComb")
# diabComb | diabFG | diabICD | survival | pre_fg | delta_fg | pre_bmi
#        0 |        |         |          |  94.76 |     6.59 |   27.87
#        1 | 0.5829 |  0.6347 |   624.61 |  98.54 |    32.13 |   29.54
#
# diabComb | delta_bmi |   age |    pdd | delta_ldl |  tsh
#        0 |    0.0619 | 60.35 | 0.6058 |    -51.35 | 2.29
#        1 |    0.4214 | 59.59 | 0.6786 |    -52.17 | 2.19