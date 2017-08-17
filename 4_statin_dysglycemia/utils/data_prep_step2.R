#########
# SETUP #
#########

# Do NOT assume strings belong to factor variables
options(stringsAsFactors = F)

# Define a function to easily compute median lab test results
getlabs = function(row, test) {
  date = get(paste0(test, "date"))[row,]
  result = get(test)[row,]
  if (test == "bmi") {
    # Pre-statin BMI: most recent BMI prior to statin start
    tmp = date <= 0 & !is.na(date)
    pre = ifelse(sum(tmp) == 0,
                 yes = NA,
                 no = result[tmp][which.max(date[tmp])])
    # Post-statin BMI: BMI after statin start closest to cutoff date (within 90
    #                  days of cutoff date if after)
    tmp = date > 0 & !is.na(date)
    post = ifelse(sum(tmp) == 0,
                  yes = NA,
                  no = result[tmp][which.min(abs(date[tmp] - df$cutoff[row]))])
  } else if (test == "fg") {
    # Pre-statin lab: median lab result prior to statin start
    tmp = unlist(result[date <= 0 & !is.na(date)])
    pre = ifelse(length(tmp) == 0, yes = NA, no = median(tmp))
    # Post-statin lab: max delta lab result after statin start but before cutoff
    tmp = unlist(result[date > 0 & !is.na(date)])
    if (length(pre) == 0) {
      post = ifelse(length(tmp) == 0, yes = NA, no = max(tmp))
    } else {
      post = ifelse(length(tmp) == 0,
                    yes = NA,
                    no = tmp[which.max(abs(tmp - pre))])
    }
  } else {
    # Pre-statin lab: median lab result prior to statin start
    tmp = unlist(result[date <= 0 & !is.na(date)])
    pre = ifelse(length(tmp) == 0, yes = NA, no = median(tmp))
    # Post-statin lab: median lab result after statin start but before cutoff
    tmp = unlist(result[date > 0 & !is.na(date)])
    post = ifelse(length(tmp) == 0, yes = NA, no = median(tmp))
  }
  return(c(pre, post))
}

# Define a function to replace a trailing value with the preceding value
replace_trailing = function(vector, replace = NA) {
  if (is.na(replace)) {
    vector_wo_blanks = vector[!is.na(vector)]
    vector[is.na(vector)] = vector_wo_blanks[length(vector_wo_blanks)]
  } else {
    vector_wo_blanks = vector[vector != replace]
    vector[vector == replace] = vector_wo_blanks[length(vector_wo_blanks)]
  }
  return(vector)
}


#########################
# IMPORT PROCESSED DATA #
#########################

bmi = read.csv("../data/compliant/bmi.csv",
               na.strings = "")
df = read.csv("../data/compliant/demographics.csv",
              na.strings = "")
labs = read.csv("../data/compliant/labs.csv",
                na.strings = "")
prescriptions = read.csv("../data/compliant/prescriptions.csv",
                         na.strings = "")


#########################
# DETERMINE TIME CUTOFF #
#########################

# Determine relevant time range for each individual:
# a) first date of FG reading greater than 125,
# b) date of diabetes ICD diagnosis,
# c) date of prescription for diabetes medication, and
# d) date of maximum post-statin FG reading.
fgdate = labs[, grep("^fgdate[[:digit:]]", colnames(labs))]
fg = labs[, grep("^fg[[:digit:]]", colnames(labs))]
fg[is.na(fg) | fgdate < 0] = -1
# We will begin by assigning (d) to all individuals
df$cutoff = fgdate[cbind(1:nrow(df), max.col(fg))]
# Next, we'll overwrite using (c) for individuals prescribed diabetes medication
overwrite = df$diabRX == 1
df$cutoff[overwrite] = df$diabRX_date[overwrite]
df$survival = df$diabRX_date
# Then, we'll overwrite using (b) for individuals with a diabetes ICD diagnosis
overwrite = df$diabICD == 1
df$cutoff[overwrite] = df$diabICD_date[overwrite]
df$survival[overwrite] = df$diabICD_date[overwrite]
# Lastly, we'll overwrite using (a) for individuals with a FG reading > 125
diabFG_date = apply(fg > 125, 1, function(x) match(T, x))
diabFG_date = fgdate[cbind(1:nrow(fgdate), diabFG_date)]
df$cutoff[!is.na(diabFG_date)] = diabFG_date[!is.na(diabFG_date)]
df$survival[!is.na(diabFG_date)] = diabFG_date[!is.na(diabFG_date)]
rm(diabFG_date, overwrite)


###############
# BMI RESULTS #
###############

# Extract dates and results for BMI measurements
bmi = merge(data.frame("id" = df$id), bmi, by = "id", all = T)
bmidate = bmi[, grep("bmidate[[:digit:]]", colnames(bmi))]
bmi = bmi[, grep("bmi[[:digit:]]", colnames(bmi))]

# Define new variables to summarize lab results over time
df$pre_bmi = as.numeric(NA)
df$post_bmi = as.numeric(NA)


#####################
# PRESCRIPTION DATA #
#####################

# Extract columns with data necessary to compute PDD/DDD
# - Number of days supplied for a prescription
day_supply = prescriptions[, grep("day_supply", colnames(prescriptions))]
day_supply[is.na(day_supply)] = 0
# - Generic name of statin prescribed
gennm = prescriptions[, grep("gennm", colnames(prescriptions))]
gennm = data.frame(t(apply(gennm, 1, replace_trailing)))
# - Milligrams of active ingredient per tablet
mgs = prescriptions[, grep("mgs", colnames(prescriptions))]
mgs = data.frame(t(apply(mgs, 1, replace_trailing)))
# - Number of tablets taken per day for a prescription
rxtablets = prescriptions[, grep("rxtablets", colnames(prescriptions))]
rxtablets[is.na(rxtablets)] = 0
# - Prescription days (using dates of pharmacy visits)
scriptdate = prescriptions[, grep("scriptdate", colnames(prescriptions))]
scriptdate = data.frame(t(apply(scriptdate, 1, replace_trailing)))
scriptdate[, ncol(scriptdate) + 1] =
  scriptdate[, ncol(scriptdate)] + day_supply[, ncol(day_supply)]
rm(prescriptions)

# Define a matrix of scaling factors to account for statins' potency differences
# (units: "Defined Daily Doses" per milligram active ingredient)
scaling = data.frame(
  apply(gennm, 2, function(x)
    sapply(x, function(y)
      switch(y,
             "at" = 0.05,
             "fl" = 0.00625,
             "lo" = 0.0125,
             "pi" = 0.25,
             "pr" = 0.0125,
             "ro" = 0.2,
             "si" = 0.025,
             0))))

# Define mgs/day (milligrams of statin consumed per day) at each point in time
mgsday = mgs * rxtablets
mgsday = data.frame(t(apply(mgsday, 1, replace_trailing, 0)))
script_mgs = mgsday
# Define PDD/DDD ("Daily Defined Doses" consumed per day) at each point in time
pdd = mgsday * scaling
pdd = data.frame(t(apply(pdd, 1, replace_trailing, 0)))
script_pdd = pdd
rm(mgs, rxtablets, scaling)

# Determine when individuals changed statin type or dose
changed_rx = data.frame(gennm[, -1] != gennm[, -ncol(gennm)] |
                          pdd[, -1] != pdd[, -ncol(pdd)], F)

# Assign the last statin prescription date as the cutoff for those without one
df$cutoff[is.na(df$cutoff)] = apply(scriptdate, 1, max)[is.na(df$cutoff)]

# Define a new data frame for the number of active days on a prescription
days = data.frame(matrix(0, ncol = ncol(day_supply), nrow = nrow(day_supply)))

# Initiate a data frame for containing information on statin usage
df$mgs_init = as.numeric(NA)
df$pdd_init = as.numeric(NA)
df$type_init = as.character(NA)
df$type_mgs = as.character(NA)
df$type_pdd = as.character(NA)
df$delta_mgs = as.numeric(NA)
df$delta_pdd = as.numeric(NA)
df$type_change = as.logical(NA)
df$type_change_date = as.numeric(NA)
drug_names = c("at", "fl", "lo", "pi", "pr", "ro", "si")


############
# LAB DATA #
############

# Extract the lab results data
ckdate = labs[, grep("^ckdate[[:digit:]]", colnames(labs))]
ck = labs[, grep("^ck[[:digit:]]", colnames(labs))]
fgdate = labs[, grep("^fgdate[[:digit:]]", colnames(labs))]
fg = labs[, grep("^fg[[:digit:]]", colnames(labs))]
hdldate = labs[, grep("^hdldate[[:digit:]]", colnames(labs))]
hdl = labs[, grep("^hdl[[:digit:]]", colnames(labs))]
ldldate = labs[, grep("^ldldate[[:digit:]]", colnames(labs))]
ldl = labs[, grep("^ldl[[:digit:]]", colnames(labs))]
tcdate = labs[, grep("^tcdate[[:digit:]]", colnames(labs))]
tc = labs[, grep("^tc[[:digit:]]", colnames(labs))]
trigdate = labs[, grep("^trigdate[[:digit:]]", colnames(labs))]
trig = labs[, grep("^trig[[:digit:]]", colnames(labs))]
rm(labs)

# Define new variables to summarize lab results over time
df$pre_ck = as.numeric(NA)
df$pre_fg = as.numeric(NA)
df$pre_hdl = as.numeric(NA)
df$pre_ldl = as.numeric(NA)
df$pre_tc = as.numeric(NA)
df$pre_trig = as.numeric(NA)
df$post_ck = as.numeric(NA)
df$post_fg = as.numeric(NA)
df$post_hdl = as.numeric(NA)
df$post_ldl = as.numeric(NA)
df$post_tc = as.numeric(NA)
df$post_trig = as.numeric(NA)
df$post_max_trig = as.numeric(NA) # Looking at `Diabetes ~ TC`
df$max_fg = as.numeric(NA) # Want to know everyone's maximum fasting glucose


#################
# HEAVY LIFTING #
#################

# Here we've consolidated all heavily computational steps into a single for loop
for (i in 1:nrow(df)) {
  
  # Restrict lab results to be before the cutoff date (BMI: within 90 days)
  bmidate[i, bmidate[i,] > df$cutoff[i] + 90 & !is.na(bmidate[i,])] = NA
  ckdate[i, ckdate[i,] > df$cutoff[i] & !is.na(ckdate[i,])] = NA
  fgdate[i, fgdate[i,] > df$cutoff[i] & !is.na(fgdate[i,])] = NA
  hdldate[i, hdldate[i,] > df$cutoff[i] & !is.na(hdldate[i,])] = NA
  ldldate[i, ldldate[i,] > df$cutoff[i] & !is.na(ldldate[i,])] = NA
  tcdate[i, tcdate[i,] > df$cutoff[i] & !is.na(tcdate[i,])] = NA
  trigdate[i, trigdate[i,] > df$cutoff[i] & !is.na(trigdate[i,])] = NA
  
  # Pre- and post-statin test results
  df[i, c("pre_bmi", "post_bmi")] = getlabs(i, "bmi")
  df[i, c("pre_ck", "post_ck")] = getlabs(i, "ck")
  df[i, c("pre_fg", "post_fg")] = getlabs(i, "fg")
  df[i, c("pre_hdl", "post_hdl")] = getlabs(i, "hdl")
  df[i, c("pre_ldl", "post_ldl")] = getlabs(i, "ldl")
  df[i, c("pre_tc", "post_tc")] = getlabs(i, "tc")
  df[i, c("pre_trig", "post_trig")] = getlabs(i, "trig")
  
  # Maximum fasting glucose
  tmp = fgdate[i,] <= df$cutoff[i] & !is.na(fgdate[i,])
  df[i, "max_fg"] = ifelse(sum(tmp) == 0,
                           yes = NA,
                           no = max(fg[i, tmp]))
  
  # Maximum post-statin total cholesterol
  tmp = trigdate[i,] > 0 & !is.na(trigdate[i,])
  df[i, "post_max_trig"] = ifelse(sum(tmp) == 0,
                                  yes = NA,
                                  no = max(trig[i, tmp]))
  
  # Restrict prescription days to the time cutoff for each individual
  scriptdate[i,] = as.integer(ifelse(scriptdate[i,] > df$cutoff[i],
                                     yes = df$cutoff[i],
                                     no = scriptdate[i,]))
  scriptdate[i,] = scriptdate[i, -1] - scriptdate[i, -ncol(scriptdate)]
  
  # Number of active days per statin prescription, given RX dates and supply
  # Assumptions:
  # (a) New prescriptions that feature a change in statin dose/type take effect
  #     immediately, AND all old tablets are discarded
  # (b) New prescriptions that are equivalent in statin dose/type take effect
  #     immediately, BUT old tablets are saved to be used when the individual
  #     runs out of newer tablets
  # (c) If an individual runs out of tablets (including old, saved tablets),
  #     they do not take any statins until their next pharmacy visit
  leftover = 0
  for (j in 1:ncol(days)) {
    if (changed_rx[i, j]) { # (a)
      days[i, j] = scriptdate[i, j]
      leftover = 0
    } else { # (b/c)
      days[i, j] = min(scriptdate[i, j], day_supply[i, j] + leftover)
      if (scriptdate[i, j] < day_supply[i, j] + leftover) { # (b)
        leftover = leftover + (day_supply[i, j] - scriptdate[i, j])
      } else { # (c)
        leftover = 0
      }
    }
  }
  
  # Determine changes in unadjusted and adjusted exposure per day
  if (all(days[i,] == 0)) {
    df$delta_mgs[i] = NA
    df$delta_pdd[i] = NA
  } else {
    df$delta_mgs[i] = unlist(rev(mgsday[i, days[i,] != 0])[1] - mgsday[i, 1])
    df$delta_pdd[i] = unlist(rev(pdd[i, days[i,] != 0])[1] - pdd[i, 1])
  }
  
  # Determine the unadjusted and adjusted exposure per prescription
  script_mgs[i,] = mgsday[i,] * days[i,]
  script_pdd[i,] = pdd[i,] * days[i,]
  
  # Initial statin dose/type
  tmp = unlist(days[i,]) != 0
  if (sum(tmp) == 0) {
    df$mgs_init[i] = NA
    df$pdd_init[i] = NA
    df$type_init[i] = NA
  } else {
    df$mgs_init[i] = unlist(mgsday[i, tmp][1])
    df$pdd_init[i] = unlist(pdd[i, tmp][1])
    df$type_init[i] = unlist(gennm[i, tmp][1])
  }
  
  # Cumulative statin use (milligrams - unadjusted for potency)
  tmp = sapply(drug_names, function(x)
    ifelse(length(unlist(script_mgs[i, gennm[i,] == x])) == 0,
           yes = 0,
           no = sum(script_mgs[i, gennm[i,] == x])))
  statin1 = drug_names[which.max(tmp)]
  statin2 = drug_names[8 - which.max(rev(tmp))]
  if(max(tmp) == 0) {
    df$type_mgs[i] = NA
  } else if (sum(tmp > 0) > 1) {
    df$type_mgs[i] = ifelse(statin1 != statin2,
                            yes = paste(statin1, "&", statin2),
                            no = statin1)
  } else {
    df$type_mgs[i] = statin1
  }
  
  # Cumulative statin use (PDD/DDD - adjusted for potency)
  tmp = sapply(drug_names, function(x)
    ifelse(length(unlist(script_pdd[i, gennm[i,] == x])) == 0,
           yes = 0,
           no = sum(script_pdd[i, gennm[i,] == x])))
  statin1 = drug_names[which.max(tmp)]
  statin2 = drug_names[8 - which.max(rev(tmp))]
  if(max(tmp) == 0) {
    df$type_pdd[i] = NA
    df$type_change[i] = NA
    df$type_change_date[i] = NA
  } else if (sum(tmp > 0) > 1) {
    df$type_pdd[i] = ifelse(statin1 != statin2,
                            yes = paste(statin1, "&", statin2),
                            no = statin1)
    df$type_change[i] = 1
    df$type_change_date[i] =
      cumsum(unlist(days[i,]))[match(T, gennm[i, -ncol(gennm)] != gennm[i, -1])]
  } else {
    df$type_pdd[i] = statin1
    df$type_change[i] = 0
    df$type_change_date[i] = NA
  }
  
}


#########################
# STATIN USE PROPERTIES #
#########################

# Compute an average milligrams of statin taken per day over time
df$tse_mgs = apply(script_mgs, 1, sum)
df$avg_mgs = df$tse_mgs / df$cutoff
# Compute an average PDD/DDD ratio to summarize individuals' statin usage over
# the relevant time range (units: "Daily Defined Doses" per day)
df$tse_pdd = apply(script_pdd, 1, sum)
df$avg_pdd = df$tse_pdd / df$cutoff


###########################
# SECOND-DEGREE VARIABLES #
###########################

# Diabetes via any method of diagnosis
df$diabANY = ifelse(df$diabFG | df$diabICD | df$diabRX, yes = 1, no = 0)

# Changes in BMI and lab results
df$delta_bmi = df$post_bmi - df$pre_bmi
df$delta_ck = df$post_ck - df$pre_ck
df$delta_fg = df$post_fg - df$pre_fg
df$delta_hdl = df$post_hdl - df$pre_hdl
df$delta_ldl = df$post_ldl - df$pre_ldl
df$delta_tc = df$post_tc - df$pre_tc
df$delta_trig = df$post_trig - df$pre_trig

# Numbers of CK tests, which are requested only for at-risk patients
df$num_pre_ck = apply(ckdate, 1, function(x) sum(x <= 0, na.rm = T))
df$num_post_ck = apply(ckdate, 1, function(x) sum(x > 0, na.rm = T))
df$num_ck = apply(ckdate, 1, function(x) sum(!is.na(x)))

# Whether an individual met their lipid lowering goals
df$met_ldl_goal = ifelse(df$post_ldl < 100, yes = 1, no = 0)

# Variables to check whether PDD is different for users of different statins
df$avg_pdd_lova = df$avg_pdd
df$avg_pdd_lova[df$type_pdd != "lo"] = 0
df$avg_pdd_other = df$avg_pdd
df$avg_pdd_other[df$type_pdd %in% c("lo", "si")] = 0


######################
# VARIABLE SELECTION #
######################

df = df[, c(
  
  # Bookkeeping and demographics
  "id", "sex", "age", "race", "hispanic",
  
  # Diabetes diagnosis
  "diabFG", "diabICD", "diabRX", "diabANY",
  
  # Time range information
  "cutoff", "survival",
  
  # BMI information
  "pre_bmi", "post_bmi", "delta_bmi",
  
  # Lab information
  "pre_ck", "post_ck", "delta_ck", "num_pre_ck", "num_post_ck", "num_ck",
  "pre_fg", "post_fg", "delta_fg", "max_fg",
  "pre_hdl", "post_hdl", "delta_hdl",
  "pre_ldl", "post_ldl", "delta_ldl", "met_ldl_goal",
  "pre_tc", "post_tc", "delta_tc",
  "pre_trig", "post_trig", "post_max_trig", "delta_trig",
  
  # Statin dose/type information
  "mgs_init", "pdd_init", "type_init",
  "tse_mgs", "avg_mgs", "delta_mgs", "type_mgs",
  "tse_pdd", "avg_pdd", "delta_pdd", "type_pdd",
  "avg_pdd_lova", "avg_pdd_other",
  "type_change", "type_change_date",
  
  # Thyroid-stimulating hormone
  "tsh"
  
)]

# Save our data to a .csv file
save.image(file = "../data/processed/workspace.RData")
write.csv(df, file = "../data/processed/data.csv", na = "", row.names = F)
