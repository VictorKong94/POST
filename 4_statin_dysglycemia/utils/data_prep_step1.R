#########
# SETUP #
#########

# Do NOT assume strings belong to factor variables
options(stringsAsFactors = F)


###################
# BMI INFORMATION #
###################

# Import raw data
bmi = read.csv("../data/raw/bmi.csv",
               na.strings = "")

# Pull important data
id = bmi$id
statin_date = as.Date(bmi$statin_date,
                      format = "%m/%d/%y",
                      origin = "01/01/00")
bmidate = bmi[, grep("MEASURE_DATE", colnames(bmi))]
for (j in 1:ncol(bmidate)) {
  bmidate[, j] = as.numeric(as.Date(bmidate[, j],
                                    format = "%m/%d/%y",
                                    origin = "01/01/00") - statin_date)
}
colnames(bmidate) = paste0("bmidate", 1:ncol(bmidate))
bmi = bmi[, grep("BMI", colnames(bmi))]
colnames(bmi) = paste0("bmi", 1:ncol(bmi))

# Select columns
bmi = cbind(id,
            bmidate, bmi)
cols = c("id",
         paste0(c("bmidate", "bmi"), rep(1:ncol(bmidate), each = 2)))
bmi = bmi[, cols]
write.csv(bmi,
          file = "../data/compliant/bmi.csv",
          na = "",
          row.names = F)
rm(list = ls())


############################
# DEMOGRAPHICS INFORMATION #
############################

# Import raw data
demographics = read.csv("../data/raw/demographics.csv",
                        na.strings = "")

# Pull important data
statin_start = as.Date(demographics$statin_start,
                       format = "%m/%d/%y",
                       origin = "01/01/00")

# Modify existing variables
demographics$diabICD_date =
  as.numeric(as.Date(demographics$diabICD_date,
                     format = "%m/%d/%y",
                     origin = "01/01/00") - statin_start)
demographics$diabRX_date =
  as.numeric(as.Date(demographics$diabRX_date,
                     format = "%m/%d/%y",
                     origin = "01/01/00") - statin_start)
demographics$hispanic[demographics$hispanic == "U"] = "N"
demographics$statin_start = NULL

# Write to file
write.csv(demographics,
          file = "../data/compliant/demographics.csv",
          na = "",
          row.names = F)
rm(list = ls())

###################
# LAB INFORMATION #
###################

# Import raw data
labs = read.csv("../data/raw/labs.csv",
                na.strings = "")

# Pull important data
id = labs$id
statin_strt = as.Date(labs$statin_strt,
                      format = "%m/%d/%y",
                      origin = "01/01/00")
ckresult = labs[, grep("CK_RESULT", colnames(labs))]
fgresult = labs[, grep("FG_RESULT", colnames(labs))]
hdlresult = labs[, grep("HDL_RESULT", colnames(labs))]
labdate = labs[, grep("LABDATE", colnames(labs))]
ldlresult = labs[, grep("LDL_RESULT", colnames(labs))]
tcresult = labs[, grep("TC_RESULT", colnames(labs))]
trigresult = labs[, grep("TRIG_RESULT", colnames(labs))]
for (j in 1:ncol(labdate)) {
  labdate[, j] = as.numeric(as.Date(labdate[, j],
                                    format = "%m/%d/%y",
                                    origin = "01/01/00") - statin_strt)
}
rm(labs, statin_strt)

# CK
L = max(apply(ckresult, 1, function(x) sum(!is.na(x))))
ck = ckresult[, 1:L]
ckdate = ck
for (i in 1:nrow(ckresult)) {
  temp = ckresult[i, !is.na(ckresult[i,])]
  ck[i,] = c(temp, rep(NA, L - length(temp)))
  temp = labdate[i, !is.na(ckresult[i,])]
  ckdate[i,] = c(temp, rep(NA, L - length(temp)))
}
colnames(ck) = gsub("CK_RESULT", "ck", colnames(ck))
colnames(ckdate) = gsub("CK_RESULT", "ckdate", colnames(ckdate))
rm(ckresult)

# Fasting glucose
fgresult[fgresult > 600] = NA # Drop a single FG reading greater than 600
L = max(apply(fgresult, 1, function(x) sum(!is.na(x))))
fg = fgresult[, 1:L]
fgdate = fg
for (i in 1:nrow(fgresult)) {
  temp = fgresult[i, !is.na(fgresult[i,])]
  fg[i,] = c(temp, rep(NA, L - length(temp)))
  temp = labdate[i, !is.na(fgresult[i,])]
  fgdate[i,] = c(temp, rep(NA, L - length(temp)))
}
colnames(fg) = gsub("FG_RESULT", "fg", colnames(fg))
colnames(fgdate) = gsub("FG_RESULT", "fgdate", colnames(fgdate))
rm(fgresult)

# HDL
L = max(apply(hdlresult, 1, function(x) sum(!is.na(x))))
hdl = hdlresult[, 1:L]
hdldate = hdl
for (i in 1:nrow(hdlresult)) {
  temp = hdlresult[i, !is.na(hdlresult[i,])]
  hdl[i,] = c(temp, rep(NA, L - length(temp)))
  temp = labdate[i, !is.na(hdlresult[i,])]
  hdldate[i,] = c(temp, rep(NA, L - length(temp)))
}
colnames(hdl) = gsub("HDL_RESULT", "hdl", colnames(hdl))
colnames(hdldate) = gsub("HDL_RESULT", "hdldate", colnames(hdldate))
rm(hdlresult)

# LDL
L = max(apply(ldlresult, 1, function(x) sum(!is.na(x))))
ldl = ldlresult[, 1:L]
ldldate = ldl
for (i in 1:nrow(ldlresult)) {
  temp = ldlresult[i, !is.na(ldlresult[i,])]
  ldl[i,] = c(temp, rep(NA, L - length(temp)))
  temp = labdate[i, !is.na(ldlresult[i,])]
  ldldate[i,] = c(temp, rep(NA, L - length(temp)))
}
colnames(ldl) = gsub("LDL_RESULT", "ldl", colnames(ldl))
colnames(ldldate) = gsub("LDL_RESULT", "ldldate", colnames(ldldate))
rm(ldlresult)

# Total cholesterol
L = max(apply(tcresult, 1, function(x) sum(!is.na(x))))
tc = tcresult[, 1:L]
tcdate = tc
for (i in 1:nrow(tcresult)) {
  temp = tcresult[i, !is.na(tcresult[i,])]
  tc[i,] = c(temp, rep(NA, L - length(temp)))
  temp = labdate[i, !is.na(tcresult[i,])]
  tcdate[i,] = c(temp, rep(NA, L - length(temp)))
}
colnames(tc) = gsub("TC_RESULT", "tc", colnames(tc))
colnames(tcdate) = gsub("TC_RESULT", "tcdate", colnames(tcdate))
rm(tcresult)

# Triglycerides
L = max(apply(trigresult, 1, function(x) sum(!is.na(x))))
trig = trigresult[, 1:L]
trigdate = trig
for (i in 1:nrow(trigresult)) {
  temp = trigresult[i, !is.na(trigresult[i,])]
  trig[i,] = c(temp, rep(NA, L - length(temp)))
  temp = labdate[i, !is.na(trigresult[i,])]
  trigdate[i,] = c(temp, rep(NA, L - length(temp)))
}
colnames(trig) = gsub("TRIG_RESULT", "trig", colnames(trig))
colnames(trigdate) = gsub("TRIG_RESULT", "trigdate", colnames(trigdate))
rm(trigresult)

# Select columns to include
labs = cbind(id,
             ckdate, ck,
             fgdate, fg,
             hdldate, hdl,
             ldldate, ldl,
             tcdate, tc,
             trigdate, trig)
cols = c("id",
         paste0(c("ckdate", "ck"), rep(1:ncol(ck), each = 2)),
         paste0(c("fgdate", "fg"), rep(1:ncol(fg), each = 2)),
         paste0(c("hdldate", "hdl"), rep(1:ncol(hdl), each = 2)),
         paste0(c("ldldate", "ldl"), rep(1:ncol(ldl), each = 2)),
         paste0(c("tcdate", "tc"), rep(1:ncol(tc), each = 2)),
         paste0(c("trigdate", "trig"), rep(1:ncol(trig), each = 2)))
labs = labs[, cols]
write.csv(labs,
          file = "../data/compliant/labs.csv",
          na = "",
          row.names = F)
rm(list = ls())


#####################
# PRESCRIPTION DATA #
#####################

# Import raw data
prescriptions = read.csv("../data/raw/prescriptions.csv",
                         na.strings = "")

# Pull important data
id = prescriptions$id
statin_date = as.Date(prescriptions$statin_date,
                      format = "%m/%d/%y",
                      origin = "01/01/00")
day_supply = prescriptions[, grep("day_supply", colnames(prescriptions))]
gennm = prescriptions[, grep("gennm", colnames(prescriptions))]
rx_qty = prescriptions[, grep("rx_qty", colnames(prescriptions))]
scriptdate = prescriptions[, grep("scriptdate", colnames(prescriptions))]
for (j in 1:ncol(scriptdate)) {
  scriptdate[, j] = as.numeric(as.Date(scriptdate[, j],
                                       format = "%m/%d/%y",
                                       origin = "01/01/00") - statin_date)
}
rm(j, prescriptions, statin_date)

# Compute the number of tablets taken per day for each prescription
rxtablets = rx_qty / day_supply
for (j in 1:ncol(rxtablets)) {
  # Assign "0" to 4537 nonstandard rxtablets values
  temp = rxtablets[, j]
  temp[!is.na(temp) & !(temp %in% c(0.5, 1, 1.5, 2, 3, 4))] = 1
  rxtablets[, j] = temp
}
colnames(rxtablets) = paste0("rxtablets", 1:ncol(rxtablets))
rm(j, rx_qty, temp)

# Extract statin type and weight information
mgs = gennm
gennm1 = gennm
for (j in 1:ncol(gennm)) {
  mgs[, j] = as.numeric(gsub("[^[:digit:]]", "", gennm[, j]))
  gennm1[, j] = tolower(substr(gennm[, j], 1, 2))
}
colnames(mgs) = paste0("mgs", 1:ncol(mgs))

# Select columns to include
prescriptions = cbind(id,
                      day_supply,
                      gennm,
                      mgs,
                      rxtablets,
                      scriptdate)
cols = c("id",
         paste0(c("scriptdate", "gennm", "mgs", "rxtablets", "day_supply"),
                rep(1:ncol(scriptdate), each = 5)))
prescriptions = prescriptions[, cols]
write.csv(prescriptions,
          file = "../data/compliant/prescriptions.csv",
          na = "",
          row.names = F)
rm(list = ls())
