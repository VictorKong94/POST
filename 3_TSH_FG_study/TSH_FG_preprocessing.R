# Setup
options(stringsAsFactors = F)

# Read in glucose and TSH test data
glucoseCase = read.csv("../data/case_glucose.csv")
glucoseCntl = read.csv("../data/cntl_glucose.csv")
tshCase = read.csv("../data/case_TSH.csv")
tshCntl = read.csv("../data/cntl_TSH.csv")
rxCase = read.csv("../data/case_all_rx_processed.csv")
rxCntl = read.csv("../data/cntl_all_rx_processed.csv")

# Compile data frame that contains all relevant information:
# - id: the patient's specific ID number,
# - cohort: the cohort to which the patient belongs,
# - tshResult: the patient's TSH test result (mU/L),
# - avgBaseFG: the patient's average pre-Statin FG
# - deltaFG: difference between mean poststatin FG and mean prestatin FG,
# - maxDelta: difference between highest poststatin FG and mean prestatin FG,
# - sex: patient's sex
# - age: patient's age in years
# - race: race with which patient identifies
# - hispanic: whether patient identifies as hispanic
# - diabetes: whether a patient was diagnosed as diabetic
# - diagFromFG: whether a patient was diagnosed as diabetic by the FG method
# - diagFromICD: whether a patient was diagnosed as diabetic by ICD standards
# - group: code that corresponds categories of diabetic, prediabetic, etc.
data = data.frame(id = integer(0),
                  cohort = character(0),
                  tshResult = numeric(0),
                  statinType = character(0),
                  avgBaseFG = numeric(0),
                  deltaFG = integer(0),
                  maxDelta = integer(0),
                  sex = character(0),
                  age = integer(0),
                  race = character(0),
                  hispanic = character(0),
                  diabetes = integer(0),
                  diagFromFG = integer(0),
                  diagFromICD = integer(0),
                  group = character(0))
for (id in sort(tshCntl$id)) {
  data[nrow(data) + 1,] = c(id,
                            "control",
                            tshCntl$tshresult[tshCntl$id == id],
                            glucoseCntl$GENERIC_NAME[glucoseCntl$id == id],
                            glucoseCntl$avepre_gluresult[glucoseCntl$id == id],
                            glucoseCntl$deltafg[glucoseCntl$id == id],
                            glucoseCntl$maxdelta[glucoseCntl$id == id],
                            glucoseCntl$PATIENT_SEX[glucoseCntl$id == id],
                            glucoseCntl$PATIENT_AGE[glucoseCntl$id == id],
                            glucoseCntl$CATEGORY.OF.KPNC.RACE[
                              glucoseCntl$id == id],
                            glucoseCntl$Is.Member.Hispanic...KPNC.variable.[
                              glucoseCntl$id == id],
                            0,
                            0,
                            0,
                            3)
}
for (id in sort(tshCase$id)) {
  data[nrow(data) + 1,] = c(200000 + id,
                            "case",
                            tshCase$tshresult[tshCase$id == id],
                            "",
                            glucoseCase$avepre_gluresult[glucoseCase$id == id],
                            glucoseCase$deltafg[glucoseCase$id == id],
                            glucoseCase$maxdelta[glucoseCase$id == id],
                            glucoseCase$PATIENT_SEX[glucoseCase$id == id],
                            glucoseCase$PATIENT_AGE[glucoseCase$id == id],
                            glucoseCase$CATEGORY.OF.KPNC.RACE[
                              glucoseCase$id == id],
                            glucoseCase$Is.Member.Hispanic...KPNC.variable.[
                              glucoseCase$id == id],
                            glucoseCase$diab.comb[glucoseCase$id == id],
                            glucoseCase$diab.from.FG[glucoseCase$id == id],
                            glucoseCase$diab.from.ICD[glucoseCase$id == id],
                            glucoseCase$group[glucoseCase$id == id])
}
data$diabetes = data$diabetes == 1
data$diagFromFG = data$diagFromFG == 1
data$diagFromICD = data$diagFromICD == 1
data$pdd = NA
for (id in rxCntl$IDs) {
  data$pdd[data$id == id] = rxCntl$PDD.DDD[rxCntl$IDs == id]
}
for (id in rxCase$IDs) {
  data$statinType[data$id == id] = rxCase$Primary.Drug[rxCase$IDs == id]
  data$pdd[data$id == id] = rxCase$PDD.DDD[rxCase$IDs == id]
}
data$hypothyroidism = data$tshResult > 5.5

write.csv(data, "../data/TSH_FG_data.csv")
