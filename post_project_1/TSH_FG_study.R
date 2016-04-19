# Read in glucose and TSH test data
glucose_case = read.csv("diab_CASE_glucose.csv")
glucose_cntl = read.csv("diab_CNTL_glucose.csv")
tsh_case = read.csv("diab_CASE_TSH.csv")
tsh_cntl = read.csv("diab_CNTL_TSH.csv")

# Compile data frame that contains all relevant information:
# - id: the patient's specific ID number,
# - group: the cohort to which the patient belongs,
# - tsh_result: the patient's TSH test result (mU/L),
# - deltafg: difference between mean poststatin FG and mean prestatin FG,
# - maxdelta: difference between highest poststatin FG and mean prestatin FG,
# - max2delta: difference between mean of two highest poststatin FGs and mean
#              prestatin FG
data = data.frame(id = integer(0),
                  group = factor(NULL, levels = c("case", "cntl")),
                  tsh_result = numeric(0),
                  deltafg = numeric(0),
                  maxdelta = numeric(0),
                  max2delta = numeric(0))
for (id in tsh_case$id) {
  data[nrow(data) + 1,] = c(id,
                            "case",
                            tsh_case$tshresult[tsh_case$id == id],
                            glucose_case$deltafg[glucose_case$id == id],
                            glucose_case$maxdelta[glucose_case$id == id])
}
for (id in tsh_cntl$id) {
  data[nrow(data) + 1,] = c(id,
                            "cntl",
                            tsh_cntl$tshresult[tsh_cntl$id == id],
                            glucose_cntl$deltafg[glucose_cntl$id == id],
                            glucose_cntl$maxdelta[glucose_cntl$id == id])
}
data$hypothyroidism = data$tsh_result > 5.5
