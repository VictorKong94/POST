######################
# Load relevant data #
######################

case = read.csv("data/CASE_all_rx_processed.csv")
cntl = read.csv("data/CNTL_all_rx_processed.csv")

################################
# Compute number of Rx changes #
################################

case_changes = data.frame("IDs" = case$IDs,
                          "PDD" = case$PDD.DDD.Factor,
                          "init_statin" = case$Initial.Drug,
                          "type_change" = apply(case[10:16] != 0, 1, sum) > 1,
                          "dose_change" = case$PDD.DDD.Changes > 0)
case_changes$both_change = case_changes$type_change & case_changes$dose_change

cntl_changes = data.frame("IDs" = cntl$IDs,
                          "PDD" = cntl$PDD.DDD.Factor,
                          "init_statin" = cntl$Initial.Drug,
                          "type_change" = apply(cntl[10:16] != 0, 1, sum) > 1,
                          "dose_change" = cntl$PDD.DDD.Changes > 0)
cntl_changes$both_change = cntl_changes$type_change & cntl_changes$dose_change

#######################################
# Partition by PDD and initial statin #
#######################################

case_by_subsets = data.frame(
  "PDD" = factor(NULL, levels = levels(case_changes$PDD)),
  "init_statin" = factor(NULL, levels = levels(case_changes$init_statin)),
  "type_change" = integer(0),
  "dose_change" = integer(0),
  "both_change" = integer(0))
for (PDD in levels(case_by_subsets$PDD)) {
  for (statin in levels(case_by_subsets$init_statin)) {
    case_by_subsets[nrow(case_by_subsets) + 1,] =
      c(PDD,
        statin,
        sum(case_changes$type_change[case_changes$PDD == PDD &
                                       case_changes$init_statin == statin]),
        sum(case_changes$dose_change[case_changes$PDD == PDD &
                                       case_changes$init_statin == statin]),
        sum(case_changes$both_change[case_changes$PDD == PDD &
                                       case_changes$init_statin == statin]))
  }
}
class(case_by_subsets$type_change) = "integer"
class(case_by_subsets$dose_change) = "integer"
class(case_by_subsets$both_change) = "integer"

cntl_by_subsets = data.frame(
  "PDD" = factor(NULL, levels = levels(cntl_changes$PDD)),
  "init_statin" = factor(NULL, levels = levels(cntl_changes$init_statin)),
  "type_change" = integer(0),
  "dose_change" = integer(0),
  "both_change" = integer(0))
for (PDD in levels(cntl_by_subsets$PDD)) {
  for (statin in levels(cntl_by_subsets$init_statin)) {
    cntl_by_subsets[nrow(cntl_by_subsets) + 1,] =
      c(PDD,
        statin,
        sum(cntl_changes$type_change[cntl_changes$PDD == PDD &
                                       cntl_changes$init_statin == statin]),
        sum(cntl_changes$dose_change[cntl_changes$PDD == PDD &
                                       cntl_changes$init_statin == statin]),
        sum(cntl_changes$both_change[cntl_changes$PDD == PDD &
                                       cntl_changes$init_statin == statin]))
  }
}
class(cntl_by_subsets$type_change) = "integer"
class(cntl_by_subsets$dose_change) = "integer"
class(cntl_by_subsets$both_change) = "integer"

#########################################
# Create spreadsheets for easy analysis #
#########################################

case_type_change = with(case_by_subsets, {
  out = matrix(nrow = nlevels(PDD), ncol = nlevels(init_statin),
               dimnames = list(levels(PDD), levels(init_statin)))
  out[cbind(PDD, init_statin)] = type_change
  out
})
case_type_change = cbind(case_type_change,
                         "Total" = apply(case_type_change, 1, sum))
case_type_change = rbind(case_type_change,
                         "Total" = apply(case_type_change, 2, sum))
write.csv(case_type_change, file = "data/case_type_change.csv")

case_dose_change = with(case_by_subsets, {
  out = matrix(nrow = nlevels(PDD), ncol = nlevels(init_statin),
               dimnames = list(levels(PDD), levels(init_statin)))
  out[cbind(PDD, init_statin)] = dose_change
  out
})
case_dose_change = cbind(case_dose_change,
                         "Total" = apply(case_dose_change, 1, sum))
case_dose_change = rbind(case_dose_change,
                         "Total" = apply(case_dose_change, 2, sum))
write.csv(case_dose_change, file = "data/case_dose_change.csv")

case_both_change = with(case_by_subsets, {
  out = matrix(nrow = nlevels(PDD), ncol = nlevels(init_statin),
               dimnames = list(levels(PDD), levels(init_statin)))
  out[cbind(PDD, init_statin)] = both_change
  out
})
case_both_change = cbind(case_both_change,
                         "Total" = apply(case_both_change, 1, sum))
case_both_change = rbind(case_both_change,
                         "Total" = apply(case_both_change, 2, sum))
write.csv(case_both_change, file = "data/case_both_change.csv")

cntl_type_change = with(cntl_by_subsets, {
  out = matrix(nrow = nlevels(PDD), ncol = nlevels(init_statin),
               dimnames = list(levels(PDD), levels(init_statin)))
  out[cbind(PDD, init_statin)] = type_change
  out
})
cntl_type_change = cbind(cntl_type_change,
                         "Total" = apply(cntl_type_change, 1, sum))
cntl_type_change = rbind(cntl_type_change,
                         "Total" = apply(cntl_type_change, 2, sum))
write.csv(cntl_type_change, file = "data/cntl_type_change.csv")

cntl_dose_change = with(cntl_by_subsets, {
  out = matrix(nrow = nlevels(PDD), ncol = nlevels(init_statin),
               dimnames = list(levels(PDD), levels(init_statin)))
  out[cbind(PDD, init_statin)] = dose_change
  out
})
cntl_dose_change = cbind(cntl_dose_change,
                         "Total" = apply(cntl_dose_change, 1, sum))
cntl_dose_change = rbind(cntl_dose_change,
                         "Total" = apply(cntl_dose_change, 2, sum))
write.csv(cntl_dose_change, file = "data/cntl_dose_change.csv")

cntl_both_change = with(cntl_by_subsets, {
  out = matrix(nrow = nlevels(PDD), ncol = nlevels(init_statin),
               dimnames = list(levels(PDD), levels(init_statin)))
  out[cbind(PDD, init_statin)] = both_change
  out
})
cntl_both_change = cbind(cntl_both_change,
                         "Total" = apply(cntl_both_change, 1, sum))
cntl_both_change = rbind(cntl_both_change,
                         "Total" = apply(cntl_both_change, 2, sum))
write.csv(cntl_both_change, file = "data/cntl_both_change.csv")
