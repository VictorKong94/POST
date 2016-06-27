all = read.csv("../data/TSH_FG_data.csv")
all$id = NULL
all = all[all$sex != "",]
all$female = all$sex == "F"
all = all[all$race %in% c("WH", "AS", "BA", "MU"),]
all$WH = all$race == "WH"
all$AS = all$race == "AS"
all$BA = all$race == "BA"
all$MU = all$race == "MU"
all = all[all$hispanic != "",]
all$hispanic = all$hispanic == "Y"
case = all[all$cohort == "case",]

# Differences Between Cohorts
hypo_cohort = chisq.test(table(all$hypothyroidism, all$cohort))
tsh_cohort = summary(glm(tshResult ~ cohort,
                     family = gaussian(link = "identity"),
                     data = all))

# Endpoint diabetes (Includes both those diagnosed by FG and ICD)
summary(glm(diabetes ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all))
summary(glm(diabetes ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all))
summary(glm(diabetes ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case))
summary(glm(diabetes ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case))

# Endpoint diabetes from fasting glucose
summary(glm(diagFromFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all))
summary(glm(diagFromFG ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all))
summary(glm(diagFromFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case))
summary(glm(diagFromFG ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case))

# Endpoint diabetes from ICD
summary(glm(diagFromICD ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all))
summary(glm(diagFromICD ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all))
summary(glm(diagFromICD ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case))
summary(glm(diagFromICD ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case))

# Endpoint deltaFG
summary(glm(deltaFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all))
summary(glm(deltaFG ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all))
summary(glm(deltaFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case))
summary(glm(deltaFG ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case))

# Endpoint maxDelta
summary(glm(maxDelta ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all))
summary(glm(maxDelta ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all))
summary(glm(maxDelta ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case))
summary(glm(maxDelta ~ hypothyroidism + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case))

# Higher tshResult ~ higher deltaFG, which is consistent with report direction.

# Follow-Up Split by Gender
summary(glm(deltaFG ~ tshResult + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all[!all$female,]))
summary(glm(deltaFG ~ tshResult + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all[all$female,]))

# Looks like we can somewhat predict deltaFG for males from tshResult,
# but not for females.