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
chisq.test(table(all$hypoT4, all$cohort)) # P = 0.5331
summary(glm(hypoT4 ~ cohort + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.79849
summary(glm(tshResult ~ cohort + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all)) # P = 0.78956

# Endpoint diabetes (Includes both those diagnosed by FG and ICD)
summary(glm(diabetes ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.37321
summary(glm(diabetes ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.88173
summary(glm(diabetes ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case)) # P = 0.3942
summary(glm(diabetes ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case)) # P = 0.9386

# Endpoint diabetes from fasting glucose
summary(glm(diagFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.3990
summary(glm(diagFG ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.9843
summary(glm(diagFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case)) # P = 0.4150
summary(glm(diagFG ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case)) # P = 0.9930

# Endpoint diabetes from ICD
summary(glm(diagICD ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.1451
summary(glm(diagICD ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = all)) # P = 0.95672
summary(glm(diagICD ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case)) # P = 0.1752
summary(glm(diagICD ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = binomial(link = "logit"),
            data = case)) # P = 0.8860

# Endpoint deltaFG
summary(glm(deltaFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all)) # P = 0.201389
summary(glm(deltaFG ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all)) # P = 0.575875
summary(glm(deltaFG ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case)) # P = 0.2283
summary(glm(deltaFG ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case)) # P = 0.6154

# Endpoint maxDelta
summary(glm(maxDelta ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all)) # P = 0.351387
summary(glm(maxDelta ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all)) # P = 0.629128
summary(glm(maxDelta ~ tshResult + female + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case)) # P = 0.558049
summary(glm(maxDelta ~ hypoT4 + female + age + WH + AS +BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = case)) # P = 0.555348

# Higher tshResult ~ higher deltaFG; consistent with reported direction.

# Follow-Up Split by Gender
summary(glm(deltaFG ~ tshResult + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all[!all$female,])) # P = 0.052887
summary(glm(deltaFG ~ tshResult + age + WH + AS + BA + MU + hispanic,
            family = gaussian(link = "identity"),
            data = all[all$female,])) # P = 0.918

# Looks like we can somewhat predict deltaFG for males from tshResult,
# but not for females.
