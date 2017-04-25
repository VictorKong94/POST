###################################
# METHOD AND PARAMETER DEFINITION #
###################################

require("ggplot2")
require("ggfortify")
require("plyr")
require("survival")

# Compute percentage breakdowns for a given variable
tab = function(variable, data = df) {
  table(data[, variable])
}

# Method to build, summarize, and assess our logistic regression model
log_reg = function(formula, data = df) {
  model = glm(formula, family = binomial(link = "logit"), data = data)
  list("model" = model,
       "summary" = summary(model),
       "anova" = anova(model, test = "Chisq"))
}

# Method to assess relationship between relationship to survival time
lin_reg = function(formula, data = df) {
  model = lm(formula, data = data)
  list("model" = model,
       "summary" = summary(model),
       "anova" = anova(model, test = "Chisq"))
}

# Method to perform a two-sample t-test
t_test = function(variable, split, data = df) {
  data = data[!is.na(data[, split]),]
  if (length(unique(data[, split])) != 2) {
    stop("Variable must have only two levels")
  }
  var.test(data[data[, split] == unique(data[, split])[1], variable],
           data[data[, split] == unique(data[, split])[2], variable])$p.value
}

# Method to compute aggregate statistics quickly
agg = function(variable, data = df) {
  report = suppressWarnings(
    aggregate(df[setdiff(colnames(data), "id")],
              by = list(data[, variable]),
              FUN = function(x) mean(x, na.rm = T))
  )
  if (length(setdiff(unique(data[, variable]), NA)) == 2) {
    drop = switch(variable,
                  "changed_statin_type" = "days_before_statin_change",
                  "decreased_pdd" = "increased_pdd",
                  "diabFG" = "survival",
                  "diabComb" = "survival",
                  "increased_pdd" = "decreased_pdd",
                  NA)
    report = rbind(
      report,
      sapply(setdiff(colnames(data), c("id", variable, drop)), function(x)
        suppressWarnings(t_test(variable = x, split = variable, data = data))
      )
    )
    report[3, 1] = "p-value"
  }
  colnames(report)[1] = variable
  report[, unique(colnames(report))]
}

# Method to make barplots from data
bar_plot = function(variable, group_by, data = df) {
  summarize = function(x, column) {
    sub = x[, column]
    c(mean = mean(sub, na.rm = T),
      se = sd(sub, na.rm = T) / sqrt(sum(!is.na(sub))))
  }
  ds = ddply(data, group_by, .fun = summarize, variable)
  ds = rename(ds, c("mean" = variable))
  ggplot(ds, aes(x = get(group_by), y = get(variable), fill = get(group_by))) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = get(variable) - se, ymax = get(variable) + se),
                  width = .1, position = position_dodge(.9)) +
    labs(x = group_by, y = variable) +
    theme(legend.position = "none")
}

# Method to make boxplots from data
box_plot = function(variable, group_by, data = df) {
  ggplot(data, aes(x = get(group_by), y = get(variable), alpha = .1)) +
    geom_boxplot(outlier.size = .5) +
    labs(x = group_by, y = variable) + 
    theme(legend.position = "none")
}

# Method to make scatterplots from data
survival = function(variable, CI = F, data = df) {
  tmp = data.frame(variable = data[, variable],
                   "survival" = data$survival,
                   "censor" = !is.na(data$survival))
  tmp$variable = droplevels(tmp$variable)
  tmp$survival[is.na(tmp$survival)] = max(tmp$survival, na.rm = T)
  fit = survfit(Surv(survival, censor) ~ variable,
                data = tmp,
                conf.type = "log-log")
  autoplot(fit, fun = "event", conf.int = CI, censor = F) +
    labs(x = "days after start of statins", y = "fasting glucose > 126",
         color = variable)
}

# Make sure we're omitting missing values from individual analyses
options(na.action = "na.omit")