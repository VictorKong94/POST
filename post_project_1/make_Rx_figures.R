# Import data
cntl = read.csv('data/cntl_all_rx_processed.csv')
case = read.csv('data/case_all_rx_processed.csv')

# Histogram
hist(cntl$PDD.DDD, col = rgb(1, 0, 0, .5), breaks = 15,
     main = 'PDD/DDD in case vs. Control',
     xlab = 'PDD/DDD (DDD/day)')
hist(case$PDD.DDD, col = rgb(0, 0, 1, .5), breaks = 15, add = TRUE)
legend(x = 'topright', legend = c('Control', 'case'),
       fill = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)), bty = 'n')

# Density Plot
plot(density(cntl$PDD.DDD), col = 'red', main = 'PDD/DDD in case vs. Control',
     xlab='PDD/DDD (DDD/day)')
lines(density(case$PDD.DDD), col = 'blue')
legend(x = 'topright', legend = c('Control', 'Case'), fill = c('red', 'blue'),
       bty = 'n')

# Prepare Summary Output
case = case[, -c(1, 3, 17, 18, 19)]
cntl = cntl[, -c(1, 3, 17, 18, 19)]
case[, -c(1, 6, 7)] = case[, -c(1, 6, 7)] / 3
cntl[, -c(1, 6, 7)] = cntl[, -c(1, 6, 7)] / 5
names(case)[-c(1, 6, 7)] = sapply(names(case)[-c(1, 6, 7)],
                                   function(x) paste0(x, '/Year'))
names(cntl)[-c(1, 6, 7)] = sapply(names(cntl)[-c(1, 6, 7)],
                                   function(x) paste0(x, '/Year'))
names(case)[1] = 'PDD/DDD'
names(cntl)[1] = 'PDD/DDD'

# Summary Output
case_summary = data.frame('Min' = sapply(case, min),
                          'Quartile1' = sapply(case, quantile, probs = .25),
                          'Median' = sapply(case, median),
                          'Mean' = sapply(case, mean),
                          'Quartile3' = sapply(case, quantile, probs = .75),
                          'Max' = sapply(case, max))
cntl_summary = data.frame('Min' = sapply(cntl, min),
                          'Quartile1' = sapply(cntl, quantile, probs = .25),
                          'Median' = sapply(cntl, median),
                          'Mean' = sapply(cntl, mean),
                          'Quartile3' = sapply(cntl, quantile, probs = .75),
                          'Max' = sapply(cntl, max))
write.csv(case_summary, file = 'data/Case Summary Statistics.csv')
write.csv(cntl_summary, file = 'data/Control Summary Statistics.csv')

# Most Radical Test Possible
p_value = sapply((sapply(case, mean) - sapply(cntl, mean)) /
                   sapply(rbind(case, cntl), sd),
                 function(x) 2 * pnorm(abs(x), lower.tail = FALSE))
write.csv(p_value, file = 'data/Significance.csv')
