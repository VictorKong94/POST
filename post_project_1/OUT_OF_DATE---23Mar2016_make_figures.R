# Import data
CNTL = read.csv('data/diab_CNTL_all_rx_processed.csv')
CASE = read.csv('data/diab_CASE_all_rx_processed.csv')

# Histogram
hist(CNTL$PDD, col = rgb(1, 0, 0, .5), breaks = 15,
     main = 'PDD/DDD in Case vs. Control',
     xlab = 'PDD/DDD (DDD/day)')
hist(CASE$PDD, col = rgb(0, 0, 1, .5), breaks = 15, add = TRUE)
legend(x = 'topright', legend = c('Control', 'Case'),
       fill = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)), bty = 'n')

# Density Plot
plot(density(CNTL$PDD), col = 'red', main = 'PDD/DDD in Case vs. Control',
     xlab='PDD/DDD (DDD/day)')
lines(density(CASE$PDD), col = 'blue')
legend(x = 'topright', legend = c('Control', 'Case'), fill = c('red', 'blue'),
       bty = 'n')

# Prepare Summary Output
CASE = CASE[, -c(1, 16)]
CNTL = CNTL[, -c(1, 16)]
CASE[, -c(1, 6, 7)] = CASE[, -c(1, 6, 7)] / 3
CNTL[, -c(1, 6, 7)] = CNTL[, -c(1, 6, 7)] / 5
names(CASE)[-c(1, 6, 7)] = sapply(names(CASE)[-c(1, 6, 7)],
                                   function(x) paste0(x, '/Year'))
names(CNTL)[-c(1, 6, 7)] = sapply(names(CNTL)[-c(1, 6, 7)],
                                   function(x) paste0(x, '/Year'))
names(CASE)[1] = 'PDD/DDD'
names(CNTL)[1] = 'PDD/DDD'

# Summary Output
CASE_summary = data.frame('Min' = sapply(CASE, min),
                          'Quartile1' = sapply(CASE, quantile, probs = .25),
                          'Median' = sapply(CASE, median),
                          'Mean' = sapply(CASE, mean),
                          'Quartile3' = sapply(CASE, quantile, probs = .75),
                          'Max' = sapply(CASE, max))
CNTL_summary = data.frame('Min' = sapply(CNTL, min),
                          'Quartile1' = sapply(CNTL, quantile, probs = .25),
                          'Median' = sapply(CNTL, median),
                          'Mean' = sapply(CNTL, mean),
                          'Quartile3' = sapply(CNTL, quantile, probs = .75),
                          'Max' = sapply(CNTL, max))
write.csv(CASE_summary, file = 'data/Case Summary Statistics.csv')
write.csv(CNTL_summary, file = 'data/Control Summary Statistics.csv')

# Most Radical Test Possible
p_value = sapply((sapply(CASE, mean) - sapply(CNTL, mean)) /
                   sapply(rbind(CASE, CNTL), sd),
                 function(x) 2 * pnorm(abs(x), lower.tail = FALSE))
write.csv(p_value, file = 'data/Significance.csv')
