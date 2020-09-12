g <- spot_weld$Weld.Strength

# plotting
hist(g, xlab="Weld Strength", main="Time Series Histogram")
boxplot(g, ylab="Weld Strength")
norm_on_hist(g, h)

get_stats(g)

# outlier detection
bb_outliers(g)
emp_rule(g)

quantile(g, probs=c(0.05))
cat('Fraction of values above 5350', sum(g > 5350)/length(g))


save.image(file='hw1.RData')
