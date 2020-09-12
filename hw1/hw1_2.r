g <- lake_solids$Solids

# plotting
hist(g, xlab="Solids", main="Measurements Histogram")
boxplot(g, ylab="Lake Solids")
norm_on_hist(g, h)
get_stats(g)

# outlier detection
cat(bb_outliers(g))
emp_rule(g)

