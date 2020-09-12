source("Downloads/hw1.r")
g <- champagne$Sales

# plotting
h <- hist(g, xlab="Champagne Sales", main="Time Series Histogram")
# norm_on_hist(g, h)
boxplot(g, ylab="Champagne Sales")
range <- max(g) - min(g)
get_stats(g)

# outlier detection
cat(bb_outliers(g))
emp_rule(g)

save.image(file='hw1.RData')

