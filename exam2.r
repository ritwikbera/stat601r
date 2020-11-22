library("ggplot2")
library("dplyr")
library("MASS")
library("geoR")
library("car")
library("cubature")

getwd()
setwd('~/work/stat601r')

# gives t value for 2 sided t test
t_value <- function(ci, dof){
  t_val = qt(1-(1-ci)/2, df=dof)
  return(t_val)
}

# gives confidence interval given t value
get_conf_interval <- function(sample_mean, std_err, t_val){
  interval = sample_mean + c(-1, 1)*t_val*std_err
  return(interval)
}


### PROBLEM 3 #######

data = c(79.3, 75.1, 78.2, 74.1, 73.9, 75.0, 77.6, 77.3, 73.8, 74.6, 75.5, 74.0, 74.7, 75.9, 72.9, 73.8, 74.2, 78.1,
         75.4, 76.3, 75.3, 76.2, 74.9, 78.0, 75.1, 76.8)
N = length(data)
mean_pof = mean(data)
median_pof = median(data)
var_pof = var(data)
sd_pof = sd(data)
se_mean_pof = sqrt(var_pof/N)
p_pof = length(data[data < 73])/N
se_p_pof = sqrt(p_pof*(1-p_pof)/N)

### PROBLEM 4 ########

data = c(19.8, 10.1, 14.9, 7.5, 15.4, 15.4, 15.4, 18.5, 7.9,
         12.7, 11.9, 11.4, 11.4, 14.1, 17.6, 16.7, 15.8, 19.5,
         8.8, 13.6, 11.9, 11.4, 10.7, 12.9, 15.8)

N = length(data)
mean_load = mean(data)
se_mean_load = sd(data)/sqrt(N)

png(file='q4.png')
qqPlot(data, distribution = "norm")
dev.off()

ci = 0.95
t_val = t_value(ci, N-1)
interval = get_conf_interval(mean_load, se_mean_load, t_val)

#### PROBLEM 5 ########
data = c(101, 104, 104, 77, 89, 88, 104, 96, 92, 70, 89, 91,
         39, 103, 93, 85, 104, 104, 67, 104, 104, 104, 87, 104, 89,
         78, 104, 76, 86, 103, 102, 80, 45, 94, 104, 104, 76, 80,
         72, 73, 81)

N = length(data)
ci = 0.95
alpha = 1 - ci
d1 = qchisq(1-alpha/2, df=N-1)
d2 = qchisq(alpha/2, df=N-1)
s = sd(data)
interval = c(sqrt((N-1)*s^2/d1), sqrt((N-1)*s^2)/d2)

##### PROBLEM 6 #######

mu = 3500
X = 3550
S = 25
N = 10
t_val = (X-mu)/(S/sqrt(N))
p_val = pt(t_val, df=N-1, lower.tail = FALSE)
p_val < 0.05
