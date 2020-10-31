library("ggplot2")
library("dplyr")
library("MASS")
library("geoR")
library("car")

getwd()
setwd('~/work/stat601r/hw4')

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

#### PROBLEM 1 #############
mean = 75.5
sd = 3.5
n = 6
se = sd/sqrt(n)
se_tranformed = (1/(2*sqrt(mean)))*se
n_min = ceiling(((1/(2*sqrt(mean)))*sd/0.03)^2)
rm(list=ls())

#### PROBLEM 2 #############
n1 = 16
n2 = 9
mean1 = 75
mean2 = 70
sigma1 = 8
sigma2 = 12
std_err = sqrt(sigma1^2/n1 + sigma2^2/n2)

ans_a = pt((4-(mean1-mean2))/std_err, df=n1+n2-2, lower.tail = TRUE, log.p = FALSE)
ans_b = pt((5.5-(mean1-mean2))/std_err, df=n1+n2-2, lower.tail = TRUE, log.p = FALSE)- pt((3.5-(mean1-mean2))/std_err, df=n1+n2-2, lower.tail = TRUE, log.p = FALSE)

n = ceiling((sqrt(sigma1^2 + sigma2^2)/2.5)^2)

rm(list=ls())

#### PROBLEM 3 ##############
mu_x = 5.2
mu_y = 3.4
mu_z = 2.2

sigma_x = 2.6
sigma_y = 2.4
sigma_z = 1.8

mu = mu_x + mu_y - 2*mu_z
sigma = sqrt(sigma_x^2 + sigma_y^2 + 4*sigma_z^2)

print(mu)
print(sigma)

#### PROBLEM 4 ##############
data = read.csv("./rain_pH.csv")
N = length(data$rain.pH)
sample_mean = 4.6982
sample_variance = 0.39623
std_err = sqrt(sample_variance/N)
ci = 0.99
t_val = t_value(ci, N-1)
interval = get_conf_interval(sample_mean, std_err, t_val)

# H1: mean is lesser than 5
p_value = pt(-(sample_mean - 5)/std_err, df=N-1, lower.tail = FALSE)

alpha = 0.01
if(p_value < alpha){
  print('H1 is correct')
} else {
  print('H0 is correct')
}

qqPlot(data$rain.pH, distribution = "norm")


### PROBLEM 5 ##################
data = read.csv("./ceramic.csv")
sample_mean = mean(data$Yield)
N = length(data$Yield)
std_err = sd(data$Yield)/sqrt(N)

ci = c(0.90, 0.95)
t_vals = c(t_value(ci[1], N-1), t_value(ci[2], N-1))

interval_90 = get_conf_interval(sample_mean, std_err, t_vals[1])
interval_95 = get_conf_interval(sample_mean, std_err, t_vals[2])

## H1: mean is different than 90
p_value = 2*pt(abs((sample_mean - 90)/std_err), df=N-1, lower.tail = FALSE)

alpha = 0.05
if(p_value < alpha){
  print('H1 is correct')
} else {
  print('H0 is correct')
}

## H1: mean is lesser than 90
p_value = pt(-((sample_mean - 90)/std_err), df=N-1, lower.tail = FALSE)

alpha = 0.05
if(p_value < alpha){
  print('H1 is correct')
} else {
  print('H0 is correct')
}

## H1: mean is greater than 90
p_value = pt(((sample_mean - 90)/std_err), df=N-1, lower.tail = FALSE)
alpha = 0.05
if(p_value < alpha){
  print('H1 is correct')
} else {
  print('H0 is correct')
}

qqPlot(data$Yield, distribution = "norm")
hist(data$Yield, main = '', xlab = 'Yield')
shapiro.test(data$Yield)

########## PROBLEM 6 ############
data = read.csv("./fiber_ash.csv")$Fiber_Ash
sample_mean = mean(data)
N = length(data)
std_err = var(data)/sqrt(N)

ci = 0.99
t_val = t_value(ci, N-1)
interval = get_conf_interval(sample_mean, std_err, t_val)

p_value = pt(-((sample_mean - 2.70)/std_err), df=N-1, lower.tail = FALSE)
alpha = 0.01

if(p_value < alpha){
  print('H1 is correct')
} else {
  print('H0 is correct')
}

print(paste0('Central Limit Theorem Applies ?', N>30))

############# PROBLEM 7 ##############
N = 20
sample_mean = 129.747
sample_sd = 0.87643
std_err = sample_sd/sqrt(N)

### part a #####
delta = 0.25
n = ceiling((t_value(0.98, Inf)*sample_sd/delta)^2)
n = ceiling((t_value(0.98, n-1)*sample_sd/delta)^2)
print(n)

### part b #####
beta = 1- 0.80
alpha = 0.05
mu0 = 130
mu1 = 130.25

n = ceiling(((qt(1-alpha/2,df=Inf)+qt(1-beta,df=Inf))*sample_sd/(mu1-mu0))^2)
n = ceiling(((qt(1-alpha/2,df=n-1)+qt(1-beta,df=n-1))*sample_sd/(mu1-mu0))^2)
print(n)

############# PROBLEM 8 ##############
data = read.csv("./pipes.csv")
sample_1 = filter(data, Pipe.Type == 'Type 1')
sample_2 = filter(data, Pipe.Type == 'Type 2')

n1 = length(sample_1$Pipe.Type)
n2 = length(sample_2$Pipe.Type)

sample_mean_1 = mean(sample_1$Deflection.Temp)
sample_mean_2 = mean(sample_2$Deflection.Temp)

S1 = sd(sample_1$Deflection.Temp)
S2 = sd(sample_2$Deflection.Temp)

pop_sd = sqrt(((n1-1)*(S1^2)+(n2-1)*(S2^2))/(n1+n2-2))
std_err = pop_sd*sqrt(1/n1 + 1/n2)

# H1: mean of Type 1 exceeds mean of Type 2
p_value = pt(((sample_mean_1 - sample_mean_2) - 0)/std_err, df=n1+n2-2, lower.tail = FALSE)
alpha = 0.05

if(p_value < alpha){
  print('H1 is correct')
} else {
  print('H0 is correct')
}

ci = 0.90
t_val = t_value(ci, n1+n2-2)
interval = get_conf_interval(sample_mean_1-sample_mean_2, std_err, t_val)

