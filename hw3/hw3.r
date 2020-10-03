####### PROBLEM 1 ###########
mu = 159.2
sigma = (200 - mu)/qnorm(0.841, mean=0, sd=1)

q1 = qnorm(0.25, mean=mu, sd=sigma, lower.tail = TRUE)
q3 = qnorm(0.75, mean=mu, sd=sigma, lower.tail = TRUE)
q90 = qnorm(0.90, mean=mu, sd=sigma, lower.tail = TRUE)

mod_risk_pc = - pnorm(mu + 1*sigma, mean=mu, sd=sigma, lower.tail=TRUE) + 
  pnorm(mu + 2*sigma, mean=mu, sd=sigma, lower.tail=TRUE)
high_risk_pc = 1 - pnorm(mu + 2*sigma, mean=mu, sd=sigma, lower.tail=TRUE)


######## PROBLEM 2 ###########

mu = 310
sigma = 45
capacity = 350
overflow_pc = 1 - pnorm(capacity, mean = mu, sd = sigma)
des_capacity =  qnorm(0.99, mean = mu, sd = sigma)
use_exceeded = qnorm(0.05, mean = mu, sd = sigma, lower.tail=TRUE)
des_usage_mean = capacity - sigma*qnorm(0.99)

q1 = qnorm(0.25, mean=mu, sd=sigma, lower.tail = TRUE)
median = qnorm(0.5, mean=mu, sd=sigma, lower.tail = TRUE)
q3 = qnorm(0.75, mean=mu, sd=sigma, lower.tail = TRUE)

######### PROBLEM 3 ###########
mu = 0.5
sigma = sqrt(1)

ans_a = 1 - plnorm(10, meanlog = mu, sdlog = sigma)
ans_b = qlnorm(0.5, meanlog = mu, sdlog = sigma)

mean = exp(mu + 0.5*sigma^2)
variance = exp(2*mu + sigma^2)*(exp(sigma^2)-1)
cov = sqrt(exp(sigma^2)-1)

ans_c = plnorm(7, meanlog = mu, sdlog = sigma) -
  plnorm(3, meanlog = mu, sdlog = sigma)

q10 = qlnorm(0.1, meanlog = mu, sdlog = sigma)
q90 = qlnorm(0.9, meanlog = mu, sdlog = sigma)

########## PROBLEM 4 ##############
library("MASS")
library("geoR")
library("car")
data = read.csv("~/Downloads/fiber_ash.csv")
qqPlot(data$Fiber_Ash, distribution = "norm")
data$transformed = data$Fiber_Ash^0.5
qqPlot(data$transformed, distribution = "norm")

b <- boxcox(data$Fiber_Ash ~ as.numeric(1:length(data$Fiber_Ash))) 


########## PROBLEM 5 ############
library("MASS")
data = read.csv("~/Downloads/corrosion.csv")
plot(density(data$Corrosion))
qqPlot(data$Corrosion, distribution = "norm")

plot(density(log(data$Corrosion)))
qqPlot(log(data$Corrosion), distribution = "norm")

b <- boxcox(data$Corrosion ~ as.numeric(1:length(data$Corrosion))) 
       #lambda = seq(-3,3,10))

########## PROBLEM 6 #############
mu = 75.5
sigma = 3.5
n = 6

reps = 1000
samples <- replicate(reps, rnorm(n=n, mean=mu, sd=sigma))
sample.avgs <- colMeans(samples)

ans_a = 1 - pnorm(75.75, mean = mu, sd = sigma/sqrt(n))
ans_b = qnorm(0.7, mean = mu, sd = sigma/sqrt(n))
ans_c = 1 - pnorm(78.7, mean = mu, sd = sigma/sqrt(n))
ans_d = ceiling((sigma/0.6)^2)

######### PROBLEM 7 #############
library("data.table")
library("ggplot2")
beta = 1.6
delta = 5
mu = 4.483
sigma = 2.689
n = 10
n_rows = 100
#cols = paste(as.character(seq(1, n, by=1)), 'v', sep="")
cols = paste0(rep(c('v'), times=n), as.character(seq(1, n, by=1)))
data = setNames(data.table(matrix(nrow = n_rows, ncol = n)), cols)
data[,cols] = rweibull(n_rows, shape = beta, scale = delta)

df = as.data.frame.matrix(data) 
bw=0.5

g <- ggplot(data = df, aes(x=v1)) +
  geom_histogram(color="black", binwidth=bw, aes(y=..density..)) + 
  stat_function(fun=dweibull, args=list(shape=beta, scale=delta))
show(g)

data$mean <- rowMeans(data)
mean_plot = ggplot(data, aes(x=mean)) +
  geom_histogram(color="black", binwidth=bw)
show(mean_plot)

mean_sample_means = mean(data$mean)
sd_sample_means = sd(data$mean)
sigma/sqrt(n)
