library("ggplot2")
library("dplyr")
library("MASS")
library("geoR")
library("car")
library("cubature")
library("data.table")

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

#### problem 1 ######

mean_1 = 4.7
mean_2 = 7.8
S1 = sqrt(4)
S2 = sqrt(6.25)
n1 = 15
n2 = 15
alpha = 0.05
df = n1+n2-2

sd_hat = sqrt(((n1-1)*S1^2 + (n2-1)*S2^2)/(n1+n2-2))
se = sd_hat*sqrt(1/n1 + 1/n2)

t = (mean_1 - mean_2)/se
p_value = 2*pt(abs(t), df=n1+n2-2, lower.tail = FALSE)
p_value < alpha

get_conf_interval(0, se, t_value(1-alpha, n1+n2-2))
c(-abs(mean_1 - mean_2), abs(mean_1 - mean_2))

ncp  = 3/se
power <- 1 - (pt(abs(t), df=df, ncp=ncp) - pt(-abs(t), df=df, ncp=ncp))

beta = 0.05
n = ceiling((qt(alpha/2,df)+qt(beta,df))*sd_hat/(-2))^2

rm(list = ls())

#### problem 2 ######

sum_x = 6322.28
sum_y = 4757.90
sum_x2 = 162674.18
sum_y2 = 107679.27
sum_xy = 125471.10
n = 250

mean_x = sum_x/n
mean_y = sum_y/n

beta1 = (sum_xy - n*mean_x*mean_y)/(sum_x2 - n*mean_x^2)
beta0 = mean_y - beta1*mean_x

plot_x = mean_x + c(-8:8)
plot_y = beta0 + beta1*plot_x
plot(plot_x, plot_y, type='l', col='green', lwd=5, xlab='BMI', ylab='Body Fat')

fat30 = beta0 + beta1*30
resid25 = 25 - (beta0 + beta1*25)

rm(list = ls())

#### problem 3 ######
data <- matrix(c(25.9, 4.9176, 30.0, 5.0500,
                 29.5, 5.0208, 36.9, 8.2464,
                 27.9, 4.5429, 41.9, 6.6969,
                 25.9, 4.5573, 40.5, 7.7841,
                 29.9, 5.0597, 43.9, 9.0384,
                 29.9, 3.8910, 37.5, 5.9894,
                 30.9, 5.8980, 37.9, 7.5422,
                 28.9, 5.6039, 44.5, 8.7951,
                 35.9, 5.8282, 37.9, 6.0831,
                 31.5, 5.3003, 38.9, 8.3607,
                 31.0, 6.2712, 36.9, 8.1400,
                 30.9, 5.9592, 45.8, 9.1416
), ncol=2, byrow=TRUE)

df <- data.frame(data)
colnames(df) <- c('sale_price', 'taxes')
data <- data.table(df)

n = length(data$taxes)

# as.data.frame(data).sale_price
var(data$sale_price)
var(data$taxes)

# linear model check
model <- lm(sale_price ~ taxes, data=data)
anova_stats <- anova(model)
residuals <- resid(model)
stdres <- rstandard(model)
durbinWatsonTest(model)

# should be random, no apparent pattern detected
plot(data$taxes, stdres, ylab="Standardized Residuals", xlab="Taxes",
     main="Residual Plot")
abline(0,0)

# normality check, similar variance
qqPlot(stdres, ylab = "Standardized Residual quantiles", main="Normal Probaility Plot", distribution = "norm")

# predicted model plot
coeffs <- coefficients(model)
fit <- fitted(model)
pred_sales <- coeffs[1] + coeffs[2]*(data$taxes)

# error metrics
ss_error = sum(residuals^2)
df_error = n - 2
ms_error = ss_error/df_error

y = data$sale_price
ss_model = sum((fit - mean(y))^2)
df_model = n - 1
ms_model = ss_model/df_model

R2 = ss_model/(ss_model+ss_error)

x = data$taxes
se_slope = sqrt(ms_error)/sqrt((n-1)*var(x))
se_intercept = sqrt(ms_error)*sqrt(1/n+(0-mean(x))^2/((n-1)*var(x)))

F_stat = ms_model/ms_error

t_val_slope = coeffs[2]/se_slope
p_val_t =  2*pt(t_val_slope, df_error, lower.tail = FALSE)
p_val_F = 2*pf(F_stat, df_model, df_error, lower.tail = FALSE)
  
# linear model plot
p <- ggplot(data, aes(x=taxes, y=sale_price)) 
p <- p + geom_point() 
p <- p + stat_smooth(method=lm)

print(p)

# quadratic model check
data$taxes2 <- data$taxes^2
quad_model <- lm(sale_price ~ taxes + taxes2, data=data)
coefficients(quad_model)
summary(quad_model)

# correlation between data
rho_xy_sample <- cor(data$sale_price, data$taxes)
confint(model)
summary(model)
sigma(model)

rm(list = ls())



