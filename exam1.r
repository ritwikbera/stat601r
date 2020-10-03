library("ggplot2")
library("dplyr")

###### problem 1 ########

x <- rweibull(n=1000, shape=1.1728, scale=1.2750)
plot(density(x))
(1 - pweibull(q = 1.5, shape = 1.1728, scale = 1.2750))^3
quantile(x, probs = c(0.20))

####### problem 3 #######

data <- matrix(c(16.1,134.9,52.7,14.4,124.3,99,24.3,16.3,15.2,47.7,
                 12.9,72.7,126.7,46.4,60.3,23.5,43.6,79.4,38,58.2,26.5,
                 297.1,491.8,1332.9,1172,1482.7,335.4,528.9,24.1,545.2,92.9,
                 337.1,102.3,255.1,100.5,159.9,168,95.2,132.5,442.6,15.8,175.8,
                 25.1,820.1,82.5,713.9,785.6,114,31.9,86.3,646.6,169.9,20.2,
                 280.2,194.2,408.4,155.5,864.6,355.4,634,2029.9,362.1,0,
                 131.1,166.5,2258.4,497.5,263.4,252.3,351.4,678.9,3010.2,67.1,318.2,
                 2476.4,181.4,2081.5,424.3,188.1,563,149.1,2122.9,1295.9,0),
               ncol=4, byrow = FALSE)
colnames(data) <- c("HighDose","Control1","Control2","Control3")
data <- as.table(data)
data <- as.data.frame(data)
data$Var2 <- as.factor(data$Var2)

# data[data$Var2 == "HighDose",]
# filter(data, Var2 == "HighDose")

p <- ggplot(data, aes(x=Var2, y=Freq, fill=Var2)) + geom_boxplot() + 
  labs(title="", x="", y="")
show(p)

###### problem 13 ##########

thickness <- c(425, 431, 416, 419, 421, 436, 418, 410, 431, 433,
               423, 426, 410, 435, 436, 428, 411, 426, 409, 437,
               422, 428, 413, 416)

mean(thickness)
sd(thickness)
n <- length(thickness)
std_err <- sd(thickness)/sqrt(length(thickness))
median(thickness)
Fn <- ecdf(thickness)
Fn(430)
