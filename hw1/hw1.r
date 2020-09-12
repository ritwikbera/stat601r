library("ggplot2")
library("car")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# statistics
get_stats <- function(g){
  m <<- mean(g)
  med <<- median(g)
  variance <<- var(g)
  std <<- sqrt(var(g))
  mode <<- getmode(g)
  
  cat('Mean: ',m)
  cat('Variance: ',variance)
  cat('Standard Deviation: ',std)
  cat('Mode: ',mode)
  cat('Median: ', med)
  
}


norm_on_hist <- function(x, h){
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2) 
}


# empirical rule analysis
emp_rule <- function(g){
  z_scores = c(-2,-1,0,1,2)
  z_points <- mean(g) + z_scores*sd(g)
  points(x=z_points,y=rep(0,5),pch=21,col='black',bg='red')
  
  sigma1 <<- (sum(g>=m-std & g<=m+std)/length(g))*100
  sigma2 <<- (sum(g>=m-(2*std) & g<=m+(2*std))/length(g))*100
  sigma3 <<- (sum(g>=m-(3*std) & g<=m+(3*std))/length(g))*100
  cat('Percentage in one SD FROM mean:',sigma1)
  cat('Percentage in two SD FROM mean:',sigma2)
  cat('Percentage in three SD FROM mean:',sigma3)
  
  qqPlot(g, ylab="data",
         xlab="quantiles", distribution="norm")
}

bb_outliers <- function(g){
  out_val <- boxplot.stats(g)$out
  out_ind <- which(g %in% c(out_val))
  return(paste("Outliers: ", paste(out_val, collapse = ", ")))
}

