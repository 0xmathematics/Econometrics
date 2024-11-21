
#class demo 3
# Rutgers Econometrics 322
# Hang Miao


setwd("~/Dropbox/Econometrics/R Emprical Analysis/class demo/demo Statistical Inference")

df <- read.csv("GDP_Growth_Rate.csv")
head(df)
tail(df)
length(df)


# GDP growth rate from 1880 to 2019
GDP_growthRate <- df$pct_change
length(GDP_growthRate)  
#drop the NA value (missing value)
GDP_growthRate<- GDP_growthRate[2:length(GDP_growthRate)]

# GDP growth rate from 1950 to 2019
GDP_GR_1950 <- GDP_growthRate[70:length(GDP_growthRate)] 
length(GDP_GR_1950)  



## sample mean
X_bar = mean(GDP_GR_1950)
X_bar

mean(GDP_growthRate)

## asume 0.035
Sigma = 0.035
Sigma = 0.075

## sample sd
sqrt( var(GDP_growthRate) )  #0.07612261
sqrt( var(GDP_GR_1950) )   # 0.035303
######################################################
# Statistical Inference
######################################################
#null hypothesis   
mu = 0.06
#Alternative Hypothesis
# mu != 0.06

# sample mean estimator
X_bar = mean(GDP_growthRate)
X_bar
# sample size
n = length(GDP_growthRate)


Sigma = 0.075
# Sigma^2 pop theoretical variance of GDP_Growth_rate
# Sigma^2/n variance of sample mean estimator

# Standard deviation of sample mean estimator
sd_X_bar = sqrt( Sigma^2/n)

# Z stat
Zstat = (X_bar- mu)/ sd_X_bar
Zstat

# alpha = 10%
#left rejection region threshold
qnorm(0.05)

#right rejection region threshold
qnorm(0.95)


2*pnorm(-abs(Zstat)) 

2*(1- pnorm(abs(Zstat)))

#90
X_bar - sd_X_bar* qnorm(.95)
X_bar + sd_X_bar* qnorm(.95)

#95
X_bar - sd_X_bar* qnorm(.975)
X_bar + sd_X_bar* qnorm(.975)

#99
X_bar - sd_X_bar* qnorm(.995)
X_bar + sd_X_bar* qnorm(.995)

