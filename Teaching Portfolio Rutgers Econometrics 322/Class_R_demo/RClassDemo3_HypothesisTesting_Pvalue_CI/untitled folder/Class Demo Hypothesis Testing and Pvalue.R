
#class demo 3
# Rutgers Econometrics 322
# Hang Miao




set.seed(5)
X1 = rnorm(500,100, 10)
X2 = rbinom(1000, 100, 0.5)

write.csv(X1,"sample_price_1.csv",row.names = FALSE)
write.csv(X2,"sample_price_2.csv",row.names = FALSE)

#########################################################
# load the csv data
#########################################################
df <- read.csv("sample_price_1.csv")

stockprice = df[  ,1]


# frequently used function
head(df)
tail(df)
length(stockprice)


#########################################################
# Estimation
#########################################################
# Compute the sample mean
samplemean = mean(stockprice)
samplemean

#########################################################
# Hypothesis testing
#########################################################
# Strike = 95
# H0: u = 95
# H1: u > 95

# compute the z statistics
# Suppose the Var = 100 of stockprice is given 
zstat = (samplemean - 95)/ sqrt(100/length(stockprice))
zstat

# compute the threshold value at alpha = 1%
qnorm(1- 0.01)

#########################################################
# P-value
#########################################################
1- pnorm(zstat)


#########################################################
# Confidence Interval
#########################################################
X_bar = samplemean
sd_X_bar = sqrt(100/length(stockprice))

#90
X_bar - sd_X_bar* qnorm(.95)
X_bar + sd_X_bar* qnorm(.95)

#95
X_bar - sd_X_bar* qnorm(.975)
X_bar + sd_X_bar* qnorm(.975)

#99
X_bar - sd_X_bar* qnorm(.995)
X_bar + sd_X_bar* qnorm(.995)






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

## asume 0.03
Sigma = 0.03


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

# Standard deviation of sample mean estimator
sd_X_bar = sqrt( Sigma^2/ n)

# Z stat
Zstat = (X_bar- mu)/ sd_X_bar
Zstat

qnorm(0.05)
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

######################################################
# Generate the return
######################################################
return = rnorm(100, 35, 20)
return



## asume 0.03
Sigma = 20


######################################################
# Statistical Inference
######################################################
#null hypothesis
mu = 20
# sample mean estimator
X_bar = mean(return)
X_bar
# sample size
n = length(return)

# Standard deviation of sample mean estimator
sd_X_bar = sqrt( Sigma^2/ n)

# Z stat
Zstat = (X_bar- mu)/ sd_X_bar
Zstat

qnorm(.99)

Pvalue = 1-pnorm(Zstat)
Pvalue


qnorm(0.05)
qnorm(0.95)

2*pnorm(-abs(Zstat)) 
2*(1- pnorm(abs(Zstat)))

#CI90
X_bar - sd_X_bar* qnorm(.95)
X_bar + sd_X_bar* qnorm(.95)

#CI95
X_bar - sd_X_bar* qnorm(.975)
X_bar + sd_X_bar* qnorm(.975)

#CI99
X_bar - sd_X_bar* qnorm(.995)
X_bar + sd_X_bar* qnorm(.995)









