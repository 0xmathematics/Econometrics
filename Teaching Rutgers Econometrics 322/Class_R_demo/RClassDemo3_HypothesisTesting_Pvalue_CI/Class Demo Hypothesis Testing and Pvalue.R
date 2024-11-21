
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










# compute the sample mean
samplemean = mean(return)
samplemean

# compute the z statistics
zstat = (samplemean - 20)/ sqrt(25/100)
zstat

pnorm(zstat)

qnorm(0.995)

qnorm(0.005)
qnorm(.01)

qnorm(.99)

z = .94

pvalue = 2*(1-pnorm(z) )
pvalue

pvalue = pnorm(z)
pvalue

pvalue = 1-pnorm(z)
pvalue
