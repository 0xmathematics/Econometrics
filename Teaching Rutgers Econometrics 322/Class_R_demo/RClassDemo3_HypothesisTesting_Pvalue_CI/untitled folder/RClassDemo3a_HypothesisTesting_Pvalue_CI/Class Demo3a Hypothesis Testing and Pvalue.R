
#class demo 3a
# Rutgers Econometrics 322
# Hang Miao

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

