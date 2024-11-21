#########################################################
# load the csv data
#########################################################
df <- read.csv("sample_price_2.csv")

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
# Strike = 60
# H0: u = 60
# H1: u < 60

# compute the z statistics
# Suppose the Var = 25 of stockprice is given 

# Rcode: var(stockprice)

# Var = 25
# Var[X_bar] = 25/length(stockprice)
# sd[X_bar] = sqrt(25/length(stockprice))


zstat = (samplemean - 60) / sqrt(25/length(stockprice))
zstat

# compute the threshold value at alpha = 1%
qnorm(0.01) 

#########################################################
# P-value
#########################################################
pnorm(zstat)


#########################################################
# Confidence Interval
#########################################################
X_bar = samplemean
sd_X_bar = sqrt(25/length(stockprice))

#90%CI
X_bar - sd_X_bar* qnorm(.95)
X_bar + sd_X_bar* qnorm(.95)

#95%CI
X_bar - sd_X_bar* qnorm(.975)
X_bar + sd_X_bar* qnorm(.975)

#99%CI
X_bar - sd_X_bar* qnorm(.995)
X_bar + sd_X_bar* qnorm(.995)

