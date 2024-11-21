# class demo 
# Rutgers Econometrics 322
# Hang Miao

###############################################
# Linear Regression 
# Simple made up Example
###############################################

#Data Preparation
x_sample = c(0:3)
constant = rep(1,4)
X = cbind(constant,x_sample)
X = as.matrix( X )
Y = c(4,1,0,1 )

#OLS Estimation
beta_ols =  solve( t(X) %*% X ) %*% t(X) %*% Y
beta_ols




