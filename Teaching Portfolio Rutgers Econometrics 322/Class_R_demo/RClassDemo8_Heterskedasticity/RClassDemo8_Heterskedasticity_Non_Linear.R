######################################################################################
# Class demo 8: Nonlinear Form and Handling Heteroskedasticity 
#                                  by WLS, GLS and Heteroskedasticity-Robust sd
######################################################################################
##############################
## Econometrics 322 Hang Miao
##############################

# Loading the data by one of the following two way:
#########################################################################################
## Approach1: Loading data by Using the third party package: Applied Econometrics with R
#########################################################################################
library(AER)                                                     
data(CASchools) # Use data
CASchools$STR <- CASchools$students/CASchools$teachers    # generating variable "STR student:teacher ratio"
CASchools$test_score <- (CASchools$read + CASchools$math) / 2   # generating variable "test_score"
head(CASchools)

# write.csv(CASchools,"CASchoolsData.csv", row.names = FALSE)

###########################################################################################
## Approach2: Loading data by Reading the data file CASchoolsData.csv under the same folder
###########################################################################################
CASchools <- read.csv("CASchoolsData.csv", header=TRUE, sep=",")
head(CASchools)



### Matrix method
y = CASchools$test_score
X = CASchools$STR
n = length(y)
intercept = rep(1,n)
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
SigmaSquared_Hat = SSR/ (n-1-1)

SigmaSquared_Hat = as.numeric(SigmaSquared_Hat)

VarCovMatrixBetaOLS = SigmaSquared_Hat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

# Software Package
linearMod2 <- lm(test_score ~ STR, data=CASchools)
summary(linearMod2) 
summary(linearMod2)$coef
betaOLS
sdBetaOLS
############################################
# Linear Relationship
############################################

# Transform Data Format into Matrices
y = CASchools$test_score
X1 = CASchools$income
n = length(y)
intercept = rep(1,n)
X = cbind(intercept,X1)
X = as.matrix(X)
y = as.matrix(y)

# OLS 
betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

# Plot the Linear Regression Line and the Data
plot(CASchools$income, y)
abline(betaOLS, col = 'red')

#########################################################
# Non-Lnear Relationship 
#    by adding a Quadratic Term
#########################################################
# y = beta_0 + beta_1 x
# y = beta_0 + beta_1 log x
y = CASchools$test_score
X1 = log(CASchools$income) #  Adding a logrithmly Term here
n = length(y)
intercept = rep(1,n)
X = cbind(intercept,X1)   #  New Matrix D has three Columns
X = as.matrix(X)
y = as.matrix(y)
# Run OLS regression
betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y
# Prediction

#creat 100 new income data ranging from the min to max of the original income
newdat = data.frame(income = seq(min(CASchools$income), max(CASchools$income), length.out = 100))
# Transform Data Format into Matrices
intercept = rep(1,length(newdat$income))
X_new = cbind(intercept,log(newdat$income))
X_new = as.matrix(X_new)

Y_predict = X_new %*% betaOLS

# Plot the Non-Linear Regression Line and the Data
plot(CASchools$income, y,main='log')
lines(x = newdat$income, y = Y_predict, col='red')





#########################################################
# y = beta_0 + beta_1 x + beta_2 x^2 +u
# Transform Data Format into Matrices
y = CASchools$test_score
X1 = CASchools$income
X2 = (CASchools$income)^2    #  Adding a Quadratic Term here
n = length(y)
intercept = rep(1,n)
X = cbind(intercept,X1,X2)   #  New Matrix D has three Columns
X = as.matrix(X)
y = as.matrix(y)

# Run OLS regression
betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

# Prediction

#creat 100 new income data ranging from the min to max of the original income
newdat = data.frame(income = seq(min(CASchools$income), max(CASchools$income), length.out = 100))
# Transform Data Format into Matrices
intercept = rep(1,length(newdat$income))
X_new = cbind(intercept,newdat,newdat^2)
X_new = as.matrix(X_new)

Y_predict = X_new %*% betaOLS

# Plot the Non-Linear Regression Line and the Data
plot(CASchools$income, y, main = 'quadratic')
lines(x = X_new[,2], y = Y_predict, col='red')

#########################################################
# Heteroskedasticity
#########################################################

# Compute the Residuals
Residual = y- X %*%betaOLS

# Heterskedasticity
plot(CASchools$income, Residual)   # Resdual
Squared_Residual = Residual*Residual
plot(CASchools$income, Squared_Residual, ylim =c(0,500))  # Squared Residual 

##################################################################################
# WLS: the Sharp estimator with its associated estimated Variance Covariance Matrix
##################################################################################

# The following step is using OLS to estimate the unkown functional
# Then use the estimated functional form (linear form in this case) to estimate Squared_Residual
# The result is denote as Squared_Residual_hat
#########################################################
# Run OLS regression of Squared Residual onto X
alphaOLS_SquaredResidual = solve( t(X) %*% X ) %*% t(X) %*% Squared_Residual

# Estimate Residual by Linear Form
Squared_Residual_Hat = X %*% alphaOLS_SquaredResidual

# Prediction (Use the 100 New Income Data Created Above )
Squared_Residual_predict = X_new %*% alphaOLS_SquaredResidual

# Plot the Non-Linear Regression Line and the Data
plot(CASchools$income, Squared_Residual,ylim =c(0,500))
lines(x = X_new[,2], y = Squared_Residual_predict, col='red')
#########################################################

# Variance Covariance Matrix for the Uncertainty u
Squared_Residual_Hat_vector = as.vector(Squared_Residual_Hat)
Lambda_hat = diag( Squared_Residual_Hat_vector )

# WLS estimator  
beta_WLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) %*% t(X)%*% solve(Lambda_hat)  %*% y 
beta_WLS
betaOLS # Note: OLS estimator is solve( t(X) %*% X ) %*% t(X) %*% y

# Variance Covariance Matrix for WLS estimator 
Var_Cov_beta_WLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) 

Var_beta_WLS = diag(Var_Cov_beta_WLS)

sd_beta_WLS = sqrt(Var_beta_WLS)
sd_beta_WLS


##################################################################################
# OLS: The Blunt Estimator with its Heteroskedasticity-Robust Standard Deviation  
##################################################################################

# Unlike the WLS case, no functional form needs to be estimated.
# Just use the Squared_Residual as the diagonal line of the Variance Covariance Matrix for u
#########################################################

#########################################################

# Variance Covariance Matrix for the Uncertainty u
Squared_Residual_vector = as.vector(Squared_Residual)
Lambda_hat = diag( Squared_Residual_vector )

# OLS estimator  
beta_OLS = solve( t(X) %*% X ) %*% t(X) %*% y
beta_OLS

# Variance Covariance Matrix for OLS estimator under Heteroskedasticity
# Same thing as the Heteroskedasticity-Robust Variance Covariance Matrix
# Same thing as Heteroskedasticity-consistent Variance Covariance Matrix
Var_Cov_beta_OLS_HR = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )

Var_beta_OLS_HR = diag(Var_Cov_beta_OLS_HR) # Hetero-Rubust variance for beta_OLS
sd_beta_OLS_HR = sqrt(Var_beta_OLS_HR)      # Hetero-Rubust sd for beta_OLS
sd_beta_OLS_HR


# Note: it is different from Variance Covariance Matrix for OLS estimator under Homoskedasticity
SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
SigmaSquared_Hat = SSR/ (n-2-1)
SigmaSquared_Hat = as.numeric(SigmaSquared_Hat)
Var_Cov_beta_OLS = SigmaSquared_Hat * solve(t(X) %*% X) 
Var_beta_OLS_Homo = diag(Var_Cov_beta_OLS) # Hetero-Rubust variance for beta_OLS
sd_beta_OLS_Homo = sqrt(Var_beta_OLS_Homo)      # Hetero-Rubust sd for beta_OLS
sd_beta_OLS_Homo


sd_beta_WLS
sd_beta_OLS_HR
sd_beta_OLS_Homo 

##################################################################################
# Software Package
##################################################################################

head(CASchools)
CASchools$incomeSquare <- (CASchools$income)^2
linearMod <- lm(test_score ~ income+incomeSquare, data=CASchools)
summary(linearMod) 
summary(linearMod)$coef

coeftest(linearMod, vcov. = vcovHC, type = "const")  # Homoskedasticity Using Software Package
sd_beta_OLS_Homo # Homoskedasticity Using Matrix

coeftest(linearMod, vcov. = vcovHC, type = "HC0")  # Heteroskedasticity Robust Using Software Package
sd_beta_OLS_HR        # Heteroskedasticity Using Matrix



##################################################################################
# GLS  pseudo code
##################################################################################

# The first step same as the WLS is to estimate the Variance Covariance Matrix for Uncertainty
# denoted as Lambda_hat

# GLS estimator
beta_GLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) %*% t(X)%*% solve(Lambda_hat)  %*% y 

# Variance Covariance Matrix for GLS estimator 
Var_Cov_beta_GLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) 
# Variance for GLS estimator 
Var_beta_GLS = diag(Var_Cov_beta_WLS)
# standard Deviation for GLS estimator 
sd_beta_GLS = sqrt(Var_beta_WLS)



