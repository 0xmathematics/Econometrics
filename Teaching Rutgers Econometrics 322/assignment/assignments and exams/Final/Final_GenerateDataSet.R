
############################################################################
# Final Exam
############################################################################

##############################
## Load the dataset
##############################

# you could use the following code to load the each of the dataset for the midterm exam
# df <- read.csv("Fill Name with .extention")


# Example for loading the dataset for question 11-22. 
# The data file name is "Dataset11_23.csv", so we just replece the fill name
df <- read.csv("Dataset11_23.csv")  
head(df)

#############################################################
## Question 1: Joint Statistical Inference: large sample
#############################################################

##############################
## Generate the dataset
##############################
set.seed(6)
N = 5000
u = rnorm(N, 0, 100)
x1= rnorm(N, 20, 5)
x2= rchisq(N, df = 20, ncp = 0)
x3= rbinom(n=N, size = 1, prob=0.1)
x4= rbinom(n=N, size = 1, prob=0.8)

beta0 = 5
beta1 = -3
beta2 = 3
beta3 = -1
beta4 = -5

y = beta0+ beta1*x1+beta2*x2+beta3*x3+beta4*x4+u

data = cbind(y,x1,x2,x3,x4)
head(data)

write.csv(data,"Dataset_JointTest1.csv",row.names = FALSE)


##############################
## Loading data
##############################
df <- read.csv("Dataset_JointTest1.csv")
head(df)


##############################
## Analysis
##############################

### matrix method
y = df[, 1]
X = df[ , c(2,3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-4-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

round(sdBetaOLS, digits = 3)

####################
# Joint Test
####################
#  H_0: b1 + 2b2 = 1, b3 = 2
# A = identity matrix, a = 0 vector, d = 4

A =  matrix( c(0,1,2,0,0,0,0,0,1,0) , nrow = 2, ncol = 5, byrow = TRUE)
a = c(1,2)
d = 2
A
a
Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
round(Wald_Stat, digits = 4)

# Chi-square Distribution
Sig_alpha = 0.9
df = d

# Critical Value
critical_Value = qchisq(Sig_alpha, df)
round(critical_Value, digits = 4)
# P-Value
pvalue = 1 - pchisq(Wald_Stat, df)
round(pvalue, digits = 4)

# F Distiribution
Sig_alpha = 0.9
df1 = d
df2 = n-3-1
round(Wald_Stat, digits = 4)
# Critical Value
critical_Value = qf(Sig_alpha, df1,df2)
round(Wald_Stat, digits = 4)
# P-Value
pvalue = pf(F_Stat, df1,df2, lower.tail = FALSE)
round(Wald_Stat, digits = 4)

#############################################################
## Question 2: Joint Statistical Inference: small sample
#############################################################

##############################
## Generate the dataset
##############################
set.seed(7)
N = 40
u = rnorm(N, 0, 100)
x1= rnorm(N, 20, 5)
x2= rchisq(N, df = 20, ncp = 0)
x3= rbinom(n=N, size = 1, prob=0.1)
x4= rbinom(n=N, size = 1, prob=0.8)

beta0 = 5
beta1 = -3
beta2 = 3
beta3 = -1
beta4 = -5

y = beta0+ beta1*x1+beta2*x2+beta3*x3+beta4*x4+u

data = cbind(y,x1,x2,x3,x4)
head(data)

write.csv(data,"Dataset_JointTest2.csv",row.names = FALSE)


##############################
## Loading data
##############################
df <- read.csv("Dataset_JointTest2.csv")
head(df)


##############################
## Analysis
##############################

### matrix method
y = df[, 1]
X = df[ , c(2,3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-4-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

round(sdBetaOLS, digits = 3)


###########################
# Joint Test Small Sample
###########################
#  H_0: b1 + b2 = 0, b3 = -1
# A = identity matrix, a = 0 vector, d = 4

A =  matrix( c(0,1,1,0,0,0,0,0,1,0) , nrow = 2, ncol = 5, byrow = TRUE)
a = c(0,-1)
d = 2
A
a
Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
Wald_Stat
round(Wald_Stat, digits = 4)
F_Stat = Wald_Stat/d
round(F_Stat, digits = 4)


# F Distiribution
Sig_alpha = 0.95
df1 = d
df2 = n-4-1

# Critical Value
critical_Value = qf(Sig_alpha, df1,df2)
round(critical_Value, digits = 4)
# P-Value
pvalue = pf(F_Stat, df1,df2, lower.tail = FALSE)
round(Wald_Stat, digits = 4)

#############################################################
## Question 3: Heteroskedasticity
#############################################################

##############################
## Generate the dataset
##############################
set.seed(8)
N = 1000
u = rnorm(N, 0, 10)
x1= runif(N, 0,50)


beta0 = 5
beta1 = -3


##############################
## HOMO1
##############################
y = beta0+ beta1*x1+u

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_1.csv",row.names = FALSE)
##############################
## HOMO2
##############################
beta0 = 3
beta1 = 2
y = beta0+ beta1*x1+u

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_3.csv",row.names = FALSE)
##############################
## HOMO3
##############################
beta0 = 5
beta1 = 8
y = beta0+ beta1*x1+u

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_4.csv",row.names = FALSE)

##############################
## Hetero
##############################
beta0 = 5
beta1 = -3
u1 = numeric(N)
for (i in 1:length(x1)) {
  u1[i] = rnorm(1,0,x1[i])
}
y = beta0+ beta1*x1+u1

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_2.csv",row.names = FALSE)

##############################
## Loading data
##############################
df1 <- read.csv("Dataset_Hetero_1.csv")
head(df1)
df3 <- read.csv("Dataset_Hetero_3.csv")
head(df1)
df4 <- read.csv("Dataset_Hetero_4.csv")
head(df1)
df2 <- read.csv("Dataset_Hetero_2.csv")
head(df2)

##############################
## Visualize Hetero
##############################
# Software Package
linearMod1 <- lm(y ~ x1, data=df1)
summary(linearMod1) 
summary(linearMod1)$coef
linearMod1$coefficients
linearMod1$residuals^2
plot(x1,linearMod1$residuals^2 )

linearMod1 <- lm(y ~ x1, data=df3)
summary(linearMod1) 
summary(linearMod1)$coef
linearMod1$coefficients
linearMod1$residuals^2
plot(x1,linearMod1$residuals^2 )

linearMod1 <- lm(y ~ x1, data=df4)
summary(linearMod1) 
summary(linearMod1)$coef
linearMod1$coefficients
linearMod1$residuals^2
plot(x1,linearMod1$residuals^2 )

linearMod2 <- lm(y ~ x1, data=df2)
summary(linearMod2) 
summary(linearMod2)$coef
linearMod2$coefficients
linearMod2$residuals^2
plot(x1,linearMod2$residuals^2  )


##############################
## Analysis
##############################
##################################################################################
# WLS: the Sharp estimator with its associated estimated Variance Covariance Matrix
##################################################################################

# The following step is using OLS to estimate the unkown functional
# Then use the estimated functional form (linear form in this case) to estimate Squared_Residual
# The result is denote as Squared_Residual_hat
#########################################################

linearMod2 <- lm(y ~ x1, data=df2)
Squared_Residual = linearMod2$residuals^2
# Run OLS regression of Squared Residual onto X
linearMod_Residual2 <- lm(Squared_Residual ~ x1)
# Estimate squared Residual by Linear Form
Squared_Residual_Hat = linearMod_Residual2$fitted.values
# Variance Covariance Matrix for the Uncertainty u
Squared_Residual_Hat_vector = as.vector(Squared_Residual_Hat)
Lambda_hat = diag( Squared_Residual_Hat_vector )


# WLS estimator
nrow = length(df2$x1)
X = cbind(rep(1,nrow),df2$x1)
y = df2$y
beta_WLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) %*% t(X)%*% solve(Lambda_hat)  %*% y 
round(beta_WLS, digits = 4)


###error because the estimated squared residual is zero
# Variance Covariance Matrix for WLS estimator 
Var_Cov_beta_WLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) 
Var_beta_WLS = diag(Var_Cov_beta_WLS)
sd_beta_WLS = sqrt(Var_beta_WLS)
round(sd_beta_WLS, digits = 4)

#############
# HR
#############
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
round(sd_beta_OLS_HR, digits = 4)




###########################################################################################
## Panel Data Analysis
###########################################################################################
library(AER)  
data("PSID7682")
# write.csv(PSID7682,"PSID7682.csv", row.names = FALSE)
df = PSID7682
head(df)
CASchools <- read.csv("PSID7682.csv", header=TRUE, sep=",")


###########################################################################################
## Approach2: Loading data by Reading the data file CASchoolsData.csv under the same folder
###########################################################################################
CASchools <- read.csv("CASchoolsData.csv", header=TRUE, sep=",")
head(CASchools)

