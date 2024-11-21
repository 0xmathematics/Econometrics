########################
# Assignment9: Heteroskedasticity
########################

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
## HOMO1
##############################
u = rnorm(N, 0, 20)
y = beta0+ beta1*x1+u

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_1.csv",row.names = FALSE)
##############################
## HOMO3
##############################
u = rnorm(N, 0, 30)
beta0 = 3
beta1 = 2
y = beta0+ beta1*x1+u

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_3.csv",row.names = FALSE)
##############################
## HOMO4
##############################
u = rnorm(N, 0, 5)
beta0 = 5
beta1 = 8
y = beta0+ beta1*x1+u

data = cbind(y,x1)
head(data)

write.csv(data,"Dataset_Hetero_4.csv",row.names = FALSE)



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
plot(df1$x1,linearMod1$residuals^2 )

linearMod1 <- lm(y ~ x1, data=df3)
summary(linearMod1) 
summary(linearMod1)$coef
linearMod1$coefficients
linearMod1$residuals^2
plot(df3$x1,linearMod1$residuals^2 )

linearMod1 <- lm(y ~ x1, data=df4)
summary(linearMod1) 
summary(linearMod1)$coef
linearMod1$coefficients
linearMod1$residuals^2
plot(df4$x1,linearMod1$residuals^2 )

linearMod2 <- lm(y ~ x1, data=df2)
summary(linearMod2) 
summary(linearMod2)$coef
linearMod2$coefficients
linearMod2$residuals^2
plot(df2$x1,linearMod2$residuals^2  )


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
linearMod_Residual2 <- lm(Squared_Residual ~ x1, data =df2)
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
round(beta_WLS, digits = 3)


###error because the estimated squared residual is zero
# Variance Covariance Matrix for WLS estimator 
Var_Cov_beta_WLS = solve( t(X) %*% solve(Lambda_hat) %*% X ) 
Var_beta_WLS = diag(Var_Cov_beta_WLS)
sd_beta_WLS = sqrt(Var_beta_WLS)
round(sd_beta_WLS, digits = 4)

#############
# HR
#############
linearMod2 <- lm(y ~ x1, data=df2)
Squared_Residual = linearMod2$residuals^2
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
round(sd_beta_OLS_HR, digits = 3)




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
