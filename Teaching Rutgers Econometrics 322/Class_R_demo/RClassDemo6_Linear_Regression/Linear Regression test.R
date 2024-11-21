############################################################################
# Class demo 6: Linear Regression
############################################################################

############################################################################
library('readxl')
#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/class demo/RClassDemo6_Linear_Regression")
##############################
## Loading data
##############################
## load excel file
dtibble <- read_excel("cps08.xlsx", col_types = "numeric")
df = data.frame(dtibble)
head(df)
nrow(df)



### matrix method
y = df$ahe
X = df[ , c(3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

u_hat = y- X %*%betaOLS
SSR2 = sum(u_hat^2)
SSR2
SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-3-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

# Using Software Package 
OLS <- lm(ahe ~ bachelor+female+age, data=df)
summary(OLS) 
summary(OLS)$coef
betaOLS
sdBetaOLS

# z statistics under default Null Hypothesis H_0: beta_k = 0 vs H_1: beta_k != 0
z_stat = ( betaOLS - 0)/sdBetaOLS
z_stat
summary(OLS)$coef[,3]
library("sandwich")
#https://www.rdocumentation.org/packages/sandwich/versions/0.1-1/topics/vcovHC

VarCovMatrixBetaOLS  # homo
vcovHC(OLS, type = 'const') # homo
vcovHC(OLS, type = 'HC0') #Hetro 1
vcovHC(OLS, type = 'HC1') #Hetro 2



# same thing to write in the following format
# homo t-stat
coeftest(OLS, vcov. = vcovHC(OLS, type = 'const'))[,3]
coeftest(OLS, vcov. = vcovHC, type = "const")[,3]

# homo sd
coeftest(OLS, vcov. = vcovHC, type = "const")[,2]
sdBetaOLS

### Heteroskedasticity Robust
betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y
u_hat = y- X %*%betaOLS
residual_square = as.vector(u_hat^2)
Lambda_hat  = diag(residual_square)
#HR vcov matrix
Var_Cov_beta_OLS_HR = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )
VarBetaOLS_HR = diag(Var_Cov_beta_OLS_HR)
# HR standard deviation
sdBetaOLS_HR = sqrt(VarBetaOLS_HR)  

# compare the result with software package
Var_Cov_beta_OLS_HR
vcovHC(OLS, type = 'HC0') #Hetro 1

# same thing to write in the following format
# t stat
coeftest(OLS, vcov. = vcovHC(OLS, type = 'HC0'))[,3]
coeftest(OLS, vcov. = vcovHC, type = "HC0")[,3]

# HR sd
coeftest(OLS, vcov. = vcovHC, type = "HC0")[,2]
sdBetaOLS_HR

# homo sd
coeftest(OLS, vcov. = vcovHC, type = "const")[,2]
sdBetaOLS

#CI homo (same as using t statistics and homo skedasticity)
confint(OLS, level = 0.95)
confint(OLS, vcov = vcovHC(OLS, type = 'const'), level = 0.95)
#CI hetero  DOES NOT WORK!!!
confint(OLS, vcov = vcovHC(OLS,type = 'HC0'), level = 0.95)


# 95% CI
betaOLS
sdBetaOLS #homo sd
sdBetaOLS_HR #hetero sd
beta1 = 8.0830009
threshold_z = qnorm(0.975)
deg = n-ncol(X)
threshold_t = qt(0.975,deg)

# HOMO
# using z-statistics and homo sd
left = betaOLS - threshold_z * sdBetaOLS
right = betaOLS + threshold_z * sdBetaOLS
CI95 = cbind(left,right)
CI95
# using t-statistics and homo sd
left = betaOLS - threshold_t * sdBetaOLS
right = betaOLS + threshold_t * sdBetaOLS
CI95 = cbind(left,right)
CI95
confint(OLS, level = 0.95) # SAME!!!!!!

# Hetero
# using z-statistics and Hetero sd
left = betaOLS - threshold_z * sdBetaOLS_HR
right = betaOLS + threshold_z * sdBetaOLS_HR
CI95 = cbind(left,right)
CI95
# using t-statistics and Hetero sd
left = betaOLS - threshold_t * sdBetaOLS_HR
right = betaOLS + threshold_t * sdBetaOLS_HR
CI95 = cbind(left,right)
CI95
confint(OLS, level = 0.95) # NOT SAME!!!!!!
#this comand does not work!!!!!
confint(OLS,  vcov = vcovHC(OLS, type = 'HC0'), level = 0.95) 
# Use the following to ge Hetero CI
HETERO_VarCov = vcovHC(OLS, type = 'HC0')
sd_HR = sqrt(diag(HETERO_VarCov))
left = betaOLS - threshold_t * sd_HR
right = betaOLS + threshold_t * sd_HR
CI95 = cbind(left,right)
CI95

#######################################################
HOMO_VarCov = vcovHC(OLS, type = 'const')
HETERO_VarCov = vcovHC(OLS, type = 'HC0')
HOMO_VarCov
HETERO_VarCov
sd = sqrt(diag(HOMO_VarCov))
sd_HR = sqrt(diag(HETERO_VarCov))
sd
sdBetaOLS
sd_HR
sdBetaOLS_HR
#####################################################


vcovHC(OLS, type = 'HC0')

# 
# z statistics
z = (8.0830009 - 5)/ 0.20881336

# t statistics
t = (8.0830009 - 5)/ 0.20881336

# Critical Value for z and t statistics for H_1: beta_k > 5 at 1% significant level
qnorm(0.99)
deg = n-3-1
qt(0.99, deg)


2.326348
# P-value for z and t for H_1: beta_k > 5 at 1% significant level
z
1- pnorm(z)
1- pt(t,deg)

# 99% CI
beta1 = 8.0830009
threshold = qnorm(0.995)
sd_beta1 = 0.20881336

left = beta1- threshold * sd_beta1
right = beta1 + threshold* sd_beta1
CI99 = c(left,right)
CI99

#CI homo
confint(OLS, level = 0.99)
confint(OLS, vcov = vcovHC(OLS, type = 'const'), level = 0.99)
#CI homo
confint(OLS, vcov = vcovHC(OLS,type = 'HC0'), level = 0.99)
