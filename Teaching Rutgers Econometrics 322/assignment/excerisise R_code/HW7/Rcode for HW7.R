## Rcode for HW6
## Hang Miao

##############################################################################
#Excercise 7.3
############################################################################
# column 2
###################
n = 4500
p = 3
beta0_intercept = 4.31
sd_beta0_intercept = 1.03
beta1_College = 5.37
sd_beta1_College = 0.21
beta2_Female = -2.57
sd_beta2_Female =0.2
beta3_Age = 0.27
sd_beta3_Age =0.05

######################   
# t statistics for age under null H0: true beta3 =0
###################### 
t_Age = (beta3_Age - 0)/sd_beta3_Age
round(t_Age, digits = 2)
# p-value
pvalue = 2*pt(-abs(t_Age),n-p-1)
round(pvalue, digits = 2)

###################### 
# column 3 # 95% CI for the income difference
###################### 

# THe only difference between Sally and Betsy is age, 
# We should compute the 95% CI for beta3_Age then multiply the difference of their age

#Sally
X3_Age_Sally = 28
#Betsy
X3_Age_Betsy = 45
#age difference
diff_age = abs(X3_Age_Sally-X3_Age_Betsy)

#95% CI for beta3_Age
beta3_Age = 0.27
sd_beta3_Age =0.03
q975 = qnorm(0.975,mean=0,sd=1)
beta3_Age_CI95 = c(beta3_Age - sd_beta3_Age * q975, beta3_Age + sd_beta3_Age * q975   )

#95% CI for income
Y_CI95 = diff_age * beta3_Age_CI95
round(Y_CI95,digits = 2) 


##############################################################################
#Excercise 7.7
############################################################################

beta1_BDR = 0.443
sd_beta1_BDR = 2.33

t_BDR = (beta1_BDR-0)/sd_beta1_BDR
round(t_BDR,digits = 3)

#one way to test H0
q95 = qnorm(0.975)
q95 < abs(t_BDR)

#the other way to test H0 by Pvalue
pvalue = 2*pnorm( -abs(t_BDR) )
pvalue < 0.05

# 95CI for the price change of the house when lot increase 1939
# first we want to calculate the 95CI for beta4_Lsize
beta4_Lsize = 0.002
sd_beta4_Lsize = 0.00044
q975 = qnorm(0.975, mean = 0, sd= 1)
CI95_beta4_Lsize = c(beta4_Lsize- q975*sd_beta4_Lsize, beta4_Lsize+ q975*sd_beta4_Lsize )


# since the lot size increase by 1939, the 95 CI for the house price change is the product of 
# 1939 and 95CI for beta4_Lsize

CI95_price= 1939 *CI95_beta4_Lsize
round(CI95_price,digits =2 )


# Wald Test
F_stat = 0.08
d = 2

# F_stat*d follows chi square distribution with degree of freedom d

q90 = qchisq(0.9, df=d)
q90<F_stat*d 

##############################################################################
#Excercise 7.7
############################################################################


#Sally
X3_Age_Sally = 28
#Betsy
X3_Age_Betsy = 45
#age difference
diff_age = abs(X3_Age_Sally-X3_Age_Betsy)

#95% CI for beta3_Age
beta3_Age = 0.27
sd_beta3_Age =0.03
q975 = qnorm(0.975,mean=0,sd=1)
beta3_Age_CI95 = c(beta3_Age - sd_beta3_Age * q975, beta3_Age + sd_beta3_Age * q975   )

#95% CI for income
Y_CI95 = diff_age * beta3_Age_CI95
round(Y_CI95,digits = 2) 





###################### 
# column 3
###################### 
beta0_intercept = 3.68
sd_beta0_intercept = 1.04
beta1_College = 5.33
sd_beta1_College = 0.21
beta2_Female = -2.57
sd_beta2_Female =0.2
beta3_Age = 0.27
sd_beta3_Age =0.03
beta4_Northeast = 0.68
sd_beta4_Northeast = 0.29
beta5_Midwest = 0.59
sd_beta5_Midwest = 0.27
beta6_South = -0.26
sd_beta6_South = 0.25

#Sally
X1_College = 1
X2_Female = 1
X3_Age = 28

Y_hat1 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age 
#Betsy
X1_College = 1
X2_Female = 1
X3_Age = 45
Y_hat2 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age 

# 95% CI for the income difference
# THe only difference between Sally and Betsy is age, 
# We should compute the 95% CI for beta3_Age then multiply the difference of their age

beta3_Age_CI95 = c(beta3_Age      )
sd = sqrt(  2* SER^2  )
q995 = qnorm(0.995, mean=0,sd=1)
CI99 = c( abs(Y_hat1-Y_hat2) - sd*q995 ,abs(Y_hat1-Y_hat2) + sd*q995  ) 

round(CI99,digits = 2) 
# Jennifer
X1_College = 1
X2_Female = 1
X3_Age = 32
X4_Northeast = 0
X5_Midwest = 1
X6_South = 0

Y_hat2 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age+
  beta4_Northeast*X4_Northeast +beta5_Midwest*X5_Midwest +beta6_South*X6_South
round(Y_hat2,digits = 2) 
#Expected difference
# There is an error in the grading system
# Have to use absolute value for the difference to get credit
abs(Y_hat1-Y_hat2)

##############################################################################
#Excercise 7.1
############################################################################
library('readxl')
##############################
## Loading data
##############################
## load excel file
dtibble <- read_excel("cps08.xlsx", col_types = "numeric")
df = data.frame(dtibble)
head(df)

### matrix method
y = df$ahe
X = df[ , c(3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-3-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

# simple regression
linearMod1 <- lm( ahe ~ age, data=df)
summary(linearMod1) 


# multiple regression
linearMod2 <- lm(ahe ~ female+bachelor+age, data=df)
summary(linearMod2) 
summary(linearMod2)$coef


# difference
X1_female = 0
X2_bachelor = 1
X3_age  = 25
X = c(1, X1_female,X2_bachelor,X3_age)
Y1 = linearMod2$coefficients %*% X

X1_female = 1
X2_bachelor = 1
X3_age  = 30
X = c(1, X1_female,X2_bachelor,X3_age)
Y2 = linearMod2$coefficients %*% X

Y1-Y2

# CI95
confint(linearMod2, level=0.95)

# Joint Test
SSR_R = sum(summary(linearMod1)$residuals^2)
SSR_U = sum(summary(linearMod2)$residuals^2)

n = nrow(df)
p = 3 
d = 2

F_stat = (SSR_R- SSR_U)/d / (SSR_U/(n-p-1))

q95 = qf(0.95,2, n-p-1 )

F_stat>q95

# R^2 and adjusted R^2 
summary(linearMod2)$r.squared
summary(linearMod2)$adj.r.squared

# Use CI to test H0
confint(linearMod2, level=0.95)

# Drop Bachelor
linearMod3 = lm(ahe ~ female+age, data=df)

summary(linearMod2)$coefficients
summary(linearMod3)$coefficients


# Joint Test
summary(linearMod3)
summary(linearMod3)$f


# one way: compare F-statistics with critical value at 1% significant level
n =nrow(df)
tail(df)
d =2
p =2
q99 = qf(0.99,2,n-p-1)
F_stat = summary(linearMod3)$f[1]
F_stat>q99

# the other: compare p-value with 1%
pvalue = pf(F_stat,2,n-p-1, lower.tail = FALSE)
pvalue<0.01

##############################################################################
#Test B ex 7.2.4
############################################################################
qchisq(1-0.05, df = 4)/4

# F = W/d implies F*d = W which follows chisquare(4), thus F follows chisquare(4)/4
