## Rcode for HW5
## Hang Miao

##############################################################################
#question 1
##############################################################################
##############################
## 95% CI
##############################
beta0Hat = 46.72
se = 12.4
n = 24
deg = n - 2

percentile_975 = qt(0.975, df = deg, lower.tail = TRUE, log.p = FALSE)

CI_right = beta0Hat +  se*percentile_975
CI_left = beta0Hat -  se*percentile_975
  
round(c(CI_left,CI_right), digits = 2)
##############################
## 5% t-statistics
##############################
beta1Hat = 65.68
betaNull = 55
se = 8.3
t_stat = (beta1Hat-betaNull)/se
round(t_stat, digits = 2)

# reject Null in favor of two sided Aternative at 5% siginificant level?
percentile_975 < t_stat

# reject Null in favor of one sided Aternative at 5% siginificant level?

percentile_95 = qt(0.95, df = deg, lower.tail = TRUE, log.p = FALSE)
percentile_95 < t_stat

##############################################################################
#question 2
##############################################################################
##############################
## Loading data
##############################
library('readxl')
## set the working directory same as the 
setwd("replace this with the Directory Path where this file and the excel data file located")
setwd("~/Dropbox/Econometrics/R Emprical Analysis/HW5")

## load excel file
dtibble <- read_excel("data-2_27_2019-7_08 PM.xlsx", skip = 1, col_types = "numeric")
df = data.frame(dtibble)
head(df)

##############################
## Linear Regression
##############################
# training the model
linearMod <- lm(Growth ~ TradeShare, data=df)

##############################
## t-statistics and p-value
##############################
# Aproach one: show detailed results from summary()
summary(linearMod) 

# Aproach two: calculate the value explicitely
beta1Hat = 1.6795
betaNull = 0
se = 0.9876
n = nrow(df)
deg = n-2
## t statistics
t_stat = (beta1Hat-betaNull)/se
round(t_stat, digits = 3)
## 5% significant test
quantile_975 = qt(0.975, df = deg, lower.tail = TRUE, log.p = FALSE)
t_stat > quantile_975
## p-value
pvalue = 2*pt(t_stat, df = deg, lower.tail = F, log.p = FALSE)
round(pvalue, digits = 3)

##############################
## 90% Confidence Interval
##############################

beta1Hat = 1.6795
se = 0.9876
n = nrow(df)
deg = n - 2

percentile_95 = qt(0.95, df = deg, lower.tail = TRUE, log.p = FALSE)

CI_right = beta1Hat +  se*percentile_95
CI_left = beta1Hat -  se*percentile_95

round(c(CI_left,CI_right), digits = 3)


##############################################################################
#question 4
##############################################################################
##############################
## 95% Confidence Interval
##############################

beta1Hat = -5.6454
se = 2.3868
#n = 100
#deg = n - 2

#percentile_975 = qt(0.975, df = deg, lower.tail = TRUE, log.p = FALSE)
percentile_975 = qnorm(0.975, lower.tail = TRUE, log.p = FALSE)
CI_right = beta1Hat +  se*percentile_975
CI_left = beta1Hat -  se*percentile_975

round(c(CI_left,CI_right), digits = 2)

##############################
## t statistics
##############################
beta1Hat = -5.6454
betaNull = 0
se = 2.3868
#n = 100
#deg = n-2
## t statistics
t_stat = (beta1Hat-betaNull)/se
round(t_stat, digits = 4)

##############################
## p-value
##############################

#### still use t distribution
pvalue = 2*pt(t_stat, df = deg, lower.tail = TRUE, log.p = FALSE)
round(pvalue, digits = 4)

#### use normal to approximate
pvalue = 2*pnorm(t_stat, lower.tail = TRUE, log.p = FALSE)
round(pvalue, digits = 4)

#### new H0
beta1Hat = -5.6454
betaNull = -5.4
se = 2.3868
#n = 100
#deg = n-2
## t statistics
t_stat = (beta1Hat-betaNull)/se
round(t_stat, digits = 4)
pvalue = 2*pnorm(t_stat, lower.tail = TRUE, log.p = FALSE)
round(pvalue, digits = 4)
##############################
## 99% Confidence Interval
##############################

beta0Hat = 504.788
se = 19.788
#n = 100
#deg = n - 2

#percentile_995 = qt(0.995, df = deg, lower.tail = TRUE, log.p = FALSE)
percentile_995 = qnorm(0.995, lower.tail = TRUE, log.p = FALSE)
CI_right = beta0Hat +  se*percentile_995
CI_left = beta0Hat -  se*percentile_995

round(c(CI_left,CI_right), digits = 1)

##############################################################################
#question 5
##############################################################################

##############################
## Loading data
##############################
## load excel file
dtibble <- read_excel("data-2_28_2019-2_29 AM.xlsx", skip = 1, col_types = "numeric")
df = data.frame(dtibble)
head(df)

##############################
## Linear Regression
##############################
# training the model
linearMod <- lm(Earnings ~ Height, data=df)
summary(linearMod) 

##############################
## Heteroskedasticity-Robust SE
##############################
X = df$Height
Y = df$Earnings
n = nrow(df)
predicted_Earnings <- predict(linearMod, df['Height']) 
Y_hat <- as.numeric(predicted_Earnings)  

u_hat <- Y - Y_hat
HR_sVar = 1/n*   1/(n-2)*sum( (X-mean(X))^2 *u_hat^2 ) / ( 1/n * sum((X-mean(X))^2) )^2

HR_SE = sqrt(HR_sVar) 

##############################
## 95% CI
##############################
beta1hat = 609.5
#percentile_995 = qt(0.995, df = deg, lower.tail = TRUE, log.p = FALSE)
percentile_95 = qnorm(0.975, lower.tail = TRUE, log.p = FALSE)
CI_right = beta1hat +  HR_SE*percentile_95
CI_left = beta1hat -  HR_SE*percentile_95

round(c(CI_left,CI_right), digits = 3)  
  
#####################################
## Linear Regression on female worker
#####################################
#select the subset

rowIndex = which( df['Sex']== 0 )
df_female = df[rowIndex, ]
# training the model
linearMod <- lm(Earnings ~ Height, data=df_female)
summary(linearMod) 

##############################
## Heteroskedasticity-Robust SE for female
##############################
X = df_female$Height
Y = df_female$Earnings
n = nrow(df_female)
predicted_Earnings <- predict(linearMod, df_female['Height']) 
Y_hat <- as.numeric(predicted_Earnings)  

u_hat <- Y - Y_hat
HR_sVar = 1/n*   1/(n-2)*sum( (X-mean(X))^2 *u_hat^2 ) / ( 1/n * sum((X-mean(X))^2) )^2

HR_SE_F = sqrt(HR_sVar) 

##############################
## 95% CI
##############################
beta1hat = -2931
#percentile_995 = qt(0.995, df = deg, lower.tail = TRUE, log.p = FALSE)
percentile_975 = qnorm(0.975, lower.tail = TRUE, log.p = FALSE)
CI_right = beta1hat +  HR_SE_F*percentile_975
CI_left = beta1hat -  HR_SE_F*percentile_975

round(c(CI_left,CI_right), digits = 3)  


#####################################
## Linear Regression on male worker
#####################################
#select the subset
rowIndex = which( df['Sex']== 1 )
df_male = df[rowIndex, ]
# training the model
linearMod <- lm(Earnings ~ Height, data=df_male)
summary(linearMod) 

##############################
## Heteroskedasticity-Robust SE for male
##############################
X = df_male$Height
Y = df_male$Earnings
n = nrow(df_male)
predicted_Earnings <- predict(linearMod, df_male['Height']) 
Y_hat <- as.numeric(predicted_Earnings)  

u_hat <- Y - Y_hat
HR_sVar = 1/n*   1/(n-2)*sum( (X-mean(X))^2 *u_hat^2 ) / ( 1/n * sum((X-mean(X))^2) )^2

HR_SE_M = sqrt(HR_sVar) 

##############################
## 95% CI
##############################
beta1hat = 7032
#percentile_995 = qt(0.995, df = deg, lower.tail = TRUE, log.p = FALSE)
percentile_975 = qnorm(0.975, lower.tail = TRUE, log.p = FALSE)
CI_right = beta1hat +  HR_SE_M*percentile_975
CI_left = beta1hat -  HR_SE_M*percentile_975

round(c(CI_left,CI_right), digits = 3)  


##############################
## Mean difference Hypothesis Test
##############################

beta1_M = 7032
beta1_F = -2931
sd1 = HR_SE_M
sd2 = HR_SE_F
se = sqrt(sd1^2 + sd2^2)

t_stat = (beta1_M -beta1_F )/se
pvalue = 2*pnorm(t_stat, mean = 0, sd = 1, lower.tail = F, log.p = FALSE )
pvalue<0.1  # 10% significant level
pvalue<0.05 # 5% significant level
pvalue<0.01 # 1% significant level


##############################################################################
#question 9
##############################################################################


##############################
## t statistics
##############################
beta1Hat = -2.28
betaNull = 0
se = 0.52

t_stat = (beta1Hat-betaNull)/se
round(t_stat, digits = 4)




