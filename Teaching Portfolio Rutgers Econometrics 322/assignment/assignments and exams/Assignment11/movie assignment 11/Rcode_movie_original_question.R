######################################################
# Assignment11: Instrumental Variable: Movie
######################################################



##############################
## Approach 2: Generate the data
##############################
#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment11")
library('readxl')
dtibble <- read_excel("Movies.xlsx")
df = data.frame(dtibble)
head(df)
colnames(df)
colnames(df)[which(names(df) == "agem1")] = 'age'
colnames(df)[which(names(df) == "hispan")] = 'hispanic'
colnames(df)[which(names(df) == "othrace")] = 'other'
colnames(df)[which(names(df) == "weeksm1")] = 'work'

# write.csv(df,"Movie.csv", row.names = FALSE)

###########################################################################################
## Loading data 
###########################################################################################
#mac

#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment11")

df <- read.csv("Movie.csv")
head(df)

##############################
## OLS Linear Regression
##############################
is.data.frame(df) 
colnames(df)
length(df[,2])
# Q(a).i
OLS_model_1 <- lm( log(assaults) ~ year1+year2 +year3 +year4 +year5 +year6 +year7 +year8 +year9+
                    month1+ month2 + month3+ month4+ month5+ month6+ month7+ month8+ month9+ month10+ month11, data = df)

####################
# Joint Test: LR Principle
####################
# F stat on all month ==0
SSR_U = sum( ( OLS_model_1$residuals )^2 )
OLS_model_1_restricted <- lm( log(assaults) ~ year1+year2 +year3 +year4 +year5 +year6 +year7 +year8 +year9
                     , data = df)
SSR_R = sum( ( OLS_model_1_restricted$residuals )^2 )
n = nrow(df)
d = 11
p = 20
f = (SSR_R-SSR_U)/d / ( SSR_U/(n-p-1) )
f #76.25926

####################
# Joint Test: Wald Principle
####################
betaOLS = as.matrix(OLS_model_1$coefficients )

start_IX = which(colnames(df) == "year1" )
end_IX = length(colnames(df))
X_names = colnames(df)[start_IX:end_IX ]
delete_IX1 = which(X_names == "year10" )
delete_IX2 = which(X_names == "month12" )
X_names = X_names[-c(delete_IX1,delete_IX2)]

intercept = rep(1,n)
X = cbind(intercept,df[,X_names])
X = as.matrix(X)
head(X)
n = nrow(df)
p = nrow(betaOLS)-1
d = 11
sigmasqaureHat = sum(OLS_model_1$residuals^2)/(n-p-1)
# Conventional H_0:  month1 = 0, month2 = 0, ..., month11 = 0
A_1 = matrix(rep(0,d*(p+1-d)), nrow = d,ncol = 10 )
A_2 = diag(d)
A = cbind(A_1, A_2)
a = as.vector(rep(0,d))

Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
F_Stat = Wald_Stat/d
Wald_Stat #838.8519
F_Stat #76.25926

# Q(a).ii movie attandance on year and month
colnames(df)
OLS_model_1_b <- lm( (attend_v+attend_m+attend_n) ~ year1+year2 +year3 +year4 +year5 +year6 +year7 +year8 +year9+
                     month1+ month2 + month3+ month4+ month5+ month6+ month7+ month8+ month9+ month10+ month11, data = df)
summary(OLS_model_1_b)
# F stat on all month ==0
SSR_U = sum( ( OLS_model_1_b$residuals )^2 )
OLS_model_1_b_restricted <- lm( (attend_v+attend_m+attend_n) ~ year1+year2 +year3 +year4 +year5 +year6 +year7 +year8 +year9
                              , data = df)
SSR_R = sum( ( OLS_model_1_b_restricted$residuals )^2 )
n = nrow(df)
d = 11
p = 20
f = (SSR_R-SSR_U)/d / ( SSR_U/(n-p-1) )
f #33.72364

# Q(b).i assult on movie attandance and controls
colnames(df)
OLS_model_2_a <- lm( log(assaults) ~ attend_v+attend_m+attend_n+
                       year1+year2 +year3 +year4 +year5 +year6 +year7 +year8 +year9+
                       month1+ month2 + month3+ month4+ month5+ month6+ month7+ month8+ month9+ month10+ month11
                     + h_chris + h_newyr+ h_easter+ h_july4+ h_mem+ h_labor
                       + w_maxa + w_maxb+ w_maxc+ w_mina+ w_minb+ w_minc+ w_rain+ w_snow, data = df)
summary(OLS_model_2_a)
coeftest(OLS_model_2_a, vcov. = vcovHC, type = "const")
coeftest(OLS_model_2_a, vcov. = vcovHC, type = "HC0")

# Q(b).ii joint test attend_v == attend_m? attend_v == attend_n? 
colnames(df)
y = log(df$assaults)
X = subset(df, select = -c(year10,month12,wkd_ind,assaults, pr_attend_v, pr_attend_m, pr_attend_n, attend_v_f, attend_m_f, attend_n_f,attend_v_b,attend_m_b,attend_n_b) )
colnames(X)
ncol(X)
n = nrow(X)
intercept = rep(1,nrow(X))
X = cbind(intercept,X)
X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y
OLS_model_2_a$coefficients # same
length(betaOLS)
length(OLS_model_2_a$coefficients)

# attend_v == attend_m?
A = numeric(length(betaOLS))
A[2] = 1
A[3] = -1
A = t(as.matrix(A))
a = as.vector(0)
d = 1
sigmasqaureHat = summary(OLS_model_2_a)$sigma^2
Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
F_Stat = Wald_Stat/d # 0.001125994

# attend_v == attend_n? 
A = numeric(length(betaOLS))
A[2] = 1
A[4] = -1
A = t(as.matrix(A))
a = as.vector(0)
d = 1
sigmasqaureHat = summary(OLS_model_2_a)$sigma^2
Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
F_Stat = Wald_Stat/d # 1.219596

# Q(b).iii 6 beta1 -2 beta2 - beta3? 
6*betaOLS[2] -2*betaOLS[3] - betaOLS[4]

#Q1
Q1 = OLS_model_1$coefficients
round(Q1,digits = 3)

#Q2
library(sandwich)
library(lmtest)
# hetero sd with t critical value
# 95% CI
betaOLS = OLS_model_1$coefficients
sd_homo = sqrt( diag( vcovHC(OLS_model_1, type = 'const') ) )
sd_hetro = sqrt( diag( vcovHC(OLS_model_1, type = 'HC0') ) )
threshold_z = qnorm(0.975)
deg = nrow(df)- 2
threshold_t = qt(0.975,deg)
# 95% CI hetero and t critical
left = betaOLS - threshold_t * sd_hetro
right = betaOLS + threshold_t * sd_hetro
CI95 = cbind(left,right)
CI95
Q2 = round(CI95,digits = 3)
Q2

#Q3
#Q4 IV estimation
library(AER)
IV_model_2 <- ivreg(work ~ morekids|samesex, data = df)
Q4 = round(IV_model_2$coefficients,digits = 3)
Q4
coeftest(IV_model_2, vcov. = vcovHC, type = "const")
coeftest(IV_model_2, vcov. = vcovHC, type = "HC0")

#Q5
#Q6 testing the relavance of instrument
# weak instrument?
head(df)
IV_stage1_model_3 <- lm(morekids ~ samesex, data = df)
summary(IV_stage1_model_3)
summary(IV_stage1_model_3)$f
Q6 = round(summary(IV_stage1_model_3)$f,digits = 0)
Q6

#Q7
#Q8
IV_model_3 <- ivreg(work ~ morekids+ age+ black+ hispanic+ other|samesex + age+ black+ hispanic+ other, data = df)
coeftest(IV_model_3, vcov. = vcovHC, type = "const")
round(IV_model_3$coefficients, digits = 3)









#Statistical Inference
#homo
library(sandwich)
library(lmtest)
coeftest(OLS_model_1, vcov = vcovHC,type = "const")

#Hetro Robust
coeftest(OLS_model_1, vcov. = vcovHC,type = "HC0")
coeftest(OLS_model_1, vcov. = vcovHC(OLS_model_1, type = 'HC0'))
#CI homo
confint(OLS_model_1, vcov = vcovHC(OLS_model_1, type = 'const'), level = 0.95)
#CI heter command not valid
confint(OLS_model_1, vcov = vcovHC(OLS_model_1,type = "HC0"), level = 0.95)


# hetero sd with t critical value
# 95% CI
betaOLS = OLS_model_1$coefficients
sd_homo = sqrt( diag( vcovHC(OLS_model_1, type = 'const') ) )
sd_hetro = sqrt( diag( vcovHC(OLS_model_1, type = 'HC0') ) )
threshold_z = qnorm(0.975)
deg = nrow(df)- 2
threshold_t = qt(0.975,deg)
# 95% CI hetero and t critical
left = betaOLS - threshold_t * sd_hetro
right = betaOLS + threshold_t * sd_hetro
CI95 = cbind(left,right)
CI95

# Q(c) 
head(df)
# first stage
OLS_model_2 <- lm(morekids ~ samesex, data = df)
summary(OLS_model_2)
coeftest(OLS_model_2, vcov = vcovHC,type = "const")
coeftest(OLS_model_2, vcov. = vcovHC,type = "HC0")


# Q(d)
# Q(e)
# Q(f) 
# second stage
df$morekids_hat = OLS_model_2$fitted.values
OLS_model_3 <- lm(work ~ morekids_hat, data = df)
summary(OLS_model_3)
coeftest(OLS_model_3, vcov = vcovHC,type = "const")
coeftest(OLS_model_3, vcov. = vcovHC,type = "HC0")


# Control Variable
head(df)
# first stage
OLS_model_4_first <- lm(morekids ~ samesex+ age+ black+ hispanic+ other, data = df)
df$morekids_hat_controled = OLS_model_4_first$fitted.values
# second stage
IV_model_4_control <- lm(work ~ morekids_hat_controled+ age+ black+ hispanic+ other, data = df)
summary(IV_model_4_control)
coeftest(IV_model_4_control, vcov = vcovHC,type = "const")
coeftest(IV_model_4_control, vcov. = vcovHC,type = "HC0")

library(AER)
IV_DemCur <- ivreg(work ~ morekids+ age+ black+ hispanic+ other|samesex + age+ black+ hispanic+ other, data = df)
coeftest(IV_DemCur, vcov. = vcovHC, type = "const")


