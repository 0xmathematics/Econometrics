########################
# Assignment11: Instrumental Variable: Fertility and labor supply
########################


##############################
## Generate the dataset
##############################
#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment11")

library(AER)  
data("Fertility")
df = Fertility

#Change Column Names
colnames(df)
colnames(df)[which(names(df) == "gender1")] = 'boy1st'
colnames(df)[which(names(df) == "gender2")] = 'boy2nd'
colnames(df)[which(names(df) == "afam")] = 'black'
colnames(df) 

#fill in the binary variables into this empty columns
head(df)
df$numerica_gender[which(df$boy1st == "female")] = 0
df$numerica_gender[which(df$boy1st == "male")] = 1
df <- subset(df, select = -c(boy1st))
colnames(df)[which(names(df) == "numerica_gender")] = 'boy1st'
colnames(df)

## incomplete for this approach


##############################
## Approach 2: Generate the data
##############################
#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment11")
library('readxl')
dtibble <- read_excel("fertility.xlsx", col_types = "numeric")
df = data.frame(dtibble)
head(df)
colnames(df)
colnames(df)[which(names(df) == "agem1")] = 'age'
colnames(df)[which(names(df) == "hispan")] = 'hispanic'
colnames(df)[which(names(df) == "othrace")] = 'other'
colnames(df)[which(names(df) == "weeksm1")] = 'work'

# write.csv(df,"Fertility.csv", row.names = FALSE)

###########################################################################################
## Loading data 
###########################################################################################
#mac

#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment11")

df <- read.csv("Fertility.csv")
head(df)

##############################
## OLS Linear Regression
##############################
is.data.frame(df) 
colnames(df)
# Q(a).i
#Sigle variate lm
OLS_model_1 <- lm(work ~ morekids, data = df)
summary(OLS_model_1)
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


