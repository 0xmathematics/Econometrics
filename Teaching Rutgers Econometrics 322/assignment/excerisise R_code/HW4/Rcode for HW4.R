## Rcode for HW4
## Hang Miao

#######################################
#question 4
#######################################

beta0 = 546.42
beta1 = -6.111
CS = 22

OLS_testscore = beta0 + beta1*CS
round(OLS_testscore, digits = 2)

#change
deltaCS = 23-19
deltaOLS_testscore = beta1*deltaCS
round(deltaOLS_testscore, digits = 2)

#sample average
bar_CS = 22.47
bar_OLS_testscore = beta0 + beta1*bar_CS
round(bar_OLS_testscore, digits = 2)


#sample standard deviation for test scores
SER = 12.1
R2 = 0.09
n = 93
SSR = SER^2*(n-2)
TSS = SSR/(1-R2)
sd_Y = sqrt(TSS/(n-1))
sd_Y
round(sd_Y, digits = 1)

#######################################
#question 5
#######################################

## import the 'readxl' package to read excel files
library('readxl')
## set the working directory same as the 
setwd("replace this with the Directory Path where this file and the excel data file located")

## load excel file
dtibble <- read_excel("data-2_25_2019-10_02 PM.xlsx", skip = 1, col_types = "numeric")
df = data.frame(dtibble)
head(df)

############################
## Median of height
##############################
median_Height = median(df$Height)
round(median_Height, digits = 2)

############################
##average earning for height at most 67
##############################
rowIndex = which(df['Height']<=67)
df_leq67 = df[rowIndex,]
avg_leq67 = mean(df_leq67$Earnings)
round(avg_leq67, digits = 2)

############################
##average earning for height greater than 67
##############################
rowIndex = which(df['Height']>67)
df_g67 = df[rowIndex,]
avg_g67 = mean(?????)
round(avg_g67, digits = 2)

############################
##Difference
##############################
diff_earning = abs(????? -?????)
round(diff_earning, digits = 2)

##############################
## 95% CI
##############################
mu1 = mean(df_g67$Earnings)
mu2 = mean(df_leq67$Earnings)
sd1 = sd(?????$Earnings)
sd2 = sd(??????$Earnings)
n1 = nrow(df_g67)
n2 = nrow(?????)

mu_diff = abs(mu1-mu2)
sd_diff = sqrt(??????)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

CI_right = mu_diff +  sd_diff*percentile_975
CI_left = ?????

round(c(CI_left,CI_right), digits = 2)


##############################
## Linear Regression
##############################
# training the model
linearMod <- lm(Earnings ~ Height, data=df)
# show coefficients
print(linearMod)
# or
beta = linearMod$coefficients
round(beta, digits = 2)
# show detailed results
summary(linearMod) 



# prediction
testdata <- data.frame(Height = 66)
predicted_earning <- predict(linearMod,testdata)  # predict distance

predicted_earning = as.numeric(predicted_earning)  
round(predicted_earning,digits = 2)

##############################
## change measurement unit
##############################

df['Height_cm'] = df$Height*2.54

# train the model with new regressor
linearMod <- lm(Earnings ~ Height_cm, data=df)
# show coefficients
beta = linearMod$coefficients
round(beta, digits = 2)

##############################
## R square
##############################
## https://www.rdocumentation.org/packages/stats/versions/3.5.2/topics/summary.lm
Rsquare = summary(linearMod)$r.squared
round(Rsquare, digits =4 )

##############################
## SER
##############################
n = nrow(df)
TSS = sum((df$Earnings - mean(df$Earnings))^2 )      

SSR = (1-Rsquare)*TSS

SER = sqrt(SSR/(n-2)) 

round(SER, digits = 2)

## or check from the summary




#######################################
#question 6  distance from college and years of education
#######################################


## load excel file
dtibble <- read_excel("data-2_26_2019-4_26 AM.xlsx", skip = 1, col_types = "numeric")
df = data.frame(dtibble)
head(df)


## training the model
linearMod <- lm(Ed ~ Dist, data=df)
## coefficients
Mod_coef = linearMod$coefficients
round(Mod_coef, digits = 3)

testdata <- data.frame(Dist = 2.6)
predicted_educ <- predict(linearMod,testdata)  # predict distance

predicted_educ = as.numeric(predicted_educ)  
round(predicted_educ,digits = 2)

# R square
Rsquare = summary(??????)$r.squared
round(Rsquare, digits =4 )

## SER
n = nrow(df)
TSS = sum((df$Ed - mean(df$Ed))^2 )      

SSR = ??????

SER = sqrt(?????) 

round(SER, digits = 4)
## or from 
summary(linearMod)
####################################################################
 #question 9
####################################################################
x = c(99,130,160)
y = 46 + 0.59* x
round(y, digits = 2)

x = 44
delta_y = 0.59* x
round(delta_y, digits = 2)





####################################################################
### sorting example
####################################################################
# sorting examples using the mtcars dataset
attach(mtcars)

# sort by mpg
newdata <- mtcars[order(mpg),] 

# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 

detach(mtcars)
####################################################################
### END sorting example
####################################################################








