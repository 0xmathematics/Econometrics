## Rcode for HW8
## Hang Miao

##############################################################################
#Review Concept 8.3
############################################################################
# change of GDP
beta1_GDP = 4.89
beta2_r = -0.03

dgGDP = 7
beta1_GDP*dgGDP

# change of interest rate
dr = 3
beta2_r*dr

##############################################################################
#Excercise 8.1
###############################################################################

S2010 = 254
S2009 = 192

# percentage change
pct = 100*(S2010-S2009)/S2009
round(pct,digits = 3)
 
# Log difference approximation
logdiff = 100*( log(S2010) - log(S2009) )
round(logdiff,digits = 3)

##############################################################################
#Excercise 8.4
###############################################################################
#################
# parameters 
################
  beta1_edu=
  beta2_female =
  beta3_female_edu =
  beta4_exp =
  beta5_exper2 =
  beta6_midwest =
  beta7_south =
  beta8_west =
  
  X1_edu=
  X2_female =
  X3_female_edu =
  X4_exp =
  X5_exper2 =
  X6_midwest =
  X7_south =
  X8_west =
###########################
 
 # if exper increase by 1, how much ahe change
beta4_exp = 0.0142
sd_beta4_exp = 0.0012
beta5_exper2 = -0.000232
sd_beta5_exper2= 0.000024

#scenario A
X4_expA = 3
#one way
dahe = beta4_exp*(X4_expA+1) +beta5_exper2*(X4_expA+1)^2-
  beta4_exp*(X4_expA) -beta5_exper2*(X4_expA)^2
daheA = 100*dahe
round(daheA,digits = 2)

# the other: an approximation for the first one
dahe = beta4_exp + 2*beta5_exper2*X4_expA
round(dahe*100,digits = 2)

#scenario B
X4_expB = 10

#one way
dahe = beta4_exp*(X4_expB+1) +beta5_exper2*(X4_expB+1)^2-
  beta4_exp*(X4_expB) -beta5_exper2*(X4_expB)^2
daheB = 100*dahe
round(daheB,digits = 2)

# the other: an approximation for the first one
dahe = beta4_exp + 2*beta5_exper2*X4_expB
round(dahe*100,digits = 2)



# t-statistics for the difference. Could be negative
diff_stat =(beta5_exper2-0)/sd_beta5_exper2
round(diff_stat,digits = 2)

##############################################################################
#Excercise 8.7
###############################################################################
#################
# parameters 
################
beta0_intercept = 6.51
sd_beta0_intercept = 0.01
beta1_female = -0.43
sd_beta1_female = 0.05

#avg Earning for male
X1_female =0
Y = beta0_intercept + beta1_female*X1_female
Y_male = exp(Y)
round(Y_male, digits = 2)
#avg Earning for female
X1_female =1
Y = beta0_intercept + beta1_female*X1_female
Y_female = exp(Y)
round(Y_female, digits = 2)
#difference
Y_male-Y_female

#t statistics female
t_stat_female = (beta1_female-0)/sd_beta1_female
round(t_stat_female, digits = 2)

# with two more regressor: marketcap and return. 
beta2_marketV = 0.37
sd_beta2_marketV = 0.004
beta3_return =0.004
sd_beta3_return = 0.003

dmarketV = 4.48
dY = dmarketV*beta2_marketV
round(dY, digits = 2)

# omitted variable bias formula
beta1_female_original = beta1_female
beta1_female_new= -0.28
sigmaRatio_u_x = 0.46
rho_x_u = (beta1_female_original-beta1_female_new)/sigmaRatio_u_x
round(rho_x_u, digits = 3)

##############################################################################
#Excercise 8.2
###############################################################################
library('readxl')
##############################
## Loading data
##############################
## load excel file
dtibble <- read_excel("cps08.xlsx", col_types = "numeric")
df = data.frame(dtibble)
head(df) # show the first 6 rows of the data frame
colnames(df) # show the column names

# multiple regression
linearMod1 <- lm(ahe ~ female+bachelor+age, data=df)
summary(linearMod1) 
summary(linearMod1)$coef

# add a new column 'log_ahe' which sotre the value of log(ahe) to the original data frame 
df['log_ahe'] = log(df$ahe)
# Re run the multiple regression
linearMod2 <- lm(log_ahe ~ female+bachelor+age, data=df)
summary(linearMod2) 
summary(linearMod2)$coef

# add a new column 'log_age' which sotre the value of log(age) to the original data frame 
df['log_age'] = log(df$age)
# Re run the multiple regression
linearMod3 <- lm(log_ahe ~ female+bachelor+log_age, data=df)
summary(linearMod3) 
summary(linearMod3)$coef

dahe = 1/35*100*0.80390509
dahe

# add a new column 'log_age' which sotre the value of log(age) to the original data frame 
head(df)
df['age2'] = (df$age)^2
# Re run the multiple regression
linearMod4 <- lm(ahe ~ female+bachelor+age+age2, data=df)
summary(linearMod4) 
summary(linearMod4)$coef

X1_female = 1
X2_bachelor = 1
X3_age = 40
X4_age2 = 40^2
X = c(1, X1_female,X2_bachelor,X3_age,X4_age2)
Y_ahe = linearMod4$coefficients %*% X
round(Y_ahe,digits = 2)


# Joint Test
head(df)
########################################
# One way: likelihood ratio principle
#######################################
# Restricted multiple regression
linearMod_r <- lm(ahe ~ female+bachelor, data=df)
# UnRestricted multiple regression
linearMod_u <- lm(ahe ~ female+bachelor+age+age2, data=df)

SSR_R = sum(summary(linearMod_r)$residuals^2)
SSR_U = sum(summary(linearMod_u)$residuals^2)

n = nrow(df)
p = 4
d = 2

F_stat = (SSR_R- SSR_U)/d / (SSR_U/(n-p-1))
round(F_stat, digits = 1)
q95 = qf(0.95,2, n-p-1 )

F_stat>q95

#######################################
## The other way: Wald Principle
#######################################

colnames(df)
d = 2  #number of constraint in Null hypothesis
p = 4  #number of regressor
# data matrix X
vec_ones = rep(1, nrow(df))  
X =  cbind( vec_ones ,df[,c("female","bachelor","age","age2" )])
#typeof(X)
X = as.matrix(X)

# Null hypothess is a linear constraint: A*beta =a
# constraint matrix A 
row1 = c(0,0,0,1,0)
row2 = c(0,0,0,0,1)
A = matrix(rbind(row1,row2),nrow =d, ncol =p+1 )
a = c(0,0)

# beta_hat 
linearMod <- lm(ahe ~ female+bachelor+age+age2, data=df)
beta_hat = linearMod$coefficients

# estimated sigma^2, i.e SER^2
sigma2 = summary(linearMod)$sigma^2

# F stat of wald principle expression
F_stat_WaldForm = 
t(A %*% beta_hat-a) %*% solve( sigma2*A%*%solve( t(X)%*%X) %*% t(A)  ) %*% (A %*% beta_hat-a)/d

# same as the F_stat calculated by previous likelihood principle
F_stat_WaldForm
F_stat


# interaction of age and female
# add one colume regarding interaction terms
df['female_age'] =  df$female * df$age
head(df)
linearMod <- lm(log_ahe ~ female+bachelor+age+female_age, data=df)
summary(linearMod)

# interaction of age and college
# add one colume regarding interaction terms
df['college_age'] =  df$bachelor * df$age
head(df)
linearMod <- lm(log_ahe ~ female+bachelor+age+college_age, data=df)
summary(linearMod)


# prediction
X1_female = 1
X2_bachelor = 1
X3_age = 40
X4_college_age = X3_age*X2_bachelor
X = c(1, X1_female,X2_bachelor,X3_age,X4_college_age)

Y_log_ahe = linearMod$coefficients %*% X
round(Y_log_ahe,digits = 2)
