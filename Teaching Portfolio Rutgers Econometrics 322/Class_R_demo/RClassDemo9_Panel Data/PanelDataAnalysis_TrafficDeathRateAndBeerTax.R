######################################################################################
# Class demo 9: Panel Data Regression
######################################################################################
##############################
## Econometrics 322 Hang Miao
##############################

#########################################################################################
## Approach1: Loading data by Using the third party package: Applied Econometrics with R
#########################################################################################
library(AER)
data(Fatalities)
head(Fatalities)
# write.csv(Fatalities,"FatalitiesData.csv", row.names = FALSE)

###########################################################################################
## Approach2: Loading data by Reading the data file FatalitiesData.csv under the same folder
###########################################################################################
Fatalities <- read.csv("FatalitiesData.csv", header=TRUE, sep=",")
head(Fatalities)


#########################################
## Data Preprocessing
#########################################
# Check if there is any missing data
is.data.frame(Fatalities) 

# Create the dependent variable fatal rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# Select the columns which will be used in our model
Fatalities <- Fatalities[ ,c('state', 'year', 'fatal_rate', 'beertax') ]
head(Fatalities)

###########################################################################################
## Pooled OLS
###########################################################################################
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/class demo/RClassDemo9_Panel Data")
df<- read.csv("TestFatal_BearTax.csv", header=TRUE, sep=",")
head(df)
#Software Package
OLS_model <- lm(fatal_rate~beertax, data = df)
###########################################################################################
#Estimation
y = df$fatal_rate;  
n = nrow(df)
X = df[ ,4]; intercept = rep(1,nrow(df)); X = cbind(intercept,X)
X = as.matrix(X); y = as.matrix(y)
betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y
#compare
betaOLS
summary(OLS_model)$coef
#homo
u_hat = y- X %*%betaOLS
SSR = t(u_hat )%*% (u_hat )
sigmasqaureHat = SSR/ (n-1-1)
sigmasqaureHat = as.numeric(sigmasqaureHat)
VarCov_Homo = sigmasqaureHat * solve(t(X) %*% X) 
VarBeta_homo = diag(VarCov_Homo)
sdBeta_homo = sqrt(VarBeta_homo)
#compare
sdBeta_homo
summary(OLS_model)$coef[,2]
#hetero
library("sandwich")
sqaured_residual = as.vector(u_hat^2)
Lambda_hat = diag(sqaured_residual)
VarCov_HR = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )
VarBeta_HR = diag(VarCov_HR)
sdBeta_HR = sqrt(VarBeta_HR)
#compare
VarCov_HR
vcovHC(OLS_model, type = 'HC0') #Hetro 1

library("lmtest")
coeftest(OLS_model, vcov. = vcovHC(OLS_model, type = 'HC0'))
sdBeta_HR


###########################################################################################
## Fixed Entity Effect Panel Data analysis 
###########################################################################################
#######################################
# Software Package
#install.packages("plm")
df<- read.csv("TestFatal_BearTax.csv", header=TRUE, sep=",")
head(df)
library(plm)
head(df)
PLM_model <- plm(fatal_rate ~ beertax, 
                              data = df,
                              index = c("state", "year"), 
                              model = "within")
library("sandwich")
library("lmtest")
coeftest(PLM_model)
#######################################
# Calculate the mean across the time for each state
Mean_X_i = aggregate(df$beertax, by=list(df$state), FUN=mean)
Mean_Y_i = aggregate(df$fatal_rate, by=list(df$state), FUN=mean)
head(Mean_X_i)
head(Mean_Y_i)
head(df)
# Change the column name of the above sample means
colnames(Mean_X_i) <- c("state", "Mean_X_i")
colnames(Mean_Y_i) <- c("state", "Mean_Y_i")
# left join the original Fatalities sets and Mean_Beertax_EF, Mean_FatalRate_EF respectively
df$X_E_demeaned <- with( merge(x = df, y = Mean_X_i, by = "state", all.x = TRUE), 
                                       beertax - Mean_X_i)
df$Y_E_demeaned <- with( merge(x = df, y = Mean_Y_i, by = "state", all.x = TRUE), 
                         fatal_rate - Mean_Y_i )
head(df)
# equivalent to "with" syntax
intermediate_df = merge(x = df, y = Mean_X_i, by = "state", all.x = TRUE)
df$X_E_demeaned <- intermediate_df$beertax - intermediate_df$Mean_X_i



#Estimation
y = df$Y_E_demeaned;  
n = nrow(df)
X = df$X_E_demeaned ; intercept = rep(1,nrow(df)); X = cbind(intercept,X)
X = as.matrix(X); y = as.matrix(y)
betaPLM = solve( t(X) %*% X ) %*% t(X) %*% y
#compare: equal
betaPLM
summary(PLM_model)$coef
#homo
u_hat = y- X %*%betaPLM
SSR = t(u_hat )%*% (u_hat )
sigmasqaureHat = SSR/ (n-1-1)
sigmasqaureHat = as.numeric(sigmasqaureHat)
VarCov_Homo = sigmasqaureHat * solve(t(X) %*% X) 
VarBeta_homo = diag(VarCov_Homo)
sdBeta_homo = sqrt(VarBeta_homo)
#compare  (not equal to each other)
sdBeta_homo # 0.174132125 
summary(PLM_model)$coef # 0.18785 


#hetero
u_hat = y- X %*%betaPLM
sqaured_residual = as.vector(u_hat^2)
Lambda_hat = diag(sqaured_residual)
VarCov_HR = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )
VarBeta_HR = diag(VarCov_HR)
sdBeta_HR = sqrt(VarBeta_HR) #0.187873428 

library("sandwich")
#compare Covariance Matrix (not equal to each other)
VarCov_HR  # non clustered Hetroskedasticity Robust Covariance Matrix
vcovHC(PLM_model, type = 'HC0') #Hetro  # clustered Hetroskedasticity Robust Covariance Matrix
vcovHC(PLM_model, type = 'HC0',cluster="group")
sqrt(0.08315617)

#compare HR (not equal to each other)
# 0.187873428 vs 0.28837 
library("lmtest")
sdBeta_HR
coeftest(PLM_model, vcov. = vcovHC(PLM_model, type = 'HC0',cluster="group"))
# close
sdBeta_HR
summary(PLM_model)$coef


#######################################################
# HR using the average mean of u^2 as 
MeanDegree <- function(x ){
   return(sum(x)/(length(x)-2))
}
head(df)
u_hat = y- X %*%betaPLM
sqaured_residual = as.vector(u_hat^2)
df$u_hat_squared = sqaured_residual
0.0412393
Mean_u_i = aggregate(df$u_hat_squared, by=list(df$state), FUN=mean)
colnames(Mean_u_i) <- c("state", "Mean_u_i")
head(Mean_u_i)
# df = subset(df, select = -c(Mean_u_i))
# left join the original Fatalities sets and Mean_Beertax_EF, Mean_FatalRate_EF respectively
df <- merge(x = df, y = Mean_u_i, by = "state", all.x = TRUE)

Lambda_hat = diag(df$Mean_u_i)                
VarCov_HR_Cluster = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )
VarBeta_HR_Cluster = diag(VarCov_HR_Cluster)
sdBeta_HR_Cluster = sqrt(VarBeta_HR_Cluster)

###################################################################
# HR Clustered sd using block diagonal Matrix
#define a utility function
fun_varcov_u <- function(x){
   return(  x %*% t(x) )
}
# examples of block diagonal Matrix
library(Matrix)
aaa =bdiag( matrix(1:4, 2, 2, byrow = TRUE), diag(3) )
bbb =diag(5)
aaa+bbb
# examples of block diagonal Matrix
aaa = matrix(1:6,2,3,byrow = TRUE)
aaa
row_list = asplit(aaa,1) # 2:split into column vectors 1: split into row vectors
u_list = lapply(row_list, fun_varcov_u) # lapply(): apply the function to each element in the list, return the new list 
bdiag( u_list ) #block diagonal Matrix
################################################################
fun_varcov_u <- function(x){
   return(  x %*% t(x) )
}
head(df)
#df = subset(df, select = -c(u_hat_squared)) # drop the unwanted column
#df = subset(df, select = -c(Mean_u_i)) # drop the unwanted column
u_hat = y- X %*%betaPLM
df$u_hat = u_hat
u_state = aggregate(df$u_hat, by=list(df$state), FUN=paste)
u_state = subset(u_state, select = -c(Group.1)) # drop the unwanted column of state name 
head(u_state)
u_state = apply(u_state, MARGIN =2 , FUN=as.numeric) # convert each entry from string charactor to numeric
u_state_list = asplit(u_state,1)  # split the rows of the matrix into list
head(u_state_list)
varcov_u_state_list = lapply(u_state_list, fun_varcov_u )
head(varcov_u_state_list)
lambda_hat = bdiag( varcov_u_state_list ) #construct the sparse block diagonal matrix
# 0.1153495801 -1.501240e-02  5.721168e-03  0.0680251476 -0.0840267207 -0.0897044325 -3.523459e-04
#lambda_hat = as.matrix(lambda_hat)
VarCov_HR_Cluster = solve( t(X) %*% X ) %*% t(X) %*% lambda_hat %*% X %*% solve( t(X) %*% X )

#intercept -3.568395e-21 1.355538e-18
#X          1.292916e-18 7.456081e-02
#intercept -2.224699e-21 1.908640e-18
#X          1.846317e-18 8.315617e-02
#
#solve( t(X) %*% X )
#intercept 2.976190e-03 3.638996e-18
#X         3.638996e-18 9.789445e-01
#t(X) %*% lambda_hat %*% X
#intercept 1.965295e-16 3.550123e-16
#X         3.690353e-16 7.780267e-02
#t(X) %*% lambda_hat %*% X
#intercept 1.965295e-16 5.306765e-16
#X         5.416403e-16 8.677175e-02
VarBeta_HR_Cluster = diag(VarCov_HR_Cluster) # 8.315617e-02
sdBeta_HR_Cluster = sqrt(VarBeta_HR_Cluster) #0.2730583
sdBeta_HR_Cluster
coeftest(PLM_model, vcov = vcovHC(PLM_model,cluster="group")) #0.28837 

varcov_u_state_list = apply(u_state, MARGIN = 1 ,fun_varcov_u )
class(u_state[1,1])










colnames(Mean_u_i) <- c("state", "Mean_u_i")
head(Mean_u_i)
# df = subset(df, select = -c(Mean_u_i))
# left join the original Fatalities sets and Mean_Beertax_EF, Mean_FatalRate_EF respectively
df <- merge(x = df, y = Mean_u_i, by = "state", all.x = TRUE)

Lambda_hat = diag(df$Mean_u_i)                
VarCov_HR_Cluster = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )
VarBeta_HR_Cluster = diag(VarCov_HR_Cluster)
sdBeta_HR_Cluster = sqrt(VarBeta_HR_Cluster)



# Run OLS with fatal_rate_E_demeaned onto beertax_E_demeaned
Panel_Data_Reg_Model = lm(fatal_rate_E_demeaned ~ beertax_E_demeaned, data = Fatalities)
summary(Panel_Data_Reg_Model)
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "const")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC0")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")




###########################################################################################
## OLS
###########################################################################################
OLS_model <- lm(fatal_rate~beertax, data = Fatalities)
summary(OLS_model) 
coeftest(OLS_model, vcov. = vcovHC, type = "const")
coeftest(OLS_model, vcov. = vcovHC, type = "HC0") # hetero
coeftest(OLS_model, vcov. = vcovHC, type = "HC1")

#################################################
# Visualization
#################################################
par(mgp=c(1 ,0.1,0),mar=c(2.5,2,2,2)+0.3)
plot( Fatalities$beertax , Fatalities$fatal_rate,
      xaxt = "n",yaxt = "n",
      xlab ="",
      ylab = "",
      cex =0.5,
      ylim = c(0, 4.5),
      pch = 1, 
      col = "steelblue")
axis(1, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
axis(2, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
title(xlab = "Beer tax",  cex.lab = 1)
title(ylab = "Fatality rate", cex.lab = 1)
title(main = "Traffic Fatality Rates and Beer Taxes", cex.main = 1)
abline(OLS_model, lwd = 1.5, col = 'red' )



###########################################################################################
## Panel Data Regression
###########################################################################################

####################################################################
# 1. Entity Fixed Effect Regression
####################################################################

# Calculate the mean across the time for each state
Mean_Beertax_EF = aggregate(Fatalities$beertax, by=list(Fatalities$state), FUN=mean)
Mean_FatalRate_EF = aggregate(Fatalities$fatal_rate, by=list(Fatalities$state), FUN=mean)
head(Mean_Beertax_EF)
head(Mean_FatalRate_EF)
head(Fatalities)
# Change the column name of the above sample means
colnames(Mean_Beertax_EF) <- c("state", "beertax_Mean_i")
colnames(Mean_FatalRate_EF) <- c("state", "fatal_rate_Mean_i")

# left join the original Fatalities sets and Mean_Beertax_EF, Mean_FatalRate_EF respectively
Fatalities$beertax_E_demeaned <- with( merge(x = Fatalities, y = Mean_Beertax_EF, by = "state", all.x = TRUE), 
                                       beertax - beertax_Mean_i)
Fatalities$fatal_rate_E_demeaned <- with( merge(x = Fatalities, y = Mean_FatalRate_EF, by = "state", all.x = TRUE), 
                                          fatal_rate - fatal_rate_Mean_i )

head(Fatalities)

# Run OLS with fatal_rate_E_demeaned onto beertax_E_demeaned
Panel_Data_Reg_Model = lm(fatal_rate_E_demeaned ~ beertax_E_demeaned, data = Fatalities)
summary(Panel_Data_Reg_Model)
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "const")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC0")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")

#################################################
# Software Package
#################################################
#install.packages("plm")
library(plm)
Software_Panel_Data_Reg_Model <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")


Panel_Data_Reg_Model$coefficients
Software_Panel_Data_Reg_Model$coefficients


####################################################################
# 2.Time Fixed Effect Regression
####################################################################

# Calculate the mean across the entity(state) for each time period
Mean_Beertax_TF = aggregate(Fatalities$beertax, by=list(Fatalities$year), FUN=mean)
Mean_FatalRate_TF = aggregate(Fatalities$fatal_rate, by=list(Fatalities$year), FUN=mean)
head(Mean_Beertax_TF)
head(Mean_Beertax_TF)

# Change the column name of the above sample means
colnames(Mean_Beertax_TF) <- c("year", "beertax_Mean_t")
colnames(Mean_FatalRate_TF) <- c("year", "fatal_rate_Mean_t")

# left join the original Fatalities sets and Mean_Beertax_EF, Mean_FatalRate_EF respectively
Fatalities$beertax_T_demeaned <- with( merge(x = Fatalities, y = Mean_Beertax_TF, by = "year", all.x = TRUE), 
                                       beertax - beertax_Mean_t)
Fatalities$fatal_rate_T_demeaned <- with( merge(x = Fatalities, y = Mean_FatalRate_TF, by = "year", all.x = TRUE), 
                                          fatal_rate - fatal_rate_Mean_t )

head(Fatalities)

# Run OLS with fatal_rate_T_demeaned onto beertax_T_demeaned
Panel_Data_Reg_Model = lm(fatal_rate_T_demeaned ~ beertax_T_demeaned, data = Fatalities)
summary(Panel_Data_Reg_Model)
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "const")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC0")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")

#################################################
# Software Package
#################################################
#install.packages("plm")
library(plm)
Software_Panel_Data_Reg_Model <- plm(fatal_rate ~ beertax, 
                                     data = Fatalities,
                                     index = c("state", "year"), 
                                     effect = "time",
                                     model = "within")
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")


Panel_Data_Reg_Model$coefficients
Software_Panel_Data_Reg_Model$coefficients

####################################################################
# 3. Mixed Entity and Time Fixed Effect Regression
####################################################################

# The code is proceeded with the following step
       # : First Entity-Demean then Time-Demean


# Since Entity-Demean is already done by section 1, 
# we are doing the Time-Demean upon the 'Entity-Demeaned' data.

head(Fatalities)
# Calculate the mean across the entity(state) for each time period
Mean_Beertax_ETF = aggregate(Fatalities$beertax_E_demeaned, by=list(Fatalities$year), FUN=mean)
Mean_FatalRate_ETF = aggregate(Fatalities$fatal_rate_E_demeaned, by=list(Fatalities$year), FUN=mean)
head(Mean_Beertax_ETF)
head(Mean_Beertax_ETF)

# Change the column name of the above sample means
colnames(Mean_Beertax_ETF) <- c("year", "beertax_Mean_it")
colnames(Mean_FatalRate_ETF) <- c("year", "fatal_rate_Mean_it")

# left join the original Fatalities sets and Mean_Beertax_EF, Mean_FatalRate_EF respectively
Fatalities$beertax_ET_demeaned <- with( merge(x = Fatalities, y = Mean_Beertax_ETF, by = "year", all.x = TRUE), 
                                        beertax_E_demeaned - beertax_Mean_it)
Fatalities$fatal_rate_ET_demeaned <- with( merge(x = Fatalities, y = Mean_FatalRate_ETF, by = "year", all.x = TRUE), 
                                           fatal_rate_E_demeaned - fatal_rate_Mean_it )

head(Fatalities)

# Run OLS with fatal_rate_E_demeaned onto beertax_E_demeaned
Panel_Data_Reg_Model = lm(fatal_rate_ET_demeaned ~ beertax_ET_demeaned, data = Fatalities)
summary(Panel_Data_Reg_Model)
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "const")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC0")
coeftest(Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")

#################################################
# Software Package
#################################################
#install.packages("plm")
library(plm)
Software_Panel_Data_Reg_Model <- plm(fatal_rate ~ beertax, 
                                     data = Fatalities,
                                     index = c("state", "year"), 
                                     effect = "twoways",
                                     model = "within")
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")


Panel_Data_Reg_Model$coefficients
Software_Panel_Data_Reg_Model$coefficients



##################################################################################
# Appendix
# Merge Two Data Set: inner join, outer join, left join, right join
##################################################################################
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))
df1
df2
#Inner Join
merge(df1, df2)
#Outer Join
merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
#Left Join
merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
#Right Join
merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
