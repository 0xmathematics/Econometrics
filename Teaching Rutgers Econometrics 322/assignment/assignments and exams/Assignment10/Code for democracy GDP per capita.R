

#############################################################
### Assignment10: Panel data analysis democracy
#############################################################

#########################################################################################
## Approach1: Loading data by Using the third party package: Applied Econometrics with R
#########################################################################################
#install.packages("lmtest") # for coefficient test
library("lmtest")
library(plm)
library(AER) 
library('readxl')
dtibble <- read_excel("income_democracy.xlsx")
df = data.frame(dtibble)
head(df)
nrow(df)

#############################
colSums(is.na(df))


clean_df = na.omit(df)
nrow(clean_df)

# write.csv(df,"GDP_Demo_original.csv", row.names = FALSE)
# write.csv(clean_df,"GDP_Demo_cleaned.csv", row.names = FALSE)

###########################################################################################
## Approach2: Loading data by Reading the data file CASchoolsData.csv under the same folder
###########################################################################################
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment10")

df <- read.csv("GDP_Demo_cleaned.csv")
head(df)

df_0 <- read.csv("GDP_Demo_original.csv")
head(df_0)

# Q(a).i
df_demID = na.omit(df_0$dem_ind)
summary(df_demID)
mean(df_demID)
sd(df_demID)

# Q(b).ii
mean( df_0[which( df_0$country =='United States'),]$dem_ind )
# Q(b).iii
mean( df_0[which( df_0$country =='Libya'),]$dem_ind )


##############################
## OLS Linear Regression
##############################
is.data.frame(df) 
colnames(df)
# Q(c).i
#Sigle variate lm
OLS_model_1 <- lm(dem_ind ~ log_gdppc, data = df_0)
summary(OLS_model_1)
#OLS_model_1 <- lm(dem_ind ~ log_gdppc, data = df_0)
coeftest(OLS_model_1, vcov = vcovHC,type = "const")
coeftest(OLS_model_1, vcov. = vcovHC,type = "HC0")
coeftest(OLS_model_1, vcov = vcovHC(OLS_model_1, cluster="group",type = "HC0") )
confint(OLS_model_1, vcov = vcovHC(OLS_model_1, cluster="group",type = "HC0"), level = 0.95)

# Q(c).ii
OLS_model_1$coefficients[2]*0.2  #%

# Q(c).iii
# unclustered
coeftest(OLS_model_1, vcov. = vcovHC,type = "HC0") # unclustered

# clustered
u_hat = OLS_model_1$residuals
head(df_0)
df_nonNa = df_0[!is.na(df_0$dem_ind) & !is.na(df_0$log_gdppc), c('country', 'year','dem_ind','log_gdppc') ]
nrow(df_nonNa) == length(u_hat)
df_nonNa$u_hat = u_hat


################################################################

# run this first 
df_nonNa$u_hat_squared = u_hat^2
Mean_u_i = aggregate(df_nonNa$u_hat_squared, by=list(df_nonNa$country), FUN=mean)
colnames(Mean_u_i) <- c("country", "Mean_u_i")
df_nonNa <- merge(x = df_nonNa, y = Mean_u_i, by = "country", all.x = TRUE)
head(df_nonNa)
#################

Lambda_hat = diag(df_nonNa$Mean_u_i)                
VarCov_HR_Cluster = solve( t(X) %*% X ) %*% t(X) %*% Lambda_hat %*% X %*% solve( t(X) %*% X )
VarBeta_HR_Cluster = diag(VarCov_HR_Cluster)
sdBeta_HR_Cluster = sqrt(VarBeta_HR_Cluster)
###################################################################
fun_varcov_u <- function(x){
  x = sapply(x, FUN=as.numeric)
  return(  x %*% t(x) )
}
head(df_nonNa)
#df = subset(df, select = -c(u_hat_squared)) # drop the unwanted column
#df_nonNa = subset(df_nonNa, select = -c(Mean_u_i)) # drop the unwanted column

u_country = aggregate(df_nonNa$u_hat, by=list(df_nonNa$country ), FUN=paste)
u_country = subset(u_country, select = -c(Group.1)) # drop the unwanted column of state name 
head(u_country)
class(u_country)
head(u_country)
#u_country = apply(u_country, MARGIN =1 , FUN=as.numeric) # convert each entry from string charactor to numeric
u_country_list = asplit(u_country,1)  # split the rows of the matrix into list
head(u_country_list)
varcov_u_country_list = lapply(u_country_list, fun_varcov_u )
head(varcov_u_country_list)
lambda_hat = bdiag( varcov_u_country_list )

head(df_nonNa)
y = df_nonNa$dem_ind;  
n = nrow(df_nonNa)
X = df_nonNa$log_gdppc; intercept = rep(1,nrow(df_nonNa)); X = cbind(intercept,X)
X = as.matrix(X); y = as.matrix(y)

VarCov_HR_Cluster = solve( t(X) %*% X ) %*% t(X) %*% lambda_hat %*% X %*% solve( t(X) %*% X )
VarBeta_HR_Cluster = diag(VarCov_HR_Cluster) # 8.315617e-02
sdBeta_HR_Cluster = sqrt(VarBeta_HR_Cluster) #0.2730583
sdBeta_HR_Cluster


# Q(d).i
# histories, institutions, social structures, and religions.

model_2_FT <- plm(dem_ind ~ log_gdppc, data=df_0, model="within",effect = 'time', index=c("country", "year")) 
coeftest(model_2_FT, vcov = vcovHC(model_2_FT, cluster="group",type = "HC0") )
coeftest(model_2_FT, vcov. = vcovHC,type = "HC0")
# Q(d).ii
model_3_FE <- plm(dem_ind ~ log_gdppc, data=df_0, model="within", index=c("country", "year")) 
coeftest(model_3_FE, vcov = vcovHC(model_2_FE, cluster="group",type = "HC0") )
coeftest(model_3_FE, vcov. = vcovHC,type = "HC0")
# Q(d).iii delete Azerbaijian, no effect for FE estimation
head(df_0)
df_no_Azerbaijian = df_0[-which(df_0$country =="Azerbaijan"),]
model_3_FE <- plm(dem_ind ~ log_gdppc, data=df_no_Azerbaijian, model="within", index=c("country", "year")) 
coeftest(model_3_FE, vcov = vcovHC(model_2_FE, cluster="group",type = "HC0") )
coeftest(model_3_FE, vcov. = vcovHC,type = "HC0")
# Q(d).iv 
# The demand for democracy is contagious and sweeps across countries (remember
# the "Arab Spring" of 2012).

# Q(d).v FT estimation
model_4_FET <- plm(dem_ind ~ log_gdppc, data=df_0, model="within",effect = 'twoway', index=c("country", "year")) 
coeftest(model_4_FET, vcov = vcovHC(model_4_FET, cluster="group",type = "HC0") )
coeftest(model_4_FET, vcov. = vcovHC,type = "HC0")

# Q(d).vi FT estimation
head(df_0)
model_5_FET_control <- plm(dem_ind ~ log_gdppc + log_gdppc + log_pop + age_1 + age_2 + age_3 + age_4 + age_5 + educ + age_median +code, data=df_0, model="within",effect = 'twoway', index=c("country", "year")) 
coeftest(model_5_FET_control, vcov = vcovHC(model_5_FET_control, cluster="group",type = "HC0") )
coeftest(model_5_FET_control, vcov. = vcovHC,type = "HC0")

Q = model_5_FET_control$coefficients
round(Q,digits = 3)


summary(OLS_model_1) 
summary(df_0$dem_ind) 
#Q1
Q1 = OLS_model_1$coefficients
round(Q1,digits = 3)
#Multi variate lm
OLS_model_2 <- lm(log(violent) ~ law+prisoners+density+income+population+black+white+male, data = df)
summary(OLS_model_2) 
#Q2
Q2 = OLS_model_2$coefficients
round(Q2,digits = 3)
#Q3
coeftest(OLS_model_2, vcov. = vcovHC, type = "const")
#Q4
coeftest(OLS_model_2, vcov. = vcovHC,type = "HC0")

#Q5 multiple choice

#Q6 multiple choice
####################################################################
# 1. Entity Fixed Effect Regression
####################################################################
library(plm)
Software_Panel_Data_Reg_Model <- plm(log(violent) ~ law+prisoners+density+income+population+black+white+male , 
                                     data = df,
                                     index = c("state", "year"), 
                                     model = "within")
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC0")
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")

#Q7 
Q7 = Software_Panel_Data_Reg_Model$coefficients
round(Q7,digits = 3)

####################################################################
# 2. Mixed Entity and Time Fixed Effect Regression
####################################################################
library(plm)

Software_Panel_Data_Reg_Model <- plm(log(violent) ~ law+prisoners+density+income+population+black+white+male , 
                                     data = df,
                                     index = c("state", "year"), 
                                     effect = "twoways",
                                     model = "within")
summary(Software_Panel_Data_Reg_Model)
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC0")
coeftest(Software_Panel_Data_Reg_Model, vcov. = vcovHC, type = "HC1")

#Q9
Q9 = Software_Panel_Data_Reg_Model$coefficients
round(Q9,digits = 3)

