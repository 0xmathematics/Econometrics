

#############################################################
### Assignment10: Panel data analysis democracy
#############################################################

#########################################################################################
## Approach1: Loading data from excel
#########################################################################################
#install.packages("lmtest") # for coefficient test
library("lmtest")
library(plm)
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
## Approach2: Loading data by Reading the data csv
###########################################################################################
#mac
setwd("~/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment10")
#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment10")

df_0 <- read.csv("GDP_Demo_original.csv")
head(df_0)
is.data.frame(df_0) 
colnames(df_0)


# Q1
##############################
## OLS Linear Regression
##############################

# OLS Estimation
OLS_model_1 <- lm(dem_ind ~ log_gdppc, data = df_0)
# Q2 Coefficient
Q2 = OLS_model_1$coefficients
round(Q2, digits = 3) #beta: -1.355  0.236
# Q3 Inference  
#homo
summary(OLS_model_1)$coef  # sd: 0.0709187 0.0086258
coeftest(OLS_model_1, vcov = vcovHC,type = "const") #sd: 0.0709187 0.0086258
#hetero
coeftest(OLS_model_1, vcov = vcovHC,type = "HC0") #sd: 0.0606914 0.0070965
#Clustered hetero
PoolOLS_model_1 <- plm(dem_ind ~ log_gdppc, data=df_0, model="pool", index=c("country", "year")) 
coeftest(PoolOLS_model_1, vcov = vcovHC(PoolOLS_model_1, cluster="group",type = "HC0") ) #0.100034 0.011791

# Q4 multiple CHoise
# Q5 Huntington's view
# Q6 entity fixed effect
model_2_FE <- plm(dem_ind ~ log_gdppc, data=df_0, model="within", index=c("country", "year")) 
summary(model_2_FE)$coef  
Q6 = model_2_FE$coefficients
round(Q6, digits = 3) #0.084 
coeftest(model_2_FE, vcov = vcovHC(model_2_FE, cluster="group",type = "HC0") ) #sd 2.6666 
coeftest(model_2_FE, vcov. = vcovHC,type = "HC0") #sd 2.6666 
# Q7
# Q8
# Q9 Estimation
# mixed entity and time fixed effect
model_3_mixed_FET <- plm(dem_ind ~ log_gdppc, data=df_0, model="within",effect = "twoway", index=c("country", "year")) 
Q9 = model_3_mixed_FET$coefficients
round(Q9, digits = 3) #0.084 

# Q10 Inference
summary(model_3_mixed_FET)$coef  # pvalue 0.05383046
coeftest(model_3_mixed_FET, vcov = vcovHC(model_3_mixed_FET, cluster="group",type = "HC0") )
coeftest(model_3_mixed_FET, vcov. = vcovHC,type = "HC0") # pvalue 0.2033

# Q11 Estimation
head(df_0)
# control variable
model_4_FET_control <- plm(dem_ind ~ log_gdppc + log_gdppc + log_pop + age_1 + age_2 + age_3 + age_4 + age_5 + educ + age_median, data=df_0, model="within",effect = 'twoway', index=c("country", "year")) 
Q11 = model_4_FET_control$coefficients
round(Q11,digits = 3)
log_gdppc     log_pop       age_1       age_2       age_3       age_4       age_5 
0.028      -0.077 -198147.100 -198147.733 -198150.011 -198147.157 -198146.949 
educ  age_median 
0.000       0.006 



# Q12 Inference
summary(model_4_FET_control)$coef  # log_gdppc pvalue 0.5223576
coeftest(model_4_FET_control, vcov = vcovHC(model_4_FET_control, cluster="group",type = "HC0") )
coeftest(model_4_FET_control, vcov. = vcovHC,type = "HC0") #log_gdppc pvalue 0.5907




