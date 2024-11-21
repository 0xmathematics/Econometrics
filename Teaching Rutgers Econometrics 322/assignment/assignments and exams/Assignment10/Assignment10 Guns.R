

#############################################################
### Assignment10: Panel data analysis
#############################################################

#########################################################################################
## Cleaning the Data Applied Econometrics with R
#########################################################################################
#install.packages("lmtest") # for coefficient test
library("lmtest")
library(AER)  
data("Guns") # Use data
df = Guns
head(df)
colnames(df)
colnames(df)[which(names(df) == "afam")] = 'black'
colnames(df)[which(names(df) == "cauc")] = 'white'
colnames(df) 


#create an empty columns
df['numerica_Law'] = numeric(nrow(df))
#fill in the binary variables into this empty columns
df$numerica_Law[which(df$law == "no")] = 0
df$numerica_Law[which(df$law == "yes")] = 1
#delete the original law row
colnames(df) 
df <- subset(df, select = -c(law))
#change the row name back to law
colnames(df) 
colnames(df)[which(names(df) == "numerica_Law")] = 'law'
# write.csv(df,"Guns.csv", row.names = FALSE)

###########################################################################################
## Loading data 
###########################################################################################
#mac
setwd("~/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment10")

#pc
setwd("C:/Users/Hahn/Dropbox/Econometrics  R Emprical Analysis/assignment/Assignment10")
df <- read.csv("Guns.csv")
head(df)



##############################
## OLS Linear Regression
##############################
is.data.frame(df) 
colnames(df)

#Sigle variate lm
OLS_model_1 <- lm(log(violent) ~ law, data = df)
summary(OLS_model_1) 
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
library(lmtest)
library(sandwich)
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

