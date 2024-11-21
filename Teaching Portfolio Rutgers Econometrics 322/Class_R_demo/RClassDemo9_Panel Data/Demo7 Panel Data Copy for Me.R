######################################################################################
# Class demo 7: Panel Data Regression
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
## Approach2: Loading data by Reading the data file CASchoolsData.csv under the same folder
###########################################################################################
Fatalities <- read.csv("FatalitiesData.csv", header=TRUE, sep=",")
head(Fatalities)

# Check if there is any missing data
is.data.frame(Fatalities) 

# Create the dependent variable fatal rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# Select the columns which will be used in our model
Fatalities <- Fatalities[ ,c('state', 'year', 'fatal_rate', 'beertax') ]
head(Fatalities)

###########################################################################################
## OLS
###########################################################################################
OLS_model <- lm(fatal_rate~beertax, data = Fatalities)
summary(OLS_model) 
coeftest(OLS_model, vcov. = vcovHC, type = "const")
coeftest(OLS_model, vcov. = vcovHC, type = "HC0")
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
# 3.Fixed Entity and Time Effect Regression
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
abline(Panel_Data_Reg_Model, lwd = 1.5, col = 'red' )


##################################################################################
# Digression: Merge Two Data Set: inner join, outer join, left join, right join
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
