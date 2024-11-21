######################################################################################
# Demo 10: Instrumental Variable Regression
######################################################################################
##############################
## Econometrics 322 Hang Miao
##############################

#########################################################################################
## Approach1: Loading data by Using the third party package: Applied Econometrics with R
#########################################################################################
library(AER)
data("CigarettesSW")
head(CigarettesSW)

# output the data
# write.csv(CigarettesSW,"CigarettesSWData.csv", row.names = FALSE)

###########################################################################################
## Approach2: Loading data by Reading the data file CigarettesSW.csv under the same folder
###########################################################################################
CigarettesSW <- read.csv("CigarettesSWData.csv", header=TRUE, sep=",")
head(CigarettesSW)



#########################################
## Data Preprocessing
#########################################
# Check if there is any missing data
is.data.frame(CigarettesSW) 


# Create New Dependent Variable: Real Per Capita Prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

# Create New Instrument Variable: Sales Tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

# Create New Instrument Variable: Real Income Per Capita
CigarettesSW$rincome <- with(CigarettesSW, income/(population*cpi) )



# Select the year which will be used in our model
Sig_data <- subset(CigarettesSW, year == "1985")
# Select the columns which will be used in our model
Sig_data <- Sig_data[ ,c('state', 'packs', 'rprice', 'salestax', 'rincome') ]
head(Sig_data)
###########################################################################################
## OLS
###########################################################################################
OLS_model <- lm(log(packs)~log(rprice), data = Sig_data)
summary(OLS_model) 
coeftest(OLS_model, vcov. = vcovHC, type = "const")
coeftest(OLS_model, vcov. = vcovHC, type = "HC0")
coeftest(OLS_model, vcov. = vcovHC, type = "HC1")

#################################################
# Visualization
#################################################
par(mgp=c(1 ,0.2,1),mar=c(2.5,2,2,2)+0.3)
plot(  log(Sig_data$rprice),  log(Sig_data$packs) ,
      #log="y",
      xaxt = "n",yaxt = "n",
      xlab ="",
      ylab = "",
      cex =0.5,
      #ylim = c(0, 1),
      pch = 1, 
      col = "steelblue")
axis(1, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
axis(2, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
title(xlab = "Quantity",  cex.lab = 1)
title(ylab = "Price", cex.lab = 1)
title(main = "Cigarettes Market Equilibrium", cex.main = 1)
abline(OLS_model, lwd = 1.5, col = 'red' )



###########################################################################################
## Instrumental Variable Regression (TSLS)
###########################################################################################

####################################################################
# 1. Estimate the Supply Curve
####################################################################
head(Sig_data)
# First Stage
LM_SupCur_1 = lm( log(rprice) ~ log(rincome), data = Sig_data)
coeftest(LM_SupCur_1, vcov. = vcovHC, type = "const")
Sig_data$log_rprice_Hat = LM_SupCur_1$fitted.values

# Second Stage
LM_SupCur_2 = lm(log(packs) ~ log_rprice_Hat , data = Sig_data)
coeftest(LM_SupCur_2, vcov. = vcovHC, type = "const")


#################################################
# Visualization
#################################################
par(mgp=c(1 ,0.2,1),mar=c(2.5,2,2,2)+0.3)
plot(  log(Sig_data$rprice),  log(Sig_data$packs) ,
       #log="y",
       xaxt = "n",yaxt = "n",
       xlab ="",
       ylab = "",
       cex =0.5,
       #ylim = c(0, 1),
       pch = 1, 
       col = "steelblue")
axis(1, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
axis(2, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
title(xlab = "Quantity",  cex.lab = 1)
title(ylab = "Price", cex.lab = 1)
title(main = "Cigarettes Market Equilibrium", cex.main = 1)
abline(OLS_model, lwd = 1.5, col = 'red' )
abline(LM_SupCur_2, lwd = 1.5, col = 'blue' )


#################################################
# Software Package
#################################################

IV_SupCur <- ivreg(log(packs) ~ log(rprice) | log(rincome), data = Sig_data)
coeftest(IV_SupCur, vcov. = vcovHC, type = "const")

IV_SupCur
LM_SupCur_2



####################################################################
# 2. Estimate the Demand Curve
####################################################################

head(Sig_data)
# First Stage
LM_DemCur_1 = lm( log(rprice) ~ salestax, data = Sig_data)
coeftest(LM_DemCur_1, vcov. = vcovHC, type = "const")
Sig_data$log_rprice_Hat = LM_DemCur_1$fitted.values

# Second Stage
LM_DemCur_2 = lm(log(packs) ~ log_rprice_Hat , data = Sig_data)
coeftest(LM_DemCur_2, vcov. = vcovHC, type = "const")

#################################################
# Visualization
#################################################
par(mgp=c(1 ,0.2,1),mar=c(2.5,2,2,2)+0.3)
plot(  log(Sig_data$rprice),  log(Sig_data$packs) ,
       #log="y",
       xaxt = "n",yaxt = "n",
       xlab ="",
       ylab = "",
       cex =0.5,
       #ylim = c(0, 1),
       pch = 1, 
       col = "steelblue")
axis(1, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
axis(2, tck = 0.03, lwd = 0.5, cex.axis=0.5, line = 0 )
title(xlab = "Quantity",  cex.lab = 1)
title(ylab = "Price", cex.lab = 1)
title(main = "Cigarettes Market Equilibrium", cex.main = 1)
abline(OLS_model, lwd = 1.5, col = 'red' )
abline(LM_SupCur_2, lwd = 1.5, col = 'blue' )
abline(LM_DemCur_2, lwd = 1.5, col = 'green' )

#################################################
# Software Package
#################################################
IV_DemCur <- ivreg(log(packs) ~ log(rprice) | salestax, data = Sig_data)
coeftest(IV_DemCur, vcov. = vcovHC, type = "const")

IV_DemCur
LM_DemCur_2