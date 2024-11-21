## Rcode for HW12
## Hang Miao

##############################################################################
#Excercise 12.1
############################################################################

#######################################
# Demand Change
#######################################

Current_Price = 7.5
Price_Change = 0.5
Percent_Change_Price = Price_Change/Current_Price*100
# log difference is an approximation of the above percent change
log_diff = (log(Current_Price+Price_Change)-log(Current_Price) )*100
beta_p = -0.94

# approch 1: correct answer but !!NOT!! accepted by myeconlab 
round(Percent_Change_Price*beta_p, digits = 2)
# approch 2: correct answer and also accepted by myeconlab 
round(log_diff*beta_p, digits = 2)
#######################################
# 95 CI
#######################################
sd = 0.21
q975 = qnorm(0.975, mean = 0, sd = 1)
# 95 CI for beta_BeerTax
left = beta_p - sd*q975
right = beta_p + sd*q975
CI95 = c(left, right)
# approch 1: correct answer but !!NOT!! accepted by myeconlab 
round(CI95*Percent_Change_Price, digits=2)
# approch 2: correct answer and also accepted by myeconlab 
round(CI95*log_diff, digits=2)




