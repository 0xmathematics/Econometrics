## Rcode for HW10
## Hang Miao

##############################################################################
#Test A Ex 10.5.2
############################################################################

# critical value of 5% significant level, i.e 95% quantile value for F(q, infnity)
# 1982 - 1988, 7 years, 7-1 binary variable for time fixed effect.
# 1 intercept.
# we want to jointly test 7-1 binary variable and the intercept together, so there is 7-1+1 constraint
d = 7-1+1
# when dg2 goes to infinity, F distribution F(dg1, dg2) times dg1 i.e F(dg1, dg2)*dg1
# converge to chi sqaure distribution with degree 1 i.e chi_square(dg1)
qchisq(0.95, d)/d

##############################################################################
#Excercise 10.1
##############################################################################
Total_Population = 6.7*10^2 # measured in ten thousand
#change of the fatility rate after increasing the tax rate by 1 unit
beta_BeerTax =  0.46

round(Total_Population*beta_BeerTax,digits = 2)

#######################################
# 95 CI
#######################################
sd = 0.36
q975 = qnorm(0.975, mean = 0, sd = 1)
# 95 CI for beta_BeerTax
left = beta_BeerTax - sd*q975
right = beta_BeerTax + sd*q975
CI95 = c(left, right)
round(Total_Population*CI95, digits=2)


#######################################
# Reduce the legal Drinking Age
#######################################
beta_18 = 0.032
round(Total_Population*beta_18,digits = 2)
#######################################
# 90 CI
#######################################
sd = 0.073
q95 = qnorm(0.95, mean = 0, sd = 1)
# 95 CI for beta_BeerTax
left = beta_18 - sd*q95
right = beta_18 + sd*q95
CI90 = c(left, right)
round(Total_Population*CI90, digits=2)

#######################################
# Increse the Real Income Per Capita
#######################################
beta_Income = 1.79
round(Total_Population*beta_Income*0.01,digits = 2)
#######################################
# 90 CI
#######################################
sd = 0.65
q95 = qnorm(0.95, mean = 0, sd = 1)
# 95 CI for beta_BeerTax
left = beta_Income - sd*q95
right = beta_Income + sd*q95
CI90 = c(left, right)
round(Total_Population*CI90*0.01, digits=2)




