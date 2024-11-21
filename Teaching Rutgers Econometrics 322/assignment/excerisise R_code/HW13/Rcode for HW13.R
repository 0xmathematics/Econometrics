## Rcode for HW13
## Hang Miao

##############################################################################
#Excercise 15.7
############################################################################

#######################################
# Expectation of AR(1)
#######################################

Intercept = 2.7
beta1 = 0.7
E_Y =  + Intercept/(1-beta1)
round(E_Y, digits = 2)

Var_u = 9
Var_Y = Var_u/(1-beta1^2)
round(Var_Y, digits = 3)

#######################################
# Autocovariance of AR(1)
#######################################

Cov_Y_lag1 =  beta1 * Var_Y
round(Cov_Y_lag1, digits = 3)

Cov_Y_lag2 =  beta1^2*Var_Y
round(Cov_Y_lag2, digits = 3)

#######################################
# Autocorrelations of AR(1)
#######################################

Corr_Y_lag1 =  Cov_Y_lag1/sqrt(Var_Y*Var_Y  )
round(Corr_Y_lag1, digits = 3)

Corr_Y_lag2 =  Cov_Y_lag2/sqrt(Var_Y*Var_Y  )
round(Corr_Y_lag2, digits = 3)

#######################################
# Forecast One Step Ahead
#######################################

Y_curr = 102.8
E_Y_next = Intercept + beta1*Y_curr
round(E_Y_next, digits = 2)


##############################################################################
#Excercise 15.3
############################################################################
#######################################
# ADF
#######################################

delta_hat = -0.0085
sd_delta = 0.0044
delta_null = 0
t_stat =  (delta_hat-delta_null)/sd_delta
round(t_stat, digits = 2)

Var_u = 9
Var_Y = Var_u/(1-beta1^2)
round(Var_Y, digits = 3) 







