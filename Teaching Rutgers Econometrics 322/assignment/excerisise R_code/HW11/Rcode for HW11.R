## Rcode for HW11
## Hang Miao

##############################################################################
# Test A Ex 11.2.2
############################################################################

# Probit Model

# coefficients
beta_intercept = -2.26
beta_PI = 2.74
beta_black = 0.71

#Original pi
x_pi0 = 0.3
#current pi
x_pi1 = 0.4
#Binary regressor Black
x_black = 1

#activation function: use cdf of normal as activation function
# pnorm() is the cdf for normal in R

# Original Probability
py_0 = pnorm(beta_intercept + beta_PI*x_pi0  + beta_black*x_black)

# Current Probability
py_1 = pnorm(beta_intercept + beta_PI*x_pi1  + beta_black*x_black)

round((py_1-py_0)*100, digits = 2)



##############################################################################
# Exercise 11.1
############################################################################

#####################
# Probit Model
#####################

beta_exp = 0.031
sd_beta_exp = 0.005

# significant test
z = (beta_exp-0)/sd_beta_exp

p_value = pnorm(z, mean = 0, sd = 1,lower.tail = FALSE)
round(p_value ,digits = 4)

# Prediction for Matthew 
beta_intercept = 0.713
x_exp = 13
# Probability
py_0 = pnorm(beta_intercept + beta_exp*x_exp )
round(py_0, digits = 3)

# Prediction for Christopher 
x_exp = 0
# Probability
py_0 = pnorm(beta_intercept + beta_exp*x_exp )
round(py_0, digits = 3)

# Prediction for Jed 
x_exp = 95-15
# Probability
py_0 = pnorm(beta_intercept + beta_exp*x_exp )
round(py_0, digits = 3)

##############################################################################
# Exercise 11.2
############################################################################
#####################
# Logistic Model        plogis() function in R
#####################

beta_exp = 0.047
sd_beta_exp = 0.011

# significant test
z = (beta_exp-0)/sd_beta_exp

p_value = pnorm(z, mean = 0, sd = 1,lower.tail = FALSE)
round(p_value ,digits = 4)

# Prediction for John  
beta_intercept = 1.053
x_exp = 13
# Probability
py_0 = plogis((beta_intercept + beta_exp*x_exp ), lower.tail = TRUE)
round(py_0, digits = 3)

# Prediction for Katherine   
x_exp = 0
# Probability
py_0 = plogis((beta_intercept + beta_exp*x_exp ), lower.tail = TRUE)
round(py_0, digits = 3)








