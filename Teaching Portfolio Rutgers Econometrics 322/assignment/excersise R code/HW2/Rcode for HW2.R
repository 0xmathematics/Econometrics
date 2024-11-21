#R code for HW2
mu=-2
sd = 2

q1 = pnorm( -3 ,mu, sd )
round(q1,digits = 4)

dnorm(0)

#######################################
## Question 5
#######################################
# convert mean measured in Fahrenheit to which measured Celsius
mu_F = 47
sd_F = 5

mu_C = 5/9*(mu_F-32)
round(mu_C, digits = 3)

# convert sd measured in Fahrenheit to which measured Celsius
Var_F = sd^2
Var_C = (5/9)^2 * Var_F
sd_C = sqrt(Var_C)
round(sd_C, digits = 3)
# convert Var measured in Fahrenheit to which measured Celsius
# use properties of variance
round(Var_C, digits = 3)
#install packages for reading excel data file. Only need to install on your computer once.

#######################################
## Question 14
#######################################

E_X = 0*0.73 + 1*0.27
E_Y = 0*0.61 + 1*0.39
Var_X = (0-E_X)^2*0.73 + (1-E_X)^2*0.27
Var_Y = (0-E_Y)^2*0.61 + (1-E_Y)^2*0.39
  
E_W = 3 +9*E_X  
round(E_W, digits = 2)

E_V = 8 + 4*E_Y
round(E_V, digits = 2)

Var_W = 9^2*Var_X
round(E_V, digits = 4)

Var_V = 4^2*Var_Y
round(E_V, digits = 4)

E_XY = 0.24
Cov_XY = E_XY - E_X*E_Y
Cov_WV = 36*Cov_XY
round(Cov_WV, digits = 4)

Corr_WV = Cov_WV/(sqrt(Var_W*Var_V))
round(Corr_WV, digits = 4)


#######################################
## Question 18
#######################################

# Normal PDF function
#dnorm(x, mean = 0, sd = 1, log = FALSE)
# Normal CDF function
#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# Normal Inverse CDF (Quantile) function 
#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# Normal Random Generator
#rnorm(n, mean = 0, sd = 1)

# q1
mu = 6
var = 9
Y_cutoff = 5
q1 = pnorm(Y_cutoff, mean = mu, sd = sqrt(var), lower.tail = TRUE, log.p = FALSE)
round(q1 ,digits = 4)

# q2
mu = -1
var = 4
Y_cutoff = -2
q2 = pnorm(Y_cutoff, mean = mu, sd = sqrt(var), lower.tail = FALSE, log.p = FALSE)
round(q2 ,digits = 4)

# q3
mu = -3
var = 4
Y_cutoff_1 = -4
Y_cutoff_2 = -2
q3 = pnorm(Y_cutoff_2, mean = mu, sd = sqrt(var), lower.tail = TRUE, log.p = FALSE) - 
  pnorm(Y_cutoff_1, mean = mu, sd = sqrt(var), lower.tail = TRUE, log.p = FALSE)
round(q3 ,digits = 4)

