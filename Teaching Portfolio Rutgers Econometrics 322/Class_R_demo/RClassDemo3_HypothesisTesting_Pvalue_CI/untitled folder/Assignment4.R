########################
# Assignment4
########################

set.seed(5)
X = rbinom(1000, 40, 0.5)

head(X)
mean(X)

write.csv(X,"Dataset_Assignment4.csv",row.names = FALSE)

##############################
## Load the dataset
##############################
df <- read.csv("Dataset_Assignment4.csv")

x = df[,1]
n = length(x)

##############################
## Analysis
##############################

#Q4
x_bar = mean(x)
round(x_bar,digits = 3)

#Q5 Variance of the estimator
# Var of X
Var_X = 10
n = length(x)
# Var of estimator
Var_X_bar = 1/n*Var_X
round(Var_X_bar,digits = 3)

############################################################
# Sample Variance of the X
Sample_Var_x = sum((x - x_bar)^2)/(n-1)
Sample_Var_x
Sample_Var_x = sum((x - x_bar)^2)/n
Sample_Var_x

# Variance of the estimator with unknown Variance parameter
var_xbar = Sample_Var_x/n
var_xbar

#sd of the estimator
sd_xbar = sqrt(var_xbar)
sd_xbar
###############################################################


# statistics
z = (x_bar - 22)/sqrt(var_xbar)
z
# Q9
z = (x_bar - 22)/sqrt(Var_X_bar)
round(z, digits = 3)
# Q10
# Critical Value
qnorm(0.10)
#Q12
# P-value
q12 = pnorm(z)
round(q12, digits = 3)

#Q13
# 90 CI
cv = qnorm(0.95)
left = x_bar - cv*sd_xbar
right = x_bar + cv*sd_xbar

CI90 = c(left,right)

round(CI90, digits = 3)
