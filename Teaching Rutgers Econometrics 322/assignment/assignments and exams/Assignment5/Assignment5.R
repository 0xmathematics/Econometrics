########################
# Assignment5
########################

###################################################################
## Mean Difference Estimation and Statistical Inference
###################################################################

##############################
## Generate the dataset
##############################
set.seed(5)
X = rnorm(1000, 15, 30)
Y = rnorm(2000, 10, 40)
X = data.frame(X)
Y = data.frame(Y)
head(X)
head(Y)
tail(X)
tail(Y)

write.csv(X,"Assignment5_X.csv",row.names = FALSE)
write.csv(Y,"Assignment5_Y.csv",row.names = FALSE)

##############################
## Load the dataset
##############################

X <- read.csv("Assignment5_X.csv")
Y <- read.csv("Assignment5_Y.csv")

nrow(X)
nrow(Y)

x = X[,1]
y = Y[,1]

##############################
## Analysis
##############################
n = length(x)
n
m = length(y)
m
x_bar = mean(x)
y_bar = mean(y)
x_bar-y_bar

#variance of the x
var_x = 30
var_x

#variance of the y
var_y = 40
var_y

#variance of the estimator
var_mean_Diff = var_x/n+var_y/m
var_mean_Diff
#sd of the estimator
sd_mean_Diff = sqrt(var_mean_Diff)
sd_mean_Diff
# statistics
z = (x_bar - y_bar )/sd_mean_Diff
z
#Q10
# Critical Value
qnorm(0.95)
#Q12
# P-value
pnorm(z)
# 90 CI
cv = qnorm(0.975)
left = x_bar - y_bar - cv*sd_mean_Diff
right = x_bar - y_bar + cv*sd_mean_Diff

CI95 = c(left,right)

round(CI95, digits = 3)
