
############################################################################
# Midterm
############################################################################


########################
# Assignment 1  Q1
########################

N_Size = 5
Prob = 0.9
# Q3
# 3 up and 2 down
Q3 = dbinom( 3 , size = N_Size , prob=Prob)
round(Q3 ,digits = 3)

# Q4
0

# Q5
# 4 up and 1 down
Q4 = dbinom( 4 , size = N_Size , prob=Prob)
round(Q4 ,digits = 3)

# Q7
# cdf
Q7 = pbinom( 4 , size = N_Size , prob=Prob, )
round(Q7 ,digits = 3)

# Q8
# cdf
Q8 = pbinom( 4 , size = N_Size , prob=Prob, lower.tail = FALSE)
round(Q8 ,digits = 3)

Q8 = dbinom( 5 , size = N_Size , prob=Prob) + dbinom( 4 , size = N_Size , prob=Prob)
round(Q8 ,digits = 3)


# Q9
# cdf
Q9 = Q8 + dbinom( 3 , size = N_Size , prob=Prob)
round(Q9 ,digits = 3)

# Q10
Q10 = pbinom(2 , size = N_Size , prob=Prob )
round(Q10 ,digits = 3)

# the other way
Q10_0 = dbinom(0 , size = N_Size , prob=Prob )
Q10_1 = dbinom(1 , size = N_Size , prob=Prob )
Q10 = Q10_0+Q10_1
round(Q10 ,digits = 3)


############################################################
# Assignment 2
############################################################

# Uniform
#Q1
Q1 = punif(12, 1, 11)
round(Q1, digits = 3)

#Q3
Q3 = dunif(3, 1, 11)
round(Q3, digits = 3)

#Q4
Q4 = punif(3, 1, 11)
round(Q4, digits = 3)

#Q5
left = punif(5, 1, 11)
right = punif(10, 1, 11)
Q5 = right-left
round(Q5, digits = 3)

# Normal N(5,9)

# Q2
Q2 = dnorm(0,5,3)
round(Q2 ,digits = 3)

# Q3
Q3 = pnorm(0,5,3)
round(Q3 ,digits = 3)

# Q5
Q5 = pnorm(6,5,3, lower.tail = FALSE)
round(Q5 ,digits = 3)

# Q6
Q6 = pnorm(6,5,3)
round(Q6 ,digits = 3)

# Q7
right = pnorm(6,5,3)
left = pnorm(5,5,3)
Q7 = right - left
round(Q7 ,digits = 3)

#Q8
Q8 = qnorm(0.5,5,3)
round(Q8 ,digits = 3)

#Q9
Q9 = qnorm(0.75,5,3)
round(Q9 ,digits = 3)

#Q10
Q10 = qnorm(0.25,5,3)
round(Q10 ,digits = 3)


############################################################
# Assignment 3
############################################################
#original
set.seed(5)
RVunif = runif(9)
JointProb = RVunif/sum(RVunif)
Roundoff_JointProb = round(JointProb,digits = 4)
sum(Roundoff_JointProb)

  0       1       2
1 0.0386 0.1322 0.1768 
2 0.0549 0.0202 0.1352 
3 0.1018 0.1558 0.1845

#New
set.seed(10)
RVunif = runif(9)
JointProb = RVunif/sum(RVunif)
Roundoff_JointProb = round(JointProb,digits = 4)
Roundoff_JointProb
sum(Roundoff_JointProb)

    0       1       2
0 0.1489 0.0900 0.1253 
1 0.2034 0.0250 0.0662 
3 0.0806 0.0799 0.1807



BB = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
BB
colSums(BB)
rowSums(BB)
AA = matrix(Roundoff_JointProb, nrow = 3, ncol = 3, byrow = TRUE)
Px = rowSums(AA)
Px
Py = colSums(AA)
Py

#Q1
#Q2
Q2 = AA[3,3]
round(Q2,digits = 3)
#Q3
Q3 = Px[1]
round(Q3,digits = 3)
#Q4
Q4 = Py[2]
round(Q4,digits = 3)
#Q5
Q5 = AA[2,2]/Py[2]
round(Q5,digits = 3)
#Q6 E[X]
Q6 = 0*Px[1] + 1*Px[2]+ 2*Px[3]
round(Q6,digits = 3)
#Q7 Var[Y]
E_Y = 0*Py[1] + 1*Py[2]+ 2*Py[3]
E_Y2 = 0*Py[1] + 1*Py[2]+ 4*Py[3]
Q7 = E_Y2-E_Y^2
round(Q7,digits = 3)
#Q8 E[X|Y=0]
Q8 = 0*AA[1,1]/Py[1] + 1*AA[2,1]/Py[1]+ 2*AA[3,1]/Py[1]
round(Q8,digits = 3)

#Q9 Cov(X,Y)
E_XY = (1*AA[2,2] + 2*AA[2,3] + 2*AA[3,2] + 4*AA[3,3])
E_X = 0*Px[1] + 1*Px[2]+ 2*Px[3]
E_Y = 0*Py[1] + 1*Py[2]+ 2*Py[3]
Q9 = E_XY-E_X*E_Y
round(Q9,digits = 3)

#Q10 Corr(X,Y)
E_Y = 0*Py[1] + 1*Py[2]+ 2*Py[3]
E_Y2 = 0*Py[1] + 1*Py[2]+ 4*Py[3]
VarY = E_Y2-E_Y^2
E_X = 0*Px[1] + 1*Px[2]+ 2*Px[3]
E_X2 = 0*Px[1] + 1*Px[2]+ 4*Px[3]
VarX = E_X2-E_X^2

Q10 = Q9/sqrt(VarY*VarX)
round(Q10,digits = 3)

################################################################
# Assignment4
################################################################

set.seed(7)
X = rbinom(1000, 20, 0.6)

head(X)
mean(X)

write.csv(X,"Dataset_Q4.csv",row.names = FALSE)

##############################
## Load the dataset
##############################
df <- read.csv("Dataset_Q4.csv")

x = df[,1]
n = length(x)

##############################
## Analysis
##############################
#Q1
#Q2
#Q3
x_bar = mean(x)
round(x_bar,digits = 3)

#Q4 Variance of the estimator
# Var of X
Var_X = 4.8
n = length(x)
# Var of estimator
Var_X_bar = 1/n*Var_X
round(Var_X_bar,digits = 3)
############################
#sample variance
Sample_Var_X = sum((x-mean(x))^2)/(n-1)
Sample_Var_X
var(x)

#Q5
#Q6
#Q7
#Q8 z stat
z = (mean(x) -11 )/sqrt(Var_X_bar)
round(z,digits = 3)

# Q9
Q9 = qnorm(0.99)
round(Q9)
# Q10
# Q11 Pvalue
Q11 = 1-pnorm(z)
round(Q11, digits = 3)

#Q12
# 99 CI
cv = qnorm(0.995)
left = mean(x) - cv*sqrt(Var_X_bar)
right = mean(x) + cv*sqrt(Var_X_bar)

Q12 = c(left,right)
round(Q12, digits = 3)



##############################################################
## Assignment 5
##############################################################
set.seed(10)
X = rnorm(400, 10, 20)
Y = rnorm(400, 15, 30)
X = data.frame(X)
Y = data.frame(Y)
head(X)
head(Y)
tail(X)
tail(Y)

write.csv(X,"Dataset_Q5_X.csv",row.names = FALSE)
write.csv(Y,"Dataset_Q5_Y.csv",row.names = FALSE)

##############################
## Load the dataset
##############################

X <- read.csv("Dataset_Q5_X.csv")
Y <- read.csv("Dataset_Q5_Y.csv")

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

#Q3
x_bar = mean(x)
y_bar = mean(y)
Q3 = x_bar-y_bar
round(Q3, digits = 3)

#Q4
Var_X = 400
Var_Y = 900
var(x)
var(y)
y_bar = mean(y)
Var_XY = Var_X/n+Var_Y/m
round(Var_XY, digits = 3)
#Q5
#Q6
#Q7
#Q8
z = (mean(x)-mean(y))/sqrt(Var_XY)
round(z, digits = 3)

#Q9
# Critical Value
Q9 = qnorm(0.05)
round(Q9, digits = 3)
#Q10
#Q11 # P-value
Q11 = pnorm(z)
round(Q11, digits = 3)

#Q12  95%CI
cv = qnorm(0.975)
left = x_bar - y_bar - cv*sqrt(Var_XY)
right = x_bar - y_bar + cv*sqrt(Var_XY)
Q12 = c(left,right)
round(Q12, digits = 3)

