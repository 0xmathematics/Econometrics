########################
# Assignment8
########################

#############################################################
## Question 1: Joint Statistical Inference: large sample
#############################################################

##############################
## Generate the dataset
##############################
set.seed(6)
N = 5000
u = rnorm(N, 0, 100)
x1= rnorm(N, 20, 5)
x2= rchisq(N, df = 20, ncp = 0)
x3= rbinom(n=N, size = 1, prob=0.1)
x4= rbinom(n=N, size = 1, prob=0.8)

beta0 = 5
beta1 = -3
beta2 = 3
beta3 = -1
beta4 = -5

y = beta0+ beta1*x1+beta2*x2+beta3*x3+beta4*x4+u

data = cbind(y,x1,x2,x3,x4)
head(data)

write.csv(data,"A8_Q1.csv",row.names = FALSE)


##############################
## Loading data
##############################
df <- read.csv("A8_Q1.csv")
head(df)


##############################
## Analysis
##############################

### matrix method
y = df[, 1]
X = df[ , c(2,3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-4-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

round(sdBetaOLS, digits = 3)

####################
# Joint Test
####################
#  H_0: b1 + 2b2 = 1, b3 = 2
#Q1
A =  matrix( c(0,1,2,0,0,0,0,0,1,0) , nrow = 2, ncol = 5, byrow = TRUE)
a = c(1,2)
d = 2
A
a
#Q2
#Q3
#Q4
Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
round(Wald_Stat, digits = 3)

#Q5
#Q6
# Chi-square Distribution
Sig_alpha = 0.9
deg = d
# Critical Value
critical_Value = qchisq(Sig_alpha, deg)
round(critical_Value, digits = 3)

# P-Value
pvalue = 1 - pchisq(Wald_Stat, deg)
round(pvalue, digits = 3)

# F Distiribution
Sig_alpha = 0.9
df1 = d
df2 = n-3-1
round(Wald_Stat, digits = 4)
# Critical Value
critical_Value = qf(Sig_alpha, df1,df2)
round(Wald_Stat, digits = 4)
# P-Value
pvalue = pf(F_Stat, df1,df2, lower.tail = FALSE)
round(Wald_Stat, digits = 4)

#############################################################
## Question 2: Joint Statistical Inference: small sample
#############################################################

##############################
## Generate the dataset
##############################
set.seed(7)
N = 40
u = rnorm(N, 0, 100)
x1= rnorm(N, 20, 5)
x2= rchisq(N, df = 20, ncp = 0)
x3= rbinom(n=N, size = 1, prob=0.1)
x4= rbinom(n=N, size = 1, prob=0.8)

beta0 = 5
beta1 = -3
beta2 = 3
beta3 = -1
beta4 = -5

y = beta0+ beta1*x1+beta2*x2+beta3*x3+beta4*x4+u

data = cbind(y,x1,x2,x3,x4)
head(data)

write.csv(data,"A8_Q2.csv",row.names = FALSE)


##############################
## Loading data
##############################
df <- read.csv("A8_Q2.csv")
head(df)


##############################
## Analysis
##############################

### matrix method
y = df[, 1]
X = df[ , c(2,3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-4-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

round(sdBetaOLS, digits = 3)


###########################
# Joint Test Small Sample
###########################
#  H_0: b1 + b2 = 0, b3 = -1
# A = identity matrix, a = 0 vector, d = 4
#Q1
A =  matrix( c(0,1,1,0,0,0,0,0,1,0) , nrow = 2, ncol = 5, byrow = TRUE)
a = c(0,-1)
d = 2
A
a
#Q2
#Q3
#Q4
Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
Wald_Stat
round(Wald_Stat, digits = 3)
F_Stat = Wald_Stat/d
round(F_Stat, digits = 3)
Q4 = F_Stat
round(Q4, digits = 3)

#Q5 degree of second argument

#Q6 95% siginificant level
Sig_alpha = 0.95
df1 = d
p = ncol(X) - 1 # number of features
df2 = n-p-1
# Critical Value
critical_Value = qf(Sig_alpha, df1,df2)
round(critical_Value, digits = 3)

#Q7 P-Value
pvalue1 = pf(F_Stat, df1,df2, lower.tail = FALSE)
round(pvalue, digits = 3)

pvalue2 = 1- pf(F_Stat, df1,df2)
round(pvalue, digits = 3)



