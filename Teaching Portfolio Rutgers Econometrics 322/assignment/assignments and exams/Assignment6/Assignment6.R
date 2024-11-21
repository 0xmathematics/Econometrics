########################
# Assignment6
########################
###################################################################
##  linear regression Large sample
###################################################################
##############################
## Generate the  dataset
##############################
set.seed(5)
N = 5000
u = rnorm(N, 0, 100)
x1= rnorm(N, 20, 5)
x2= rchisq(N, df = 20, ncp = 0)
x3= rbinom(n=N, size = 1, prob=0.1)
x4= rbinom(n=N, size = 1, prob=0.8)

beta0 = 5
beta1 = -2
beta2 = 3
beta3 = 8
beta4 = -3

y = beta0+ beta1*x1+beta2*x2+beta3*x3+beta4*x4+u

data = cbind(y,x1,x2,x3,x4)
head(data)

write.csv(data,"A6.csv",row.names = FALSE)


##############################
## Loading data
##############################
df <- read.csv("A6.csv")
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

#Q3
betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y
round(betaOLS, digits = 3)

#Q4
uhat = y- X %*%betaOLS
SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-4-1)
sigmasqaureHat = as.numeric(sigmasqaureHat)
round(sigmasqaureHat, digits = 3)


#Q5
VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 
VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)
round(sdBetaOLS, digits = 3)

#Q9
b = 0 #null hypothesis
z = (betaOLS[2]-b)/sdBetaOLS[2]
round(z, digits = 3)

#Q10
Q10 = qnorm(.995)
round(Q10, digits = 3)

#Q12
Q12 = 2*pnorm(-abs(z))
round(Q12, digits = 3)

# multiple regression
linearMod2 <- lm(y ~ x1+x2+x3+x4, data=df)
summary(linearMod2) 
summary(linearMod2)$coef
betaOLS
sdBetaOLS

#Q12
Q12 = 2*pnorm(-abs(z))
round(Q12, digits = 3)

#Q13 99%CI
i = 2

cv = qnorm(0.995)
cv = qt(0.995,n-1-4)
left = betaOLS[i]  - cv*sdBetaOLS[i]
right = betaOLS[i]  + cv*sdBetaOLS[i]
CI99 = c(left,right)
round(CI99, digits = 3)

linearMod2 <- lm(y ~ x1+x2+x3+x4, data=df)
summary(linearMod2) 
summary(linearMod2)$coef
confint(linearMod2,'x1',0.99)



