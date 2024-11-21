########################
# Assignment7
########################
###################################################################
##  Small Sample Linear Regression
###################################################################
##############################
## Generate the  dataset
##############################
set.seed(6)
N = 30
u = rnorm(N, 0, 10)
x1= rnorm(N, 20, 4)
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

write.csv(data,"A7.csv",row.names = FALSE)

##############################
## Loading data
##############################
df <- read.csv("A7.csv")
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
round(sdBetaOLS,digits = 3)

# Check the Code Using Third Party Package
linearMod2 <- lm(y ~ x1+x2+x3+x4, data=df)
summary(linearMod2) 
summary(linearMod2)$coef
round(betaOLS, digits = 3)
round(sdBetaOLS,digits = 3)

# H0: beta2 = 2 
# H1: beta2 > 2

# t = (estimator - true parameter) / sd(estimator )

t = ( betaOLS[3] - 2) / sdBetaOLS[3]

round(t,3 )

# H0: beta2 = 2 
# H1: beta2 != 2

t = ( betaOLS[3] - 2) / sdBetaOLS[3]


n = nrow(y)
p = 4
deg = n-p-1
# cdf(-abs(t) )
pt (- abs(t),deg) 
# 1 - cdf( abs(t) )

pvalue = 2 * pt (- abs(t),deg)  
pvalue = 1 - pt(abs(t),deg ) +  pt (- abs(t),deg)  
round(pvalue,3 )

# H1: > < !=






#Q10
b = 2 #null hypothesis
i = 3 # the beta_2
t = (betaOLS[i]-b)/sdBetaOLS[i]
round(t, digits = 3)

#Q11
deg = n - ncol(X)
Q11 = qt(.90,df=deg)
round(Q11, digits = 3)


#Q13
Q12 = 1- pt(t, deg)
round(Q12, digits = 3)

#Q14 99%CI
i = 3
cv = qt(0.95,deg)
left = betaOLS[i]  - cv*sdBetaOLS[i]
right = betaOLS[i]  + cv*sdBetaOLS[i]
CI90 = c(left,right)
round(CI90, digits = 3)

# multiple regression
linearMod2 <- lm(y ~ x1+x2+x3+x4, data=df)
summary(linearMod2) 
summary(linearMod2)$coef