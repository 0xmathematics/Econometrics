############################################################################
# Class demo 6: Linear Regression
############################################################################

############################################################################
library('readxl')
##############################
## Loading data
##############################
## load excel file
dtibble <- read_excel("cps08.xlsx", col_types = "numeric")
df = data.frame(dtibble)
head(df)
nrow(df)



### matrix method
y = df$ahe
X = df[ , c(3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

u_hat = y- X %*%betaOLS
SSR = t(u_hat)%*% u_hat
sigmasqaureHat = SSR/ (n-3-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

# Using Software Package 
linearMod2 <- lm(ahe ~ bachelor+female+age, data=df)
summary(linearMod2) 
summary(linearMod2)$coef
betaOLS
sdBetaOLS

# z statistics
z = (8.0830009 - 5)/ 0.20881336
# or
i = 2
b = 5
z = (betaOLS[i] - b)/ sdBetaOLS[i]

# t statistics
t = (8.0830009 - 5)/ 0.20881336
# or
i = 2
b = 5
z = (betaOLS[i] - b)/ sdBetaOLS[i]

# Critical Value for z and t statistics for H_1: beta_k > 5 at 1% significant level
qnorm(0.99)
deg = n-3-1
qt(0.99, deg)


2.326348
# P-value for z and t for H_1: beta_k > 5 at 1% significant level
z
1- pnorm(z)
1- pt(t,deg)

# 99% CI
beta1 = 8.0830009
threshold = qnorm(0.995)
sd_beta1 = 0.20881336

left = beta1- threshold * sd_beta1
right = beta1 + threshold* sd_beta1
CI99 = c(left,right)
CI99


