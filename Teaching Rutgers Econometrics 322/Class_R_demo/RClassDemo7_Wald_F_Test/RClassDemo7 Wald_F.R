############################################################################
# Class demo on Linear Regression
# Rutgers Econometrics 322
# Hang Miao
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



### matrix method
y = df$ahe
X = df[ , c(3,4,5)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-3-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

# multiple regression
linearMod2 <- lm(ahe ~ female+bachelor+age, data=df)
summary(linearMod2) 
summary(linearMod2)$coef

####################
# Joint Test
####################
# Conventional H_0:  beta1 = 0, beta2 = 0, beta3 = 0

A = cbind(rep(0,3) ,diag(3))
a = as.vector(rep(0,3))
d = 3

Wald_Stat = t(A %*% betaOLS-a) %*% solve(sigmasqaureHat*A %*% solve(t(X) %*% X) %*% t(A) ) %*% (A %*% betaOLS-a)
F_Stat = Wald_Stat/d

H1: Abeta != a

# Chi-square Distribution
Sig_alpha = 0.1
deg = d

# Critical Value
critical_Value = qchisq(1-Sig_alpha, deg)


# P-Value
pvalue = 1 - pchisq(Wald_Stat, deg)

# F Distiribution
Sig_alpha = 0.1 # 10% significant level
df1 = d
df2 = n-3-1

# Critical Value
critical_Value = qf(1-Sig_alpha, df1,df2)
critical_Value
F_Stat
# P-Value
pvalue = pf(F_Stat, df1,df2, lower.tail = FALSE)
pvalue
pvalue = 1- pf(F_Stat, df1,df2)
pvalue