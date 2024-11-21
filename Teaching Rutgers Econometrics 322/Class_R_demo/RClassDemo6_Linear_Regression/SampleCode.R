############################################################################
# Class demo on Linear Regression
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


