# Question 1
# prepare the data
library(AER) 

data(CASchools) # Use data
CASchools$size <- CASchools$students/CASchools$teachers    # generating variable "size"
CASchools$score <- (CASchools$read + CASchools$math) / 2   # generating variable "score"

head(CASchools)
Income = CASchools$income
IncomeSquare = Income^2

write.csv(CASchools,"CASchoolsData.csv", row.names = FALSE)
#1.
quadratic_model <- lm(score ~ Income + IncomeSquare, data = CASchools) # Fit in a quadratic model
summary(quadratic_model)
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1") # Heteroskedastic Robust Standard Errors


# Log Linear Model
LogLinear_model <- lm( ~ ???, data = ???) # Estimate A Log Linear Function

coeftest(???, vcov = ???, type = "???") # Heteroskedastic Robust Standard Errors

# Log-Log Model
LogLog_model <- lm(??? ~ ???, data = ???)

coeftest(???, vcov = ???, type = "???") # Heteroskedastic Robust Standard Errors

# Question 2
library(AER)
data("CigarettesSW")
summary(CigarettesSW)
head(CigarettesSW)

write.csv(CASchools,"CigarettesSWData.csv", row.names = FALSE)

# compute real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

#  compute the sales tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

# generate a subset for the year 1995
c1995 <- subset(CigarettesSW, year == "1995")

# perform the regular OLS regression
c1995$log_packs <- log(c1995$packs)
c1995$log_rprice = log(c1995$rprice)
head(c1995)

#OLS_model = lm( log(c1995$packs) ~ log(c1995$rprice), data = c1995) 
OLS_model = lm( log_packs ~ log_rprice, data = c1995) 
coeftest(OLS_model, vcov. = vcovHC, type = "HC1")
coeftest(OLS_model, vcov. = vcovHC, type = "const")

# perform the first stage regression
cig_s1 <- lm(log_packs ~ salestax, data = c1995)
coeftest(cig_s1, vcov = vcovHC, type = "HC1")

summary(cig_s1)$coef

# perform TSLS using 'ivreg()'
cig_ivreg <- ivreg(log_packs ~ log_rprice | salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")


# Question 3
library(readxl)
library(quantmod)
# load US macroeconomic data
#USMacroSWQ <- read_xlsx("us_macro_quarterly.xlsx", sheet = 1,col_types = c("text", rep("numeric", 9)))
USMacroSWQ <- read_xlsx("us_macro_quarterly.xlsx")
data(USMacroSWQ)
head(USMacroSWQ)

# format date column
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")

# adjust column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", 
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# GDP series as xts object
GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]
head(GDP)
# GDP growth series as xts object
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))


GDPGRSub <- GDPGrowth["1962::2012"]
head(GDPGRSub)

#use ar.ols() command 
# report the outcome with standard errors and significant level
ar.ols(GDPGRSub, order.max = 1, demean = F, intercept = T)


