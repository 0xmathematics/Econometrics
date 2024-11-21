## Rcode for HW3
## Hang Miao


################################################################
# Preparation (start)
################################################################
################################################################
# Install the following if you haven't install before.
# you need to install them only once
################################################################
install.packages("readxl")
#install packages for data analysis
install.packages("dplyr")

#################################################################
#Load the package for reading excel data file. 
# Have to excute these commands every time you restart the Rstudio
################################################################
library('readxl')
#Load the data analysis package dplyr and its dependency packages
library('base')
library('stats')
library('dplyr')

################################################################
#set the working directory to where the data file located
setwd(" enter your wd here ")
#There is another way illustrated in the PDFtutorial on sakai.
################################################################

#####################################################################
# The following is the code with comment for loading data from excel
# Since all of the data in Myeconlab stored in the excel format, you will
# see code similar to the following one again and again from now on but with no comment
#####################################################################
#using the function read_excel in readxl package to load the data as as tibble data structure
dtibble <- read_excel("data-9_22_2019-9_27 PM.xlsx",   skip = 2, col_types = "numeric")
#here is what the data looks like:
head(dtibble) # the function head() shows the first ten rows of the data

#If you are more familiar with dtibble data structure, you could operate using dtibble.
#Since I prefer data frame data structure in R, so I will convert the dtibble further to data frame:
df = data.frame(dtibble)
# this is what data frame looks like. Simmilar to tibble. 
head(df)
################################################################
# Preparation (end) 
################################################################

# Here is the start for the sample code of HW3  
#######################################
#question 2
#######################################

## import the 'readxl' package which is required to read excel files
library('readxl')

###############################################
## set the working directory using the following method
setwd("replace this line with the Directory Path where this file and the excel data file located")
## If you use the method illustrated in the vedio or PDF document,
## then you could safely skip this step
##############################################

## 10sample
dtibble <- read_excel("data-1_27_2019-2_09 PM.xlsx", col_names = FALSE, col_types = "numeric")
df = data.frame(dtibble)
head(df)

df_mean_10 = mean(df[,1])
round(df_mean_10, digits = 3)

## 100 sample
dtibble <- read_excel("data-1_27_2019-2_24 PM.xlsx", col_names = FALSE, col_types = "numeric")
df = data.frame(dtibble)
head(df)

df_mean_100 = mean(df[,1])
round(df_mean_100, digits = 3)

## 999 sample

dtibble <- read_excel("data-1_27_2019-2_28 PM.xlsx", col_names = FALSE, col_types = "numeric")
df = data.frame(dtibble)
head(df)

df_mean_999 = mean(df[,1])
round(df_mean_999, digits = 3)


#######################################
#question 3
#######################################
mu = 112
sigma = sqrt(58)
#In a random sample of size n = 184
n = 184

Z_score = sqrt(n)*(114 - mu)/sigma
cdf_114 = pnorm(Z_score, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
round(cdf_114, digits = 4)

# In a random sample of size n = 107

n = 107
Z_score1 = sqrt(n)*(117 - mu)/sigma
Z_score2 = sqrt(n)*(113 - mu)/sigma
cdf_113_117 = pnorm(Z_score1, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) - 
  pnorm(Z_score2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
round(cdf_113_117, digits = 4)

#In a random sample of size n = 77

n = 77
Z_score = sqrt(n)*(113 - mu)/sigma
cdf_113 = pnorm(Z_score, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)
round(cdf_113, digits = 4)

#######################################
#question 4
#######################################
#p_hat
n = 400
p_hat = 213/n
round(p_hat, digits = 4)
#se
se = sqrt(p_hat*(1-p_hat)/n)
round(se, digits = 4)
#pvalue H1:p not eq 0.5
Z_score = (p_hat - 0.5)/se
pvalue = 2*pnorm(Z_score, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)
round(pvalue, digits = 3)
#pvalue H1:p gt 0.5
pvalue = pnorm(Z_score, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)
round(pvalue, digits = 3)


#######################################
#question 7
#######################################
## confidence interval
n = 414
mu = 678.5
sd = 20.5
?qnorm()
percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 2)

## mean-difference testing
mu1 = 690.3
mu2 = 682.5
sd1 = 20.4
sd2 = 18.8
n1 = 229
n2 = 182

sd_diff = sqrt(sd1^2/n1 + sd2^2/n2)
t_stat = (mu1-mu2)/sd_diff
round(t_stat, digits = 2) 

## p-value
pvalue = pnorm(t_stat, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)
round(pvalue, digits = 6) 

#######################################
#question 8
#######################################
## Load Excel Data
dtibble <- read_excel("data-1_28_2019-1_57 PM.xlsx", skip = 1, col_names = TRUE, col_types = "numeric")
df = data.frame(dtibble)
head(df)

# Subset the df for year ==1992
df_1992 = df[df['Year'] == 1992,]
df_1992
mean_1992 = mean(df_1992[,'Ahe'])
round(mean_1992, digits = 3)

# Subset the df for year ==2008
df_2008 = df[df['Year'] == 2008,]
df_2008
mean_2008 = mean(df_2008[,'Ahe'])
round(mean_2008, digits = 3)

# Confidence Interval for 1992 
n = nrow(df_1992)
mu = mean(df_1992[,'Ahe'])
sd = sd(df_1992[,'Ahe'])

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 3)


# Confidence Interval for 2008 
n = nrow(df_2008)
mu = mean(df_2008[,'Ahe'])
sd = sd(df_2008[,'Ahe'])

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 3)

# Confidence Interval between 1992 and 2008 
mu1 = mean(df_2008[,'Ahe'])
mu2 = mean(df_1992[,'Ahe'])
sd1 = sd(df_2008[,'Ahe'])
sd2 = sd(df_1992[,'Ahe'])
n1 = nrow(df_2008)
n2 = nrow(df_1992)

mu_diff = mu1-mu2
sd_diff = sqrt(sd1^2/n1 + sd2^2/n2)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu_diff +  sd_diff*percentile_975
CI_left = mu_diff - sd_diff*percentile_975

round(c(CI_left,CI_right), digits = 3)



## inflation-adjusted average hourly earnings (AHE) in 1992
CPI1 = 215.2
CPI2 = 140.3

Adj_Price = df_1992[,'Ahe']*(CPI1/CPI2)
mean_Adj_Price = mean(Adj_Price)
round(mean_Adj_Price, digits = 3)

## CI for inflation-adjusted average hourly earnings (AHE) in 1992
n = nrow(df_1992)
mu = mean_Adj_Price
sd = sd(Adj_Price)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 3)

## CI for the difference between 
## inflation-adjusted average hourly earnings (AHE) in 1992
## and 2008

# Confidence Interval between 1992 and 2008 
mu1 = mean(df_2008[,'Ahe'])
mu2 = mean_Adj_Price
sd1 = sd(df_2008[,'Ahe'])
sd2 = sd(Adj_Price)
n1 = nrow(df_2008)
n2 = nrow(df_1992)

mu_diff = mu1-mu2
sd_diff = sqrt(sd1^2/n1 + sd2^2/n2)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu_diff +  sd_diff*percentile_975
CI_left = mu_diff - sd_diff*percentile_975

round(c(CI_left,CI_right), digits = 3)

## 2008 CI for mean of AHE for High School Graduates
df_2008_hs = df_2008[df_2008['Bachelor'] == 0,]
n = nrow(df_2008_hs)
mu = mean(df_2008_hs[,'Ahe'])
sd = sd(df_2008_hs[,'Ahe'])

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 3)

## 2008 CI for mean of AHE for Graduates
df_2008_g = df_2008[df_2008['Bachelor'] == 1,]
n = nrow(df_2008_g)
mu = mean(df_2008_g[,'Ahe'])
sd = sd(df_2008_g[,'Ahe'])

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 3)

## 2008 mean diff between high school and graduates
mu1 = mean(df_2008_g[,'Ahe'])
mu2 = mean(df_2008_hs[,'Ahe'])
sd1 = sd(df_2008_g[,'Ahe'])
sd2 = sd(df_2008_hs[,'Ahe'])
n1 = nrow(df_2008_g)
n2 = nrow(df_2008_hs)

mu_diff = mu1-mu2
sd_diff = sqrt(sd1^2/n1 + sd2^2/n2)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu_diff +  sd_diff*percentile_975
CI_left = mu_diff - sd_diff*percentile_975

round(c(CI_left,CI_right), digits = 3)

## CI for Inflation Adjusted mean of 1992 AHE for high school
CPI1 = 215.2
CPI2 = 140.3


df_1992_hs = df_1992[df_1992['Bachelor'] == 0,]
Adj_df_1992_hs = df_1992_hs[,'Ahe']*(CPI1/CPI2)

n = length(Adj_df_1992_hs)
mu = mean(Adj_df_1992_hs)
sd = sd(Adj_df_1992_hs)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 3)

## 1992 inflation adjusted CI for mean of AHE for Graduates
df_1992_g = df_1992[df_1992['Bachelor'] == 1,]
Adj_df_1992_g = df_1992_g[,'Ahe']*(CPI1/CPI2)

n = length(Adj_df_1992_g)
mu = mean(Adj_df_1992_g)
sd = sd(Adj_df_1992_g)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975
round(c(CI_left,CI_right), digits = 3)

## 1992 mean diff between high school and graduates
mu1 = mean(Adj_df_1992_g)
mu2 = mean(Adj_df_1992_hs)
sd1 = sd(Adj_df_1992_g)
sd2 = sd(Adj_df_1992_hs)
n1 = length(Adj_df_1992_g)
n2 = length(Adj_df_1992_hs)

mu_diff = mu1-mu2
sd_diff = sqrt(sd1^2/n1 + sd2^2/n2)

percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu_diff +  sd_diff*percentile_975
CI_left = mu_diff - sd_diff*percentile_975

round(c(CI_left,CI_right), digits = 3)





#######################################
#question 18 (original question 19) 
#######################################

################################################################
# Install the following if you haven't install before.
# you need to install them only once
################################################################
install.packages("readxl")
#install packages for data analysis
install.packages("dplyr")

#################################################################
#Load the package for reading excel data file. 
# Have to excute these commands every time you restart the Rstudio
################################################################
library('readxl')
#Load the data analysis package dplyr and its dependency packages
library('base')
library('stats')
library('dplyr')

################################################################
#set the working directory to where the data file located
setwd(" enter your wd here ")
#There is another way illustrated in the PDFtutorial on sakai.
################################################################

################################################################
# All the following code is for loading data from excel
################################################################
#using the function read_excel in readxl package to load the data as as tibble data structure
dtibble <- read_excel("data-9_22_2019-9_27 PM.xlsx",   skip = 2, col_types = "numeric")
#here is what the data looks like:
head(dtibble) # the function head() shows the first ten rows of the data

#If you are more familiar with dtibble data structure, you could operate using dtibble.
#Since I prefer data frame data structure in R, so I will convert the dtibble further to data frame:
df = data.frame(dtibble)
# this is what data frame looks like. Simmilar to tibble. 
head(df)

################################################################
# Marginal Prob for Age
################################################################
Pr_M_Age = colSums(df[,-1])
round(Pr_M_Age, digits = 4)

################################################################
# Conditional Expectation for AHE given Age = 30
################################################################
#Marginal Prob of Age = 30 is:
# one way
Pr_M_Age30 = colSums(df['X30'])
# another way
Pr_M_Age30 = Pr_M_Age['X30']

#conditional Prob of AHE given Age = 30 is:
Pr_C_Age30 = df['X30']/Pr_M_Age30

#conditional Expectation of AHE given Age = 30 is:
E_AHE_age30 = sum(df[,1]*Pr_C_Age30)
round(E_AHE_age30, digits = 4)


################################################################
# Replicate the Graph in Myeconlab using R (not a question)
################################################################
marginal_density = round(colSums(df[,-1]),digits = 4)
conditional_expectation =  colSums(df[,1] * df[,-1])/marginal_density
colnames(df)[-1]
plot(conditional_expectation,
     xaxt = "n",yaxt = "n",
     main="Expectation of AHE Conditioned on Age",
     ylab="E[ AHE | Age ]",xlab="Age",
     col.lab="red", cex.lab=0.9,
     ylim =c(15,30))
axis(1, at=1:10, labels=colnames(df)[-1],cex.axis=0.7 )
axis(2, cex.axis=0.7 )



################################################################
# Expectation of AHE
################################################################
# Marginal Prob for AHE is:
Pr_M_AHE = rowSums(df[,-1])
round(Pr_M_AHE, digits = 4)

#Expectation of AHE is:
E_AHE = sum(df[,1]*Pr_M_AHE)
round(E_AHE, digits = 4)

################################################################
# Variance of AHE
################################################################
# using Var[X] = E[X^2] - E[X]^2
V_AHE = sum(df[,1]^2* Pr_M_AHE) - E_AHE^2
round(V_AHE, digits = 4)


################################################################
# Covariance of AHE and Age
################################################################
# using Cov(X,Y) = E[XY] - E[X]E[Y]

# X
AHE = df[,1]
# Y
Age = c(25:34)
# note: c(25:34) here is creating a column range from 25 to 34

#E[X]
E_Age = sum(Age *Pr_M_Age)


# E[XY]  
E_AHE_Age = sum( AHE%*%t(Age)*df[,-1]  )

# Cov(X,Y)
Cov_AHE_Age = E_AHE_Age - E_Age*E_AHE

round(Cov_AHE_Age, digits = 4)


################################################################
# Correlation of AHE and Age
################################################################
# using Corr(X,Y) = Cov(X,Y)/sqrt(Var[X]Var[Y] )

# Var[Y]
Age = c(25:34)
V_Age = sum(Age^2*Pr_M_Age) - E_Age^2

# Corr(X,Y)
Corr_AHE_Age = Cov_AHE_Age/ sqrt(V_Age*V_AHE)
round(Corr_AHE_Age, digits = 4)











