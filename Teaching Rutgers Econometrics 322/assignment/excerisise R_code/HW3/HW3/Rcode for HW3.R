## Rcode for HW3
## Hang Miao

#######################################
#question 2
#######################################

## import the 'readxl' package to read excel files
library('readxl')
## set the working directory same as the 
setwd("replace this with the Directory Path where this file and the excel data file located")

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

df_mean_100 = ?????
round(?????, digits = 3)




## 999 sample

################################################################
# Delete
###############################################################
dtibble <- read_excel("?????.xlsx", col_names = FALSE, col_types = "numeric")
df = data.frame(dtibble)
head(df)

df_mean_999 = ?????
round(?????, digits = 3)
################################################################
# Delete
###############################################################


#######################################
#question 3
#######################################
mu = ?????
sigma = sqrt(?????)
#In a random sample of size 184
n = ?????
?pnorm
Z_score = sqrt(n)*(114 - mu)/sigma
cdf_114 = pnorm(Z_score, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
round(cdf_114, digits = 4)


# In a random sample of size 107

n = ?????
Z_score1 = ?????
Z_score2 = ?????
cdf_113_117 = pnorm(?????, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) - 
  pnorm(?????, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
round(cdf_113_117, digits = 4)

#In a random sample of size n = 77

n = 77
Z_score = sqrt(n)*(113 - mu)/sigma
# we could use lower.tail = FALSE to compute the area under PDF greater than the Z_score.
cdf_113 = pnorm(Z_score, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
round(cdf_113, digits = 4)

#######################################
#question 4
#######################################
#p_hat
n = ?????
p_hat = ?????
round(p_hat, digits = 4)
#se
se = ?????
round(se, digits = 4)
#pvalue H1:p not eq 0.5
Z_score = ?????
pvalue = 2*pnorm(Z_score, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)
round(pvalue, digits = 3)
#pvalue H1:p gt 0.5
pvalue = pnorm(Z_score, mean = 0, sd = 1, lower.tail = F, log.p = FALSE)
round(pvalue, digits = 3)


#######################################
#question 7
#######################################
## confidence interval
n = ?????
mu = ?????
sd = ?????
?qnorm()
percentile_975 = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
percentile_975

CI_right = mu + 1/sqrt(n) * sd*percentile_975
CI_left = mu - 1/sqrt(n) * sd*percentile_975

round(c(CI_left,CI_right), digits = 2)

## mean-difference testing
mu1 = ?????
mu2 = ?????
sd1 = ?????
sd2 = ?????
n1 = ?????
n2 = ?????

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
dtibble <- read_excel("?????.xlsx", skip = 1, col_names = TRUE, col_types = "numeric")
df = data.frame(dtibble)
head(df)

# Subset the df for year ==1992
df_1992 = df[df['Year'] == 1992,]
df_1992
mean_1992 = mean(df_1992[,'Ahe'])
round(mean_1992, digits = 3)

# Subset the df for year ==2008
df_2008 = df[df['Year'] == ?????,]
df_2008
mean_2008 = mean(?????)
round(?????, digits = 3)

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
n = ?????
mu = ?????
sd = ?????

percentile_975 = ?????
percentile_975

CI_right = ?????
CI_left = ?????

round(?????, digits = 3)

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
df_2008_g = ?????
n = ?????
mu = ?????
sd = ?????

percentile_975 = ?????
percentile_975

CI_right = ?????
CI_left = ?????

round(?????, digits = 3)

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
















