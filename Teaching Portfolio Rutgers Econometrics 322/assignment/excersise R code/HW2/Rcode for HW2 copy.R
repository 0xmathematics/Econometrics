#R code for HW2

#######################################
## Question 5
#######################################
# convert mean measured in Fahrenheit to which measured Celsius
mu_F = 47
sd_F = 5

mu_C = 5/9*(mu_F-32)
round(mu_C, digits = 3)

# convert sd measured in Fahrenheit to which measured Celsius
Var_F = sd^2
Var_C = (5/9)^2 * Var_F
sd_C = sqrt(Var_C)
round(sd_C, digits = 3)
# convert Var measured in Fahrenheit to which measured Celsius
# use properties of variance
round(Var_C, digits = 3)
#install packages for reading excel data file. Only need to install on your computer once.

#######################################
## Question 14
#######################################

E_X = 0*0.73 + 1*0.27
E_Y = 0*0.61 + 1*0.39
Var_X = (0-E_X)^2*0.73 + (1-E_X)^2*0.27
Var_Y = (0-E_Y)^2*0.61 + (1-E_Y)^2*0.39
  
E_W = 3 +9*E_X  
round(E_W, digits = 2)

E_V = 8 + 4*E_Y
round(E_V, digits = 2)

Var_W = 9^2*Var_X
round(E_V, digits = 4)

Var_V = 4^2*Var_Y
round(E_V, digits = 4)

E_XY = 0.24
Cov_XY = E_XY - E_X*E_Y
Cov_WV = 36*Cov_XY
round(Cov_WV, digits = 4)

Corr_WV = Cov_WV/(sqrt(Var_W*Var_V))
round(Corr_WV, digits = 4)


install.packages("readxl")
#install packages for data analysis
install.packages("dplyr")

#Load the package for reading excel data file. Have to excute this command every time you restart the Rstudio
library('readxl')

#Load the data analysis package dplyr and its dependency packages
library('base')
library('stats')
library('dplyr')
#set the working directory to where the data file located
setwd("C:/Users/merel/Desktop/R Emprical Analysis/HW2")


# normal densitiy function
#dnorm(x, mean = 0, sd = 1, log = FALSE)
# normal densitiy function
#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#rnorm(n, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
pnorm(1.96, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
pnorm(1.96, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
pnorm(1.96, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)-pnorm(-1.96, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


#########################################################################################################
# How to load the raw data into R
#########################################################################################################

#using the function read_excel in readxl package to load the data as as tibble data structure
dtibble <- read_excel("data.xlsx",   skip = 2, col_types = "numeric")
#here is what the data looks like:
head(dtibble) # the function head() shows the first ten rows of the data

#If you are more familiar with dtibble data structure, you could operate using dtibble.
#Since I prefer data frame data structure in R, so I will convert the dtibble further to data frame:
df = data.frame(dtibble)
# this is what data frame looks like. Simmilar to tibble. 
head(df)

#########################################################################################################
# the reason we want to transform  is the dataframe contains severel useful functions as follow:
#########################################################################################################
# column names: returns the column names of the data
colnames(df)
# select certain column name
colnames(df)[1]
# Select certain columns or rows by colume names
df['Hourly.Earnings..AHE.']
df["X25"]
# Select certain columns or rows by index
df[,1]
df[,2]
# Select certain columns or rows by which() function
rowIndex = which(df['Hourly.Earnings..AHE.']==6)
df[rowIndex,]
# add all the entries together
sum(df)
# add all joint density together
sum(df[,-1])
# add entries colume wisely: marginal density for age
colSums(df[,-1])
# add entries row wisely:  marginal density for hourly wage
rowSums(df[,-1])


#########################################################################################################
##### Plot the Expectation of AHE Conditioned on Age
#########################################################################################################

################################################################
# Data Visualization in R: Example

# x axis
x <- seq(-pi,pi,0.1)
#plot
plot(x, sin(x),main="Overlaying Graphs",ylab="y",xlab="x",type="l",col="blue")
#add lines
lines(x,cos(x), col="red")
#add legend
legend("topleft",c("sin(x)","cos(x)"),fill=c("blue","red"))
#type
#"p" - points
#"l" - lines
#"b" - both points and lines
#"c" - empty points joined by lines
#"o" - overplotted points and lines
#"s" and "S" - stair steps
#"h" - histogram-like vertical lines
#"n" - does not produce any points or lines
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

#################################################################
# Expectation
E_AHE = sum(df[,1] * df[,-1])
# t(conditional_expectation) %*% marginal_density

E_Age = ?????
#################################  

##################################


# Variance
Var_AHE = sum(df[,1]^2 * df[,-1]) - E_AHE^2
Var_Age = ?????
#################################  

##################################  
  

# Covariance
cov = sum((df[,1]-E_AHE) %*% t(age -E_Age) * df[,-1])

# Correlation
????? #Myeconlab answer is 0.1932





