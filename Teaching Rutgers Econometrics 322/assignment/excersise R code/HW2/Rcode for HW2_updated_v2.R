#R code for HW2
#Econometrics 322
#Hang Miao

#######################################
## Question 3
#######################################
# convert mean measured in Fahrenheit to which measured Celsius
mu_f = 59
sd_f = 11

mu_c = 5/9*(mu_f-32)
round(mu_c, digits = 3)
# convert sd measured in Fahrenheit to which measured Celsius

sd_c = (5/9)*sd_f
round(sd_c, digits = 3)
# convert Var measured in Fahrenheit to which measured Celsius
# use properties of variance
var = ?????
round(var, digits = 3)
#install packages for reading excel data file. Only need to install on your computer once.

#######################################
## Question 4
#######################################
# by the definition of expectation
E_X = 0*0.55 + 1*0.45
E_Y = 0*0.44 + 1*0.56


Var_X = (0-E_X)^2*0.55 + (1-E_X)^2*0.45
Var_Y = (0-E_X)^2*0.44 + (1-E_X)^2*0.56
  
E_W = 9 +2*E_X  
round(E_W, digits = 2)

E_V = 4 + 6*E_Y
round(E_V, digits = 2)

Var_W = 2^2*Var_X
round(Var_W, digits = 4)

Var_V = ?????
round(Var_V, digits = 4)

E_XY = ?????
Cov_XY = E_XY - E_X*E_Y
Cov_WV = ?????*Cov_XY
round(Cov_WV, digits = 4)

Corr_WV = ?????
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


#################################################
# #round function
#################################################
Marginal_Density_Age30 = colSums(df[,-1])['X30']
round(Marginal_Density_Age30, digits = 4)

#########################################################################################################
# Digression: Apply Matrix Operation to Dataframe
#########################################################################################################
###review of matrix algebra
# creat matrix
M1 = matrix(1:10, nrow = 5, ncol = 2, byrow = TRUE)
M1
M2 = matrix(rep(1,times = 10), nrow = 5, ncol = 2, byrow = TRUE)
M2
# creat vector
V1 = c(1:5)
V1
V2 = rep(1, times = 5)
V2
# transpose a column vector to row vector
t(V1)
# transpose M1 a 5*2 matrix to a 2*5 matrix
t(M1)

# vector sumation
V1 + V2
# vector  element wise product
V1 * V2
# vector inner product
t(V1) %*% V2


# matrix addition element wise
M1 + M2
# matrix multiplication:
# M1 (a 5*2 matrix) multiply transpose of M2 (a 2*5 matrix) 
M1 %*% t(M2) 
# transpose of M1 (a 2*5 matrix) multiply transpose of M2 (a 2*5 matrix)
t(M1) %*% M2

# simmilar to the element wise vector product, R is allowed to multiply a vector to matrix element wisely 
# a 5 by 2 matrix multiply a 5 by 1 vector, apply the elemnt-wise vector product column wisely
M1 * V1
#  a 5 by 1 vector multiply a 5 by 2 matrix, same result
V1 * M1 

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





