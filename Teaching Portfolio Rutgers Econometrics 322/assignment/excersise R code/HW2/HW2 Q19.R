# Solution for HW2 Q 19
# Econometrics 322 Rutgers
# Hang Miao 

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


