###########################################################################
# R tutorial  
# Author: Hang Miao
# For Econometrics class use only, non distributable.
###########################################################################


######################################################
# 0 Assignment and basics
######################################################
n <- 15
n
a = 12
a
24 -> z
z
#Variables must start with a letter, but may also contain numbers and periods. R is case sensitive.
N <- 26.42
N
n

#To see a list of your objects, use ls( ). The ( ) is required, even though there are no arguments.
ls()
#Use rm to delete objects you no longer need.
rm(n)
ls()

#You may see online help about a function using the help command or a question mark.
?ls
help(rm)
#Several commands are available to help find a command whose name you don't know. Note that
#anything after a pound sign (#) is a comment and will not have any effect on R.

help.search("help") # "help" in name or summary; note quotes!
help.start() # also remember the R Commands web page (link on class page)

#Other data types are available. You do not need to declare these; they will be assigned
#automatically.
name <- "Mike" # Character data
name

q1 <- TRUE # Logical data
q1

q2 <- F
q2
####################################################
## 1. Simple calculation
####################################################
#R may be used for simple calculation, using the standard arithmetic symbols +, -, *, /, as well as
#parentheses and ^ (exponentiation).
a <- 12+14
a
3*5
(20-4)/2
7^2
#Standard mathematical functions are available. 
exp(2)

log(10) # Natural log

log10(10) # Base 10

log2(64) # Base 2

pi

cos(pi)

sqrt(100) # square root

####################################################
## 2. Vectors
####################################################

# Vectors may be created using the c command, separating your elements with commas.
a <- c(1, 7, 32, 16)
a

#Sequences of integers may be created using a colon (:).
b <- 1:10
b

c <- 20:15
c

# Other regular vectors may be created using the seq (sequence) and rep (repeat) commands.
d <- seq(1, 5, by=0.5)
d
e <- seq(0, 10, length=5)
e
f <- rep(0, 5)
f
g <- rep(1:3, 4)
g
h <- rep(4:6, 1:3)
h

#Random vectors can be created with a set of functions that start with r, such as rnorm (normal) or
#runif (uniform).
x <- rnorm(5) # Standard normal random variables
x

y <- rnorm(7, 10, 3) # Normal r.v.s with mu = 10, sigma = 3
y

z <- runif(10) # Uniform(0, 1) random variables
z

#If a vector is passed to an arithmetic calculation, it will be computed element-by-element.
c(1, 2, 3) + c(4, 5, 6)
sqrt(c(100, 225, 400))

#If the vectors involved are of different lengths, the shorter one will be repeated until it is the
#same length as the longer.
c(1, 2, 3, 4) + c(10, 20)
c(1, 2, 3) + c(10, 20)

#To select subsets of a vector, use square brackets ([ ]).
d
d[3]
d[5:7]

#A logical vector in the brackets will return the TRUE elements.
d > 2.8
d[d > 2.8]

#The number of elements in a vector can be found with the length command.
length(d)
length(d[d > 2.8])

####################################################
## 3. Simple statistics
####################################################

#There are a variety of mathematical and statistical summaries which can be computed from a
#vector.
1:4

sum(1:4)

prod(1:4) # product
24
max(1:10)

min(1:10)

range(1:10)

X <- rnorm(10)
X
mean(X)
sort(X)
median(X)
var(X)
sd(X)

####################################################
## 4. Matrices
####################################################

#Matrices can be created with the matrix command, specifying all elements (column-by-column)
#as well as the number of rows and number of columns.
A <- matrix(1:12, nr=3, nc=4)
A

#You may also specify the rows (or columns) as vectors, and then combine them into a matrix
#using the rbind (cbind) command.

a <- c(1,2,3)
a

b <- c(10, 20, 30)
b

c <- c(100, 200, 300)
c

d <- c(1000, 2000, 3000)
d

B <- rbind(a, b, c, d)
B

C <- cbind(a, b, c, d)
C

#To select a subset of a matrix, use the square brackets and specify rows before the comma, and
#columns after.
C[1:2,]

C[,c(1,3)]

C[1:2,c(1,3)]

#Matrix multiplication is performed with the operator %*%. Remember that order matters!
B%*%C
C%*%B

#You may apply a summary function to the rows or columns of a matrix using the apply function.
C
sum(C)
apply(C, 1, sum) # apply sum function on each row
apply(C, 2, sum) # apply sum function on each column
rowSums(C)
colSums(C)
