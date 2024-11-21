
1+1


2^3

2*3

9/3

n <- 15
n

n <- 8

variable_name = 8
variable_name

N <- 26.42

sample(  c(1, 0)   , 1)

#Example binomial ((finite) Discret random variable )
# hit the target 10 times (n = 10)
# prob of successfully hit the target is 0.6


# Q: what is the prob of successfully hit the target by 10 times?
# Q: what is the prob of successfully hit the target by 0 times?
# Q: what is the prob of successfully hit the target by 7 times?
# Q: what is the pmf?



# Q: what is the cdf?
#ANS
0.6^10

#ANS
0.4^10

#ANS when k = 7
# (10 choose 7) * 0.6^7* 0.4^3
0.2149908

# dbinom() is pmf function of binomial RV
dbinom(7, size = 10, prob = 0.6)

#ANS
p0 <- dbinom(0, size = 10, prob = 0.6)
p1 <- dbinom(1, size = 10, prob = 0.6)
p2 <- dbinom(2, size = 10, prob = 0.6)
#...
dbinom(7, size = 10, prob = 0.6)
dbinom(10, size = 10, prob = 0.6)

# Q: what is the probability Pr( X <= 2) ? i.e CDF(2) = Pr( X <= 2)
# calculate the probablity of hiting the target by 0, 1, 2
p0 + p1 + p2
pbinom(2,size=10, prob=0.6)

# Q: what is the probability Pr( X <= 7) ? i.e CDF(7) = Pr( X <= 7)
# calculate the probablity of hiting the target by 0, 1, 2, 3, 4, 5, 6,7
pbinom( 7,size=10, prob=0.6)
 
#pbinom() cdf function of binomial RV


# for discrete random variable
# d stands for pmf function
# p stands for cdf function



# for continuous random variable 
# d stands for pdf fucniton
# p stands for cdf function



p1 = dbinom(1, size = 10, prob = 0.8, log = FALSE)

p8 = dbinom(8, size = 10, prob = 0.8, log = FALSE)

p0 + p1+ ... + p8

pbinom(8, size = 10, prob=0.8, lower.tail = TRUE, log.p = FALSE)
pbinom(8, size = 10, prob=0.8)


#p8 = dbinom(8, size = 10, prob = 0.8, log = FALSE)
p9 = dbinom(9, size = 10, prob = 0.8, log = FALSE)
p10 = dbinom(10, size = 10, prob = 0.8, log = FALSE)
p9+p10

pbinom(8, size = 10, prob=0.8, lower.tail = FALSE, log.p = FALSE)


rbinom(100, size=10, prob=0.8)


rbinom(1 ,size=1, prob=0.8)

sample(c("Head", "Tail"), 1)

dbinom(1 ,size=1, prob=0.8)

dbinom(0 ,size=1, prob=0.8)

pbinom(0 ,size=1, prob=0.8)

pbinom(1 ,size=1, prob=0.8)


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




