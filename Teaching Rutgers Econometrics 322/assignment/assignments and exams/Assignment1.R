########################
# Assignment 1 
# Rutger Econometrics 322
# Hang Miao
########################

N_Size = 5
Prob = 0.7
# Q4
# 4 up and 1 down
Q4 = dbinom( 4 , size = N_Size , prob=Prob)
round(Q4 ,digits = 3)

# Q7
# cdf
Q7 = pbinom( 4 , size = N_Size , prob=Prob, )
round(Q7 ,digits = 3)

# Q8
# cdf
Q8 = pbinom( 4 , size = N_Size , prob=Prob, lower.tail = FALSE)
round(Q8 ,digits = 3)

Q8 = dbinom( 5 , size = N_Size , prob=Prob)
round(Q8 ,digits = 3)

0.7^5


# Q9
# cdf
Q9 = Q8 + dbinom( 4 , size = N_Size , prob=Prob)
round(Q9 ,digits = 3)

# Q10
# 5 down, 4down 1 up, 3down 2 up no!!!
# 0 up,  1 up, 3down 2 up no!!! 101 > 100
# 1 way
Q10 = pbinom(1 , size = N_Size , prob=Prob )
round(Q10 ,digits = 3)

# the other way
Q10_0 = dbinom(0 , size = N_Size , prob=Prob )
Q10_1 = dbinom(1 , size = N_Size , prob=Prob )
Q10 = Q10_0+Q10_1
round(Q10 ,digits = 3)

