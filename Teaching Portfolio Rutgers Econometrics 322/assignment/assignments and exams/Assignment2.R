########################
# Assignment 2
# Rutger Econometrics 322
# Hang Miao
########################

# Uniform
Q4 = dunif(4, 2, 6)
round(Q4, digits = 3)

Q5 = punif(4, 2, 6)
round(Q5, digits = 3)

Q6_right = punif(6, 2, 6)
Q6_left = punif(5, 2, 6)
Q6 = Q6_right - Q6_left
round(Q6, digits = 3)


# Normal
pdfQ2 = dnorm(3,0,1)
round(pdfQ2 ,digits = 3)

cdfQ3 = pnorm(3,0,1)
round(cdfQ3 ,digits = 3)



#Suppose X~ N(2,4), 
#Q1: what is pdf (X = 3)?
#Q2: what is cdf (X = 2)?
#Ans Q2: cdf (X = 2) = Prob(X<= 2)
Q1 = dnorm(3,2,2  )
Q2 = pnorm(2,2,2  )
  
pnorm(0,2,2  )

 
#6
pdfQ6 = pnorm(1,0,1)
round(pdfQ6 ,digits = 3)

#7
pdfQ7 = pnorm(-1,0,1)
round(pdfQ7 ,digits = 3)


#8
pdfQ8 = pnorm(1,0,1, lower.tail = FALSE)



round(pdfQ8 ,digits = 3)

pdfQ8 = pnorm(1,0,1)
round(1-pdfQ8 ,digits = 3)

#9
pdfQ9 = pnorm(-1,0,1, lower.tail = FALSE)
round(pdfQ9 ,digits = 3)

#10 

pdfQ10 =pnorm(1,0,1) - pnorm(-1,0,1)
round(pdfQ10 ,digits = 3)

#10
pdfQ7 = pnorm(-1,0,1)
round(pdfQ7 ,digits = 3)

# Q7
# cdf
Q7 = pbinom( 4 , size = N_Size , prob=Prob, )
round(Q7 ,digits = 3)

# Q8
# cdf
Q8 = pbinom( 4 , size = N_Size , prob=Prob, lower.tail = FALSE)
round(Q8 ,digits = 3)


# Q9
# cdf
Q9 = Q8 + dbinom( 4 , size = N_Size , prob=Prob)
round(Q9 ,digits = 3)

# Q10
# 5 down, 4down 1 up, 3down 2 up no!!!
Q10 = pbinom(1 , size = N_Size , prob=Prob )
round(Q10 ,digits = 3)


