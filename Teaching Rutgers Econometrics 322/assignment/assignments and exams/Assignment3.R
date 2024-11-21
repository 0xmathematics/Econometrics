########################
# Assignment2
########################

# Normal
pdfQ2 = dnorm(3,0,1)
round(pdfQ2 ,digits = 3)

cdfQ3 = pnorm(3,0,1)
round(cdfQ3 ,digits = 3)

#6
pdfQ6 = pnorm(1,0,1)
round(pdfQ6 ,digits = 3)

#7
pdfQ7 = pnorm(-1,0,1)
round(pdfQ7 ,digits = 3)


#8
pdfQ8 = pnorm(1,0,1, lower.tail = F)
round(pdfQ8 ,digits = 3)

#9
pdfQ9 = pnorm(-1,0,1, lower.tail = F)
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


