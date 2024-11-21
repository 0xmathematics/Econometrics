
## Rcode for HW9
## Hang Miao

##############################################################################
#Exercise 9.6	
############################################################################

p = 1
n=131
SER = 15.11

SSR = SER^2*(n-p-1)
newSER = sqrt( 2*SSR/(2*n-p-1)   )
round(newSER, digits = 2)

sd_beta0 = 15.3
sd_beta1 = 13.2

sd_beta0_n = sd_beta0/SER*newSER/sqrt(2)
sd_beta1_n = sd_beta1/SER*newSER/sqrt(2)

round(sd_beta0_n, digits = 2)
round(sd_beta1_n, digits = 2)