## Rcode for HW15
## Hang Miao

#Q2
Var_Y = 16.71
Q2 = c(15.77/Var_Y,13.78/Var_Y,12.08/Var_Y,11.14/Var_Y)
round(Q2,digits = 2)

#Q3
beta1 = 0.95
sd = 0.02
crit = qnorm(0.975)
left = beta1 -crit*sd
right = beta1 + crit*sd
CI_95 = c(left, right)
round(CI_95,digits = 2)






