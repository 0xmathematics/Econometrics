## Rcode for HW6
## Hang Miao

##############################################################################
#question 6.3
##############################################################################

 
 ##column 2
  beta0_intercept = 4.53
  beta1_College = 5.64
  beta2_Female = -2.7
  beta3_Age = 0.30
    
  # Sally
    X1_College = 1
    X2_Female = 1
    X3_Age = 33
    
    Y_hat1 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age
    round(Y_hat1,digits = 2)
    
    # Betsy
    X1_College = 1
    X2_Female = 1
    X3_Age = 45
    
    Y_hat2 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age
    round(Y_hat2,digits = 2)    
    #Expected difference
    # There is an error in the grading system
    # Have to use absolute value for the difference to get credit
    abs(Y_hat1-Y_hat2)
    

##############################################################################
#Excercise 6.2
##############################################################################
    
#######################################  
## Compute the Adjusted R^2
############################################
#coulumn1
p = 3
SER = 6.33
R2 = 0.178
n = 4700
# SER^2 =  SSR/(n-p-1)
SSR = SER^2*(n-p-1)
# R2 = 1 - SSR/TSS
TSS = SSR/(1-R2)
adj_R2 = 1 - (n-1)/(n-p-1) * (SSR/TSS)
round(adj_R2,digits = 3)
 
#coulumn 2
p = 4
SER = 6.28
R2 = 0.192
n = 4700

# SER^2 =  SSR/(n-p-1)
SSR = SER^2*(n-p-1)
# R2 = 1 - SSR/TSS
TSS = SSR/(1-R2)
adj_R2 = 1 - (n-1)/(n-p-1) * (SSR/TSS)
round(adj_R2,digits = 3)

#coulumn 3
p = 7
SER = 6.27
R2 = 0.196
n = 4700

# SER^2 =  SSR/(n-p-1)
SSR = SER^2*(n-p-1)
# R2 = 1 - SSR/TSS
TSS = SSR/(1-R2)
adj_R2 = 1 - (n-1)/(n-p-1) * (SSR/TSS)
round(adj_R2,digits = 3)    
    
##############################################################################
#Excercise 6.10
##############################################################################
###first subquestion
Var_u = 1
Var_x1 = 3
n = 334
rho = 0
Var_bHat1 = 1/n*1/(1-rho^2)*Var_u/Var_x1
  
round(Var_bHat1, digits = 5)

###Second subquestion
Var_u = 1
Var_x1 = 3
n = 334
rho = 0.54
Var_bHat1 = 1/n*1/(1-rho^2)*Var_u/Var_x1
round(Var_bHat1, digits = 5)

##############################################################################
#Excercise 6.4
############################################################################
# column 3
###################
  beta0_intercept = 3.41 
  beta1_College = 4.95
  beta2_Female = -2.38
  beta3_Age = 0.26
  beta4_Northeast = 0.63
  beta5_Midwest = 0.55
  beta6_South = -0.25
  ######################   
# Juanita
X1_College = 1
X2_Female = 1
X3_Age = 32
X4_Northeast = 0
X5_Midwest = 0
X6_South = 1

Y_hat1 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age +
  beta4_Northeast*X4_Northeast +beta5_Midwest*X5_Midwest +beta6_South*X6_South

round(Y_hat1,digits = 2) 

# Jennifer
X1_College = 1
X2_Female = 1
X3_Age = 32
X4_Northeast = 0
X5_Midwest = 1
X6_South = 0

Y_hat2 = beta0_intercept + beta1_College*X1_College +beta2_Female*X2_Female + beta3_Age*X3_Age+
  beta4_Northeast*X4_Northeast +beta5_Midwest*X5_Midwest +beta6_South*X6_South
round(Y_hat2,digits = 2) 
#Expected difference
# There is an error in the grading system
# Have to use absolute value for the difference to get credit
abs(Y_hat1-Y_hat2)

