
#########################################################################################################
# Matrix Operation 
###########################################################

###review of matrix algebra
# creat matrix
M1 = matrix( 1:10, nrow = 5, ncol = 2, byrow = TRUE )
M1
M1 = matrix( 1:10, nrow = 5, ncol = 2, byrow = FALSE )
M1

# an equivalent way to creat the same matrix above is replace 1:10 
# by the explicitely write out each element in a vector format c(1,2,3,4,5,6,7,8,9,10)
M1_ = matrix( c(1,2,3,4,5,6,7,8,9,10) , nrow = 5, ncol = 2, byrow = TRUE)
M1_

M2 = matrix(rep(1,times = 10), nrow = 5, ncol = 2, byrow = TRUE)
M2

M1 + M2

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
M1 %*% t(M2)

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

M1 %*% c(1,2)

### creat a diagnol matrix
M5 = diag(c(1,2,3,4,5))
M5
### creat an identity matrix diagnol matrix with 5 by 5 dimension
M6 = diag(5)
M6
### Matrix Inversion
M5inv = solve(M5)
M5inv
M6inv = solve(M6)
M6inv

M5 %*% M5inv

