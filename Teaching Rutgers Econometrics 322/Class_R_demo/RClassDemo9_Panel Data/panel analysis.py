# -*- coding: utf-8 -*-
"""
Created on Wed Mar 31 17:13:03 2021

@author: Hang
"""
#%%

filename = 'TestFatal_BearTax.csv'
df = pd.read_csv(filename)
print(df.head() )

#%%
from tqdm import tqdm
import pandas as pd
import numpy as np
import os


# get the current 
print( os.getcwd() )
#change working directory

#wd = 'C:/Users/Hahn/Desktop/code/python/Crypto Data/panel analysis'
#os.chdir(wd)

# load csv as df
filename = 'TestFatal_BearTax.csv'
df = pd.read_csv(filename)
print(df.head() )



from math import *
from scipy.sparse import diags # tridiagonal
from numpy.linalg import inv # inverse
from numpy.linalg import det #determinant
from numpy.linalg import solve # solve linear system
from scipy.linalg import lu # LU decomposition
from numpy.linalg import eig # Eigen decompostion
import matplotlib.pyplot as plt


# pooled OLS package
n = df.shape[0]
p = 1
y = np.array( df['fatal_rate'] ).reshape(n,1)
intercept = np.ones(n).reshape(n,1)
regressor = np.array(df['beertax']).reshape(n,p)
X = np.concatenate((intercept, regressor),axis=1) # 0 rows 1 columns
#coefficient
β = inv(X.T.dot(X)).dot(X.T).dot(y)
#predicted
y_hat = X.dot(β)
#residual
u_hat = y_hat - y
#SSR
SSR = np.sum( u_hat.T.dot(u_hat) )
#sigma^2
sigma_2_hat = SSR/(n-p-1)

####################################################
# Homo Cov
VarCov_Homo = sigma_2_hat * inv( X.T.dot(X)  )
# Var betaOLS
VarBeta_homo = np.diag(VarCov_Homo)
# sd betaOLS
sdBeta_homo = np.sqrt(VarBeta_homo)

####################################################
# Hetero Cov
sqaured_residual = u_hat * u_hat
sqaured_residual = sqaured_residual.reshape((n,))
Lambda_hat = np.diag(sqaured_residual)
VarCov_HR = inv( X.T.dot(X) ).dot( X.T).dot( Lambda_hat).dot( X).dot( inv( X.T.dot(X) ) )
VarBeta_HR = np.diag(VarCov_HR)
sdBeta_HR = np.sqrt(VarBeta_HR)



# print(β)
# print(y_hat)
# print(u_hat)
# print(sigma_2_hat )
# print( VarCov_Homo)
# print( VarBeta_homo)
# print( sdBeta_homo)
# print( sdBeta_HR)

#%%
####################################################
# Clustered Hetero Cov
df['u_hat'] = u_hat
num_time =  df['u_hat'].groupby(level="state").count() 
num_entity = len( df.index.unique(level = 'state') )
filter_matrix = np.kron( np.eye(num_entity), np.ones((7,7)))
u_var_cov = u_hat.dot( u_hat.T )
Lambda_hat = u_var_cov * filter_matrix
VarCov_HR = inv( X.T.dot(X) ).dot( X.T).dot( Lambda_hat).dot( X).dot( inv( X.T.dot(X) ) )
VarBeta_HR = np.diag(VarCov_HR)
sdBeta_HR = np.sqrt(VarBeta_HR)
print(VarCov_HR )
print(sdBeta_HR )
#%%
print( df.head())
df['u_hat'] = u_hat




def fun_varcov_u(x):
   return x.dot(x.T) 


print( df.head())
print(df['u_hat'].groupby(level="state").apply() )
#%%
########################
# block matrix
########################

from scipy.linalg import block_diag

A = np.identity(2)
B = [[3, 4, 5],
     [6, 7, 8]]
C = block_diag(A, B)
print( A)
print( B)
print( C )
print( type(A ))
print( type(B ))
print( type(C ))

#%%



print(df.head())


print(Lambda_hat )

#print( u_var_cov )
#print( filter_matrix.shape)

#df['u_hat'].reshape( )

#print( np.array(df['u_hat']).reshape(len(df['u_hat']),1) )
#print( len( df['u_hat']) )
#u_clustered_var_cov = df['u_hat'].groupby(level="state").apply( fun_varcov_u )
#%%
from scipy.linalg import block_diag
## compute the uu' matrix for each entity group
def fun_varcov_u(x):
   n = len(x)
   x = np.array( x).reshape(n,1)
   # vec * vec.T
   return x.dot(x.T)

u_clustered_var_cov = df['u_hat'].groupby(level="state").apply( fun_varcov_u )
u_clustered_var_cov_diag_matrix = block_diag(u_clustered_var_cov)
print( u_clustered_var_cov.values )
print( u_clustered_var_cov )
print( u_clustered_var_cov_diag_matrix)
#print(u_clustered_var_cov[0] )


'''
a = np.arange(5)
print(a)
print( fun_varcov_u(a))
'''
#%%

from linearmodels import PooledOLS
# Set multi index
df.set_index(['state','year'], drop=True, inplace=True)
df['intercept'] = 1
mod = PooledOLS(df['fatal_rate'],df[['intercept','beertax']])


#‘unadjusted’, 'homoskedastic' - Assume residual are homoskedastic
#‘robust’, ‘heteroskedastic’ - Control for heteroskedasticity using White’s estimator
# ‘clustered`
# 'homoskedastic'
res_homo = mod.fit(cov_type= 'homoskedastic' , cluster_entity=True)

# 'heteroskedastic'  'robust'
res_hetero = mod.fit(cov_type= 'robust' , cluster_entity=True)
#print(df.head())

#%%
#print(df.head())
# compare homo
print(res_homo)
print(β)
print( sdBeta_homo )

# compare hetero
print( res_hetero )
print( sdBeta_HR )

#%%
I4 = np.identity(4)
print(I4)
d = np.diag(I4)
print(d)
I4_copy = np.diag(d)
print(I4_copy)
print(d*d )
d_vec = d.reshape([4,1])
print( d_vec)
print( d_vec*d_vec)
#%%
print( d_vec)
d_vec = d_vec.reshape((4,))
print( d_vec)
#print(y_hat)
#print(u_hat)








#%%
########################################
# Appendix
########################################

########################################
# OLS Estimation
########################################
import numpy as np
from math import *
from scipy.sparse import diags # tridiagonal
from numpy.linalg import inv # inverse
from numpy.linalg import det #determinant
from numpy.linalg import solve # solve linear system
from scipy.linalg import lu # LU decomposition
from numpy.linalg import eig # Eigen decompostion
import matplotlib.pyplot as plt

np.set_printoptions(precision=4,suppress=True,threshold=5)

c1 = np.ones(4).reshape(4,1)
c2 = np.array(np.arange(4)).reshape(4,1)
X = np.concatenate((c1, c2),axis=1) # 0 rows 1 columns
print(X)
y = np.array([4,1,0,1]).reshape(4,1)
print(y)

β = inv(X.T.dot(X)).dot(X.T).dot(y)
print(β)
y_hat = X.dot(β)
print(y_hat)
u_hat = y_hat - y
print(u_hat)
#%%

########################################
# Visualization
########################################
#setting for 
%matplotlib inline
import matplotlib.pyplot as plt
#plt.rcParams['axes.labelsize'] = 20
from math import *
n = 4
plt.figure(1,figsize=(30,15),dpi=40)
#plt.figure()
x = np.arange(n)

plt.plot(x,y_hat)
plt.scatter(x, y, c = 'red',s = 300, marker = 'x')
plt.scatter(x, y_hat, c = 'green',s = 300, marker = '*')
plt.xlim(-0.1, 3.1)    # setting x-axis limits
plt.ylim(-0.1, 4.1)  # setting y-axis limits

plt.xlabel('x')
plt.ylabel('Y')
plt.title('OLS')
plt.legend(["OLS Regression Line","Original Data","Predicted Value" ], loc ="upper right") 
plt.show()
#%%