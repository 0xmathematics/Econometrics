########################################################
# Empirical Analysis: LLN 
########################################################
# Hang Miao
# Non distributable, for 322 Econometrics Course only


##########################################
# Law of Large number
##########################################

################################
# Normal Underlying Population
################################
# The underlying population follows Normal(mu, sigma^2)
N = 10000000
mu = 5
sigma = 4
# randomly generate N samples from the underlying population
X = rnorm(N, mean = mu, sd = sigma)

E = mu # Theoretical Expectation for Normal(mu, sigma^2)
Var = sigma^2 # Theoretical Expectation for Normal(mu, sigma^2)
sd = sigma # Theoretical standard deviation


# Plot the histogram of N samples 
# randomly draw from the underlying population distribution Normal(mu, sigma^2)


hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Normal(mu, sigma^2)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# suppose we only use a small propotion of the total sample to estimate the population mean of the 
# underlying population mu. The estimator is the sample mean: 1\n sum x_i
# We claim that as the size of sample increase, the sample mean converge to the population mean

# the following is the sample size vector contains 24 candidiate sample size
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(mu-sigma,mu+sigma), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = mu, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )




################################
# Binomial Underlying Population
################################


# The underlying population follows Binomial(n,p)
N = 10000000
n_Trials = 40
p = 0.2
# randomly generate N samples from the underlying population
X = rbinom(n=N, size = n_Trials, prob=p)

E = n_Trials*p # Theoretical Expectation for Binomial Distribution
Var = n_Trials*p*(1-p) # Theoretical Expectation for Binomial Distribution
sd = as.integer(sqrt(Var)) #  Theoretical standard deviation round to the nearest integer
# Plot the histogram of N samples 
# randomly draw  from the underlying population distribution Normal(mu, sigma^2)
hist(X, 
     col = "steelblue" , 
     prob = FALSE,
     breaks = seq(0,n_Trials,1),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Binomial(n,p)
x = seq(0,n_Trials,1)
lines(x,dbinom(x,n_Trials,p)* N ,col="red")
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# suppose we only use a small propotion of the total sample to estimate the population mean of the 
# underlying population which is n_Trials*p=40*0.2=8. The estimator is the sample mean: 1\n sum x_i
# We claim that as the size of sample increase, the sample mean converge to the population mean

# the following is the sample size vector contains 24 candidiate sample size
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )




################################
# Chi Square Distribution
################################

# The underlying population follows Chi(df)
N = 10000000
df = 5
# randomly generate N samples from the underlying population
X = rchisq(N, df, ncp = 0)
E = df# Theoretical Expectation for  Chi Square Distribution
Var =2*df # Theoretical Expectation for Binomial Distribution
sd = as.integer(sqrt(Var)) # Theoretical standard deviation

# draw the histogram of N samples randomly draw 
# from the underlying population distribution Chi(df)
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Chi(df)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dchisq(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# suppose we only use a small propotion of the total sample to estimate the population mean of the 
# underlying population mu. The estimator is the sample mean: 1\n sum x_i
# We claim that as the size of sample increase, the sample mean converge to the population mean

# the following is the sample size vector contains 24 candidiate sample size
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

################################
# t Distribution
################################

# The underlying population follows t-distribution(df)
N = 10000000
df = 4 # df should be great than 2, otherwise theoretical variance does not exist
# randomly generate N samples from the underlying population
X = rt(N, df, ncp = 0)
E = 0# Theoretical Expectation for  t Distribution
Var = ifelse(df > 2, df/(df-2), Inf)  # Theoretical Expectation for Binomial Distribution
sd = sqrt(Var) # Theoretical standard deviation

# draw the histogram of N samples randomly draw 
# from the underlying population distribution t(df)
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlim= c(-10,10), 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution t(df)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dt(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# suppose we only use a small propotion of the total sample to estimate the population mean of the 
# underlying population mu. The estimator is the sample mean: 1\n sum x_i
# We claim that as the size of sample increase, the sample mean converge to the population mean

# the following is the sample size vector contains 24 candidiate sample size
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

################################
# F Distribution
################################

# The underlying population follows f(df1, df2)
#df1 = 9 
#df2 = 7
# 8,6
N = 10000000
df1 = 5
df2 = 5 # df2 should be great 2 otherwise theoretical expectation does not exist
        # if df2 less than 5, theoretical variance does not exist
# randomly generate N samples from the underlying population
X = rf(N, df1, df2)
E = df2/(df2-2)# Theoretical Expectation for  f  Distribution
Var =2*df2^2*(df1+df2-2)/(df1*(df2-2)^2*(df2-4)) # Theoretical Expectation for Binomial Distribution
sd = sqrt(Var) # Theoretical standard deviation

# draw the histogram of N samples randomly draw 
# from the underlying population distribution f(df1, df2)
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 5000, 
     xlim= c(0,20),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution f(df1, df2)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(df(x,df1,df2), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# suppose we only use a small propotion of the total sample to estimate the population mean of the 
# underlying population mu. The estimator is the sample mean: 1\n sum x_i
# We claim that as the size of sample increase, the sample mean converge to the population mean

# the following is the sample size vector contains 24 candidiate sample size
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

################################
# F Distribution
################################

