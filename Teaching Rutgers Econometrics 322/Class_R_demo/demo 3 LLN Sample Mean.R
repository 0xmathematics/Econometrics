



################################
# Bernolli Underlying Population
################################










################################
# Law of Large Number
################################



# The underlying population follows Bernolli(p)
N = 1000
p = 0.5
# randomly generate N samples from the underlying population
X = rbinom(n=N, size = 1, prob=p)

E = p # Theoretical Expectation for Binomial Distribution
Var = p*(1-p) # Theoretical Expectation for Binomial Distribution
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

