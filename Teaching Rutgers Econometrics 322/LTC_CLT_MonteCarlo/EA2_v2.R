########################################################
# Empirical Analysis: CLT
########################################################
# Hang Miao
# Non distributable, for 322 Econometrics Course only


##########################################
# Central Limiit Theory
##########################################

################################
# Normal Underlying Population
################################
# The underlying population follows Normal(mu, sigma^2)
N = 10000000
mu = 20
sigma = 5
# randomly generate N samples from the underlying population
X = rnorm(N, mean = mu, sd = sigma)
E = mu
Var = sigma^2
sd = sigma

# Plot the histogram of N samples randomly draw 
# from the underlying population distribution Normal(mu, sigma^2)
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



### small Sample and Large Sample
Sample_Size = c(10, 100000 )
### number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=1
## Conduct reps (2000) times of following experiment: 
## each time draw sample_Size  many of samples from underlying population
## samples is a matrix with dimension Sample_Size*reps
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution
# Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )


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
#sd = as.integer(sqrt(Var)) # Theoretical standard deviation
sd = sqrt(Var) # Theoretical standard deviation
# draw the histogram of N samples randomly draw 
# from the underlying population distribution Normal(mu, sigma^2)
hist(X, 
     col = "steelblue" , 
     prob = FALSE,
     breaks = seq(0,n_Trials,1),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Binomial(n,p)
x = seq(0,n_Trials,1)
lines(x,dbinom(x,n_Trials,p)* N ,col="red")
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )


### small Sample vs Large Sample
Sample_Size = c(2, 100000 )
### number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=2
## Conduct reps (2000) times of following experiment: 
## each time draw sample_Size  many of samples from underlying population
## samples is a matrix with dimension Sample_Size*reps
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(0,n_Trials,1)
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution
# Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )


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
sd = sqrt(Var) # Theoretical standard deviation

# draw the histogram of N samples randomly draw 
# from the underlying population distribution Chi(df)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Chi(df)
curve(dchisq(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )



### small Sample vs Large Sample
Sample_Size = c(2, 100000 )
### number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=2
## Conduct reps (2000) times of following experiment: 
## each time draw sample_Size  many of samples from underlying population
## samples is a matrix with dimension Sample_Size*reps
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )




################################
# t Distribution
################################

# The underlying population follows t-distribution(df)
N = 10000000
df = 5 # df should be great than 2, otherwise theoretical variance does not exist
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


### small Sample vs Large Sample
Sample_Size = c(2, 100000 )
### number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=1
## Conduct reps (2000) times of following experiment: 
## each time draw sample_Size  many of samples from underlying population
## samples is a matrix with dimension Sample_Size*reps
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution
# Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

################################
# F Distribution
################################

# The underlying population follows f(df1, df2)
#df1 = 8
#df2 = 6
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


### small Sample and Large Sample
Sample_Size = c(2, 100000 )
### number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=2
## Conduct reps (2000) times of following experiment: 
## each time draw sample_Size  many of samples from underlying population
## samples is a matrix with dimension Sample_Size*reps
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution
# Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )
