#class demo 
# Rutgers Econometrics 322
# Hang Miao


# In this Demo, I would like to to select a sample of 7 students
# from the roster (popuplation).
# The folowing Sampling techniques could give me a representive sample

########################################
#  Simple Sampling
########################################

#creat a vector contains elements from 1 to 70
dice = c(1:70)
# randomly pick one number
sample(dice,1)
# randomly pick seven number
sample(dice,7)

########################################
# Systematic Sampling
########################################
# randomly pick one number
first = sample(dice,1)
first
studentSample = c(first)

#then select the next one with 10 greater than the previous one
#provided the number is less than 70
# if exceed 70, then count from begining with mod 70.

for (i in 1:6) {
  if(first+10*i<=70){
    studentSample=append(studentSample, first+10*i )
    }
  else{
    studentSample=append(studentSample, first+10*i-70 )
  }
}
studentSample
########################################
# stratified sampling
########################################

#algorithm:
# divid in two group: male and female
# Use simple random sample to select 4 from male group and 3 from female group


########################################
#clustered sampling
########################################

#algorithm:
# randomly divide the population into ten subgroup
# randomly choose one of them

#implementation

# randomly split the population into 10 clusters 
population = sample(dice)
cluster = split(population, 1:10)
cluster
# use simple random sampling to choose one from 10 clusters
num = sample(1:10,1)
cluster[num]

