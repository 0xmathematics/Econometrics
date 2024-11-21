############################################################################
# Final Exam
############################################################################


# you could use the following code to load the each of the dataset for the final exam
# df <- read.csv("Fill Name with .extention")


# Example for loading the dataset for question 11-22. 
# The data file name is "Dataset11_23.csv", so we just replece the fill name
df <- read.csv("Dataset11_23.csv")  
head(df)

# df has the dataframe data structure, you could use the following command to extract the data vector you need
x = df[,1]


# you may use the following code to create a matrix, for detailed information please check demo2
M1 = matrix( 1:10, nrow = 5, ncol = 2, byrow = TRUE)
M1
# an equivalent way to creat the same matrix above is replace 1:10 
# by the explicitely write out each element in a vector format c(1,2,3,4,5,6,7,8,9,10)
M2 = matrix( c(1,2,3,4,5,6,7,8,9,10) , nrow = 5, ncol = 2, byrow = TRUE)
M2
# run the code you will find M1 == M2