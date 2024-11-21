############################################################################
# Midterm Exam
############################################################################


# you could use the following code to load the each of the dataset for the midterm exam
# df <- read.csv("Fill Name with .extention")


# Example for loading the dataset for question 11-22. 
# The data file name is "Dataset11_23.csv", so we just replece the fill name
df <- read.csv("Dataset11_23.csv")  
head(df)

# df has the dataframe data structure, you could use the following command to extract the data vector you need
x = df[,1]


