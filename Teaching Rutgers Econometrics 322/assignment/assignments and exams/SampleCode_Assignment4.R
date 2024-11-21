############################################################################
# Sample Code for Loading the .csv File
############################################################################

# In class, we know how to load data from excel file.
# In practice, most of the dataset are stored in csv file, 
# the reason is the .csv data is more compact and take less space.
# For the Assignment and Exams, the data are store in the .csv file. 
# you could use the following code to load the datasets

# follow the same step as loading excel file:
# 1.put the data and this R script together within the same folder
# 2. set the working directory same as "the source file location"
# 3. use the embeded function "read.csv()" to read csv file, 
# no need to install any third party package, since most of the dataset are in this format
# format: df <- read.csv("Fill Name with .extention")


# Example for loading the dataset "Dataset_Assignment4.csv". 
# The data file name is "Dataset_Assignment4.csv", so we just replece the fill name
df <- read.csv("Dataset_Assignment4.csv")  
head(df)

# df has the dataframe data structure, you could use the following command to extract the data vector you need
x = df[,1]
x


