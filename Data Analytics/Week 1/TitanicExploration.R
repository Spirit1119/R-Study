# BU510.650 Data Analytics Week 1
# Exploring data using R
# Example: Titanic

# Set working directory: During the current session, R will use this as the default location when 
# looking for input files and creating output files. 
# To set working directory: In R Studio, click Session -> Set Working Directory -> Choose Directory. 
# You could also do this by directly typing a command. 
# For example, I keep my R files for this class in a folder called R-Work under the C: folder.
# To set the folder C:/R-Work as my working directory, I would use the following command:  
# setwd("C:/R-Work")

# What you see in this pane is an R Script, or the source code.
# If you wish to start a new R Script in R Studio, 
# click the new document button in the top-left corner and select "R Script".
# You can create your own R Script in this pane (for example, for your homework) and 
# when you save it, your code will be saved as a .R file

# Next we want R to read the data set we want to work with. 
# Our data is in the file titanic.csv.
# In R Studio, you can click File -> Import Dataset -> From CSV 
# Browse to titanic.csv and select it. Click "Import". 
# R will now store the data in titanic.csv in a data frame also called titanic
# Again, you can do this by directly typing a command. For example, to read titanic.csv, 
# which I saved in my working directory (in my case, the folder R-Work under C), I would simply use:
titanic <- read.csv("titanic.csv")

# When R reads a .csv file, it stores the data into an object type called a "data frame." 
# the above command stored the contents of titanic.csv in a data frame called titanic.
# To view the data frame as a spreadsheet:
View(titanic)

# Let's check what types of variables we have in titanic. We use "str" command.
str(titanic)

# Let's check what values are stored in the Survived column. 
# We can use "titanic$Survived" to refer to the "Survived" column of "titanic" data frame. 
titanic$Survived

# Let's summarize the information in the Survived column. 
# The "table" function goes through a column and counts the occurrence of each value 
# In this case how many times 0s and 1s occurred in the "Survived" column, 
# with 0 corresponding to a passenger who died and 1 corresponding to a passenger who survived.
table(titanic$Survived)

# The function prop.table will convert the numbers in the table to proportions.
prop.table(table(titanic$Survived))

# The function "summary" summarizes the information contained in this argument.
# Let us summarize the information in the Gender column, which shows each passenger's gender.
# Use 0-1 distribution, it will come out the counts of each item.
summary(titanic$Gender)

# We can also create a multi dimensional table, say a table that distributes the passengers according to 
# their gender and whether they survived or not.
table(titanic$Gender, titanic$Survived)

# Let's convert the numbers into proportions. 2*2
prop.table(table(titanic$Gender, titanic$Survived))

# Let's use prop.table so that the proportions add up to 100% in each row. female & male horizontal sum is 1.
prop.table(table(titanic$Gender, titanic$Survived),1)
# survive & die vertical sum is 1.
prop.table(table(titanic$Gender, titanic$Survived),2)

# Now, let us summarize the information in the Age column. Multiple data in the information will bring statistics of the colomn.
summary(titanic$Age)

# We will add another column to our data frame. 
# This column will be titled "Child" with all 0.
# It will be equal to 1 if the passenger is less than 18 years old and 1 otherwise.
# The selected steps can be done at once.
titanic$Child=0
titanic$Child[titanic$Age<18]=1

# We will use "aggregate" function to count how many passengers survived, 
# grouping the passengers according to their gender and whether or not they were children.  
# The "aggregate" function divides the data into groups according to 
# the variable to the right of "~" and applies the function specified 
# in "FUN" to the variable to the left of "~". 
# In this case, it will add the numbers (0s and 1s in the Survived column) 
# for four different groups of passengers: 
# Male children, female children, male adults, female adults. numbers are the numbers of survivors.
aggregate(Survived ~ Child + Gender, data=titanic, FUN=sum)


# We will use "aggregate" function again, this time 
# to count how many passengers belong to each of the four different groups 
# (male children, female children, male adults, female adults). 
# We can achieve that by setting "FUN = length".
# 2*2male&female + adults&child. 
aggregate(Survived ~ Child + Gender, data=titanic, FUN=length)


# Finally, we will use "aggregate" function again, this time 
# to find the proportion that survived in each group. 
# Here, "FUN" is set to a function, which we define.
# prop
aggregate(Survived ~ Child + Gender, data=titanic, FUN=function(x) {sum(x)/length(x)})

# Next we add a new column to our data frame, called "Fare2", 
# to indicate how expensive the fare was for each passenger. 
# We will group them into four: those who paid $30+, between 20 and 30, 
# between 10 and 20, and less than 10.
titanic$Fare2<-"30+"
titanic$Fare2[titanic$Fare < 30 & titanic$Fare >= 20] <- "20-30"
titanic$Fare2[titanic$Fare < 20 & titanic$Fare >= 10] <- "10-20"
titanic$Fare2[titanic$Fare < 10] <- "<10"

# Finally, we check how many passengers survived when we group them 
# according to their fare category, travel class, and gender. 
# We use the "aggregate" function again.
aggregate(Survived ~ Fare2 + Pclass + Gender, data=titanic, FUN=function(x) {sum(x)/length(x)})

# Search for the passenger information named as Rose. == means exactly the same. = means equal.
titanic[titanic$Name=="Rose",]

