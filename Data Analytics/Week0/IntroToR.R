# BU510.650 Data Analytics Session 0
# Introduction to R
#######################################
# Try a few commands to interact with the command line
# plot 1000 randomly generated numbers from the standard normal distribution
plot(rnorm(1000))
# Assign value 5 to variable a and value 10 to variable b and calculate a + b - a * b  
a = 5
b <- 10
a + b - a * b 
# To display the names of "objects" currently stored in your R session, try objects() or ls()
objects()
ls()
# To remove objects a and b, try rm(a,b)
rm(a,b)
ls()
#######################################
# If you know the name of the function or object for which you want help, for example, the function "read.csv", use help() with the function name in parentheses
help("read.csv")
?read.csv
#######################################
# When you want to assign a sequence of values to a vector, put the sequence of values in c() function
x <- c(1,2,3)
c(1,2,3) -> x
y <- c(x,2,x)
z <- c(1e3,100)
# You can use seq() or rep() functions to generate regularly spaced valued, for example, see what you get for the vector x when you make the following assignments
seq(-5, 5, by=1) -> x
x <- seq(length=10, from=-5, by=.5)
x <- rep(x, times=5)
x <- rep(x, each=5)
x

# Set the vector x = (1,.,6) and y = 2, and multiply x by y.
x = 1:5
y = 2
x * y
# Set the vector y = (1,10) and multiply x=1(1,.,6) by y and divide x by y. Pay attention to how uses the shorter vector, y.
y = c(1,10)
x * y
x / y
# You can also assign the value NA (missing value)
z <- c(1:5,NA)
z
# Compare how R treats NULL to how it treats NA, NULL is ignored, NA is registered. 
z <- c(1,NULL,3)
z
#######################################
# Next, we practice with logical vector operations. First, set x = (1,..,6)
x = 1:6
# test whether x > 3, which creates a logical vector
x > 3
# test whether x is not equal to 3 ("!=" denotes not equal in R syntax)
x != 3
# test whether x is equal to 3 ("==" denotes equal in R syntax)
x == 3
# We can also assign the logical vector resulting from a test to a variable, for example:
y = x > 3
y
# Next we give examples of how R indexes vectors (and other objects) to refer to specific elements. To start, create a vector x = (0, 0.2., 0.4,.,2)
x = seq(0,2,.2)
x
# Here is how we refer to the 4th element of x
x[4]
# create a new vector by picking x's 4th element onward
y = x[4:length(x)]
# the index could itself be a vector, for example, create a new vector by picking 5th, 6th and 8th elements of x
x[c(5,6,8)]
# create a new vector by picking 2nd through 5th elements of x
z = 2:5
x[z]
# We can also pick the elements of a vector that satisfies a certain logic condition. We give examples below. First, set x = (1,.,10) 
x = 1:10 
# try summary(x) to see the information it summarizes about the vector
summary(x) 
# generate logical vector y by setting y = x > 5. try summary(y) to see the information it summarizes about the vector y
y = x > 5
summary(y) 
# suppose we wanted to pick the elements of x that are greater than 5. Here is how we could do it.
x[y]
# Another way to do the same thing:
x[x>5]
# suppose we wanted to pick the elements of x that are not greater than 5. Here is how we could do it. "!" is the "not" logical operator in R.
x[!y]
#######################################
# Next we give examples of factors, which hold categorical variables. For example, in a project, maybe we are comparing countries statistically, and country would be a categorical variable, whose values would be held in a factor
# create a vector of character strings
country <- c("US", "UK", "China", "India", "Japan", "Korea")
# Currently, "country" is just a vector of text. Let's convert the vector "country" to a factor. Print both countryf and country on screen and see how they compare.  
countryf <- factor(country)
countryf
country
# try converting the factor countryf to numeric vector
as.numeric(countryf)
# try converting the vector country to numeric vector and see how the result compares to the previous one
as.numeric(country)
#######################################
# Next we give examples of matrices and data frames. A matrix is a rectangular array. It can be viewed as a collection of column vectors all of the same length and the same type (i.e., numeric, character, or logical). A data frame is also a rectangular array. All columns must be the same length, but they may be of different types. 
# generate a vector of numbers 1 through 6
aa = 1:6
# store the contents of aa in a 2 by 3 matrix and print aa on screen to see the output
dim(aa) <- c(2,3)
aa
# generate two vectors, a = (1,.,5) and b is 5 randomly generated numbers from the standard normal distribution.
a <- 1:5
b <- rnorm(5)
# generate a matrix, named c.matrix, by binding a and b together as columns
c.matrix <- cbind(a,b)
c.matrix

# display the row names and column names of the matrix c.matrix
rownames(c.matrix)
colnames(c.matrix)
# display the element in the fourth row, second column of c.matrix
c.matrix[4,2]
# display the first row of c.matrix
c.matrix[1,]
# display the second column of c.matrix
c.matrix[,2]
# Here is another example of matrix operations. Generate six random numbers from the standard normal distribution and store them in a matrix with three rows
m.normal = matrix(rnorm(6),nrow=3)
m.normal
# Obtain matrix m2 by multiplying each element of m.normal by 10 and print the matrix m2 on screen
m2 = m.normal *10
m2 
# add 50 to each element in the second column of m2
m2[,2] = m2[,2]+50
m2[,2]
# use summary() function to summarize the information in matrix m2
summary(m2)
#######################################
# Next we go through a few operations to observe the difference between a matrix and a data frame. First, generate a vector x of numbers 1 through 10 and a vector y with 10 random numbers from the standard normal distribution
x = 1:10
y = rnorm(10)
# form matrix mat by binding the two vectors as columns
mat <- cbind(x,y)
# display the class of the first column of mat - you will see that it is "numeric"
class(mat[,1])
# form vector z with elements "a1","a2",., "a10" by using the "paste()" function
z = paste0("a", 1:10)
# form a matrix called tab by binding x,y,z as columns
tab <- cbind(x,y,z)
# display the class of tab - it should be "matrix"
class(tab)
# display the class of the first column of tab, which is the vector x - you will see that it is "character". It used to be "numeric" but when we combined x and y with "character" vector z to form a matrix, R forced x and y to become "character" vectors also, because a matrix must have vectors of the same type.
class(tab[,1])
# now generate a data frame called tab, by using x, y, z as columns again
tab <- data.frame(x,y,z)
# display the class of tab - you should see "data.frame"
class(tab)
# display the first six rows of the data frame called tab - head() function does that
head(tab)
# display the class (or mode) of the first column of tab, which is vector - you should see "numeric". Notice that R did not force x to become a character vector when we combined x, y, and z to form a data.frame.
mode(tab[,1])
# display the row names of the data frame
rownames(tab) 
# change the row names so they are called row1, row2, etc.  
rownames(tab) <- paste0("row", 1:10)
#######################################
# Next we give more examples of data.frame operations. To refer to a column of a data frame, you can use the $ operator. For example, to refer to column x of data frame tab, we write:
tab$x
# we can change the column names of a data frame.
# display the column names of tab
colnames(tab)
# change the column names to "a", "b", "c"
colnames(tab) <- c("a","b","c")
# change the column names to "col1", "col2", "col3"
colnames(tab) <- paste0("col",1:3)
# a "list" is a collection of objects that may have the same or different types. We create a list below
x = list(1, "y",c(2,4,6))
# check the length of x - you will see that it consists of 3 objects
length(x) 
# check the class of x - it should be "list"
class(x)
# here is how we refer to the second element of the list
x[[2]]
# a data.frame is a special list, whose column vectors are matched. Check if the data.frame tab, which we created earlier, is a list. We use the is.list logical operator, which should return TRUE.
is.list(tab)
# print the second element of the list tab, which is a vector
tab[[2]]
#######################################
# Next we review some basic plotting functions
# generate 50 random numbers from the standard normal distribution
x = rnorm(50)
# generate a sequence of 50 numbers evenly spread between 0 and 100 
y = seq(from=0, to=100, length.out=50)
# plot x against y
plot(x, y, xlab="x normal random", ylab="y sequence", main="plot test", pch=5, col=4) 
# to make a plot directly to a file, we can use functions png(), postscript, etc. For example, the following comments together will create a plot of y versus x and save it in file myplot.png
png("my_plot.png", width=480, height=360)
plot(x, y, xlab="x normal random", ylab="y sequence", main="plot test", pch=5, col=4) 
# R can have multiple graphic "devices" open - the window in your R console, the window in your R Studio, .png file, etc. To see a list of active devices: 
dev.list()
# To close the most recent device: 
dev.off()
# To close device 3
dev.off(3)
# To use device 2
dev.set(2) 
#######################################
# here is some basic information about reading data. When we use function read.table(.), the data in "." is stored  as a data frame. Use fix() to view the data in a spreadsheet-like window. Use function read.cvs("filename.cvs") to read data into R from the file "filename.cvs" Setting header = TRUE tells R that the first line contains the variable names. Setting na.strings="." tells R that whenever it sees ".", it should be treated as a missing element. For example, the following command will read the file Auto.csv into data.frame Auto, treating ?s in the file as missing elements.
Auto = read.csv("Auto.csv", header=TRUE, na.strings="?")
# We can also read data from the internet, for example, the following commands will read the .csv file in the supplied url into a data.frame called tomato
theurl <- "http://www.jaredlander.com/data/Tomato%20First.csv"
tomato <- read.table(file = theurl, header=TRUE, sep=",")
#######################################
# Next we review what R can do with probability distributions like normal or Uniform. Each distribution has a name in R: Normal is norm, Uniform is unif, Binomial is binom, Poisson is pois, Student's is t, F is f, and Ch-squared is chisq. The prefix d followed by the distribution name computes density. The prefix p computes the cumulative distribution function, the prefix q computes the quantile, and the prefix r generates random variables. We give examples for Uniform below. 
# For a random variable that is uniformly distributed between 5 and 15, compute the density at x = 8 
dunif(8, min=5, max=15)
# For a random variable that is uniformly distributed between 5 and 15, compute the cumulative distribution function at x = 8 (i.e., the probability that the random variable's value is 8 or less)
punif(10, min=5, max=15)
# For a random variable that is uniformly distributed between 5 and 15, compute the 0.8 quantile, i.e., find x such that 80% of the distribution lies below it
qunif(.8, min=5, max=15)
# Generate 10 values drawn at random from a uniform distribution between 5 and 15
runif(10, min=5, max=15)
#######################################
# The next example shows the role of function set.seed(): Setting a seed ensures reproducible results from random processes in R. For example, set the seed to 5 and generate 3 random values from the normal distribution with mean = 10 and standard deviation = 20: 
set.seed(5)
rnorm(3, mean=10, sd=20)
# generate another 3 numbers from the same distribution, without setting a seed. Are they the same numbers as you generated earlier?
rnorm(3, mean=10, sd=20)
# now set the seed to 5 and generate 3 numbers from the same distribution again. Are they the same numbers as you generated above?
set.seed(5)
rnorm(3, mean=10, sd=20)
#######################################
# The next example is about the sample function
# Use sample function to pick five numbers at random from the set {1, ., 40}
sample(1:40, 5)
# By default, sample picks random elements without replacement, i.e., no number appears twice. If you want to allow replacement, here is an example:
#generate ten flips of a coin, with H for heads and T for tails
sample(c("H","T"), 10, replace=TRUE)
# What if we want to sample outcomes with different probabilities. For example, generate ten random occurrences of "success" and "failure" with success having 80% probability and failure having 20% probability
sample(c("success","failure"), 10, replace=TRUE, prob=c(0.8,0.2))
#######################################
# we can also define our own functions in R, using function(). For example, define a function g(x) = 3*x^-4 and see what it returns for x = 2
g <- function(x){3*x^(-4)}
g(2)



