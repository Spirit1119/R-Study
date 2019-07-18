# read the data into an R data.frame called my.ad
my.ad<-read.csv("Advertising.csv")

# Before we do regression, here are few commands to review the data a bit
# let us see the first six rows of the data
head(my.ad)

# let us see the last six rows of the data
tail(my.ad)

# here is another way to see a preview of the data, V is capitalized
View(my.ad)

# the following commands will show us the names of variables, etc. in the data
names(my.ad)
summary(my.ad)

# what if we wanted to remove some of the information from the data
# the following will remove rows 2 through 5 and will assign the result to a new data.frame called my.ad2
my.ad2 <- my.ad[-(2:5),]

# the following will remove rows 2, 5, and 10 and will assign the result to a new data.frame called my.ad3
my.ad3 <- my.ad[-c(2,5,10),]

# the following will remove column 1 nd will assign the result to a new data.frame called my.ad4
my.ad4 <- my.ad[,-1] 

# check the first six rows of my.ad4
head(my.ad4)

# the command par(mfrow=c(x,y)) divides plot space into x rows and y columns, will affect next the plots
par(mfrow=c(1,1)) 
par(mfrow=c(2,2))

# plot Sales versus TV budget
plot(my.ad$TV,my.ad$Sales)
plot(my.ad$Newspaper,my.ad$Sales)


# run a regression of Sales wrt TV budget
my.lm=lm(Sales~TV,data=my.ad)
summary(my.lm)

##In-class exercise##
# plot Sales versus Newspaper budget
# run a regression of Sales wrt Newspaper budget

# Next, we run five regressions reported on Slide 28 of Session 2 notes
# Regression with TV only 
my.lm.1=lm(Sales~TV, data=my.ad)
summary(my.lm.1)

# Regression with Radio only 
my.lm.2=lm(Sales~Radio, data=my.ad)
summary(my.lm.2)

# Regression with Newspaper only 
my.lm.3=lm(Sales~Newspaper, data=my.ad)
summary(my.lm.3)

# Regression with TV and Radio 
my.lm.4=lm(Sales~TV+Radio, data=my.ad)
summary(my.lm.4)

# Regression with TV, Radio, and Newspaper 
my.lm.5=lm(Sales~TV+Radio+Newspaper, data=my.ad)
summary(my.lm.5)

# Suppose that we decided to use the model with TV and Radio for prediction
# Let us now predict the sales in a city where TV budget is $100K and Radio budget is $50K
# The following yields a prediction along with a 95% confidence interval around it
predict(lm(Sales~TV+Radio, data=my.ad), data.frame(TV=100, Radio=50), interval="confidence")

# The following yields a prediction along with a 95% prediction interval around it
predict(lm(Sales~TV+Radio, data=my.ad), data.frame(TV=100, Radio=50), interval="prediction")