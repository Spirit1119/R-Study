#######################################################
############# Example: Toyota Used-Car Prices
#######################################################

## TASK 1 ##

# read the data in file ToyotaCorolla.csv into a data.frame called toyota
toyota<-read.csv("ToyotaCorolla.csv")

# view the first six rows 
head(toyota)

# view summary information about the data
summary(toyota)

## TASK 2 ##

# create a new column in toyota, called CNGFUel, such that it is 1 if the car's fuel type is CNG, 0 otherwise
toyota$CNGFuel=ifelse(toyota$FuelType=="CNG",1,0)

# create a new column in toyota, called DieselFuel, such that it is 1 if the car's fuel type is Diesel, 0 otherwise
toyota$DieselFuel=ifelse(toyota$FuelType=="Diesel",1,0)

# let us view the first six rows to see if what we did above seems to be working
head(toyota)

# now that we created those indicator variables for CNG and Diesel, we can remove the old FuelType column
toyota=toyota[,-4]

# let us view the first six rows of the updated toyota
head(toyota)

## TASK 3 ##

# we create plots of price versus several variables to see how the variable affects price
plot(Price~Age,data=toyota)
plot(Price~KM,data=toyota)
plot(Price~HP,data=toyota)
plot(Price~MetColor,data=toyota)
plot(Price~Automatic,data=toyota)
plot(Price~CC,data=toyota)
plot(Price~Doors,data=toyota)
plot(Price~Weight,data=toyota)

## TASK 4 ##

# run a regression including all input variables
m1=lm(Price~.,data=toyota) 
summary(m1)


## TASK 5 ##

# run a regression after excluding one or more variables

# for example, it might be a good idea to exclude MetColor (why?)
m2=lm(Price~.-MetColor,data=toyota) 
summary(m2)

# which other variables would you suggest excluding -- run regression again after excluding other less influential variables

## TASK 6 ##

# To check the effect of age^2 and km^2 on price, we will add columns that hold the values for age^2 and km^2

# add a new column, called AgeSq, which will hold the value of age^2 for each car
toyota$AgeSq = toyota$Age^2

# add a new column, called KMSq, which will hold the value of KM^2 for each car
toyota$KMSq = toyota$KM^2

# check the first six rows of toyota to see if the changes worked
head(toyota)

## TASK 7 ##

# run a regression with Age and KM only
m10 = lm(Price~Age+KM, data=toyota)
summary(m10)

# run a regression including AgeSq as well
m11 = lm(Price~Age+KM+AgeSq, data=toyota)
summary(m11)

# run a regression including KMSq as well
m12 = lm(Price~Age+KM+AgeSq+KMSq, data=toyota)
summary(m12)

# run a regression after excluding KM 
m13 = lm(Price~Age+AgeSq+KMSq, data=toyota)
summary(m13)

## TASK 8 ##

# run the regression with respect to Age and KM
m10 = lm(Price~Age+KM, data=toyota)
summary(m10)

# the following command will plot the residuals for all the cars in our data set
# res is for residual part of data (residual = actual - predicted) 
plot(m10$res) 

# the following command will put on the x-axis the fitted price (our predicted price) for each car
# in the data set versus the residual for that car on the y-axis 
plot(m10$res ~ m10$fitted) 
