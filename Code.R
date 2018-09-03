# Cleaning the workspace
rm(list = ls())

# Setting the required working directory
setwd("~/Desktop/IAS")

library(data.table)

# Getting data
data = fread('Training.csv', header = TRUE)

str(data)

# Changing data type
data$`Education Level` <- as.factor(data$`Education Level`)
data$`Age Range` <- as.factor(data$`Age Range`)
data$`Employment Status` <- as.factor(data$`Employment Status`)
data$Gender <- as.factor(data$Gender)
data$Year <- as.factor(data$Year)

# Re-checking newly-formatted data types
str(data)
#/
#/
#/
# Cleaning data Process - Step 1
summary(data)
summary(data$`Employment Status`)

# Taking a closer look at Employed individuals
subset1 <- data[data$`Employment Status` == 'Employed',]
summary(subset1)
# We see that the minimum value for weekly hours worked and weekly earnings is 0. Something is wrong here.
subset2 <- data[data$`Employment Status` == 'Unemployed',]
summary(subset2)
# Weekly hours and weekly earnings for unemployed are all 0. This is as we suspected.
subset3 <- data[data$`Employment Status` == 'Not in labor force',]
summary(subset3)
# Weekly earnings and weekly hours worked for Not in labor force are all 0. Is there a differentiating factor between subset2 and subset 3?
#/
#/
#/
# Cleaning data Process - Step 2
# Yearly total
table(data[,'Year'])
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 7999 8001 8001 8001 8001 8001 8001 8001 

#Taking a look at the individuals with status of 'Employed'
nrow(data[data$`Employment Status` == 'Employed' & data$`Weekly Earnings` == 0 & data$`Weekly Hours Worked` == 0,])
# 599 rows should be re-labelled or dropped
table(data[data$`Employment Status` == 'Employed' & data$`Weekly Earnings` == 0 & data$`Weekly Hours Worked` == 0, 'Year'])
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 51   62   53   69   81  106   87   90 

nrow(data[data$`Employment Status` == 'Employed' & data$`Weekly Earnings` > 0 & data$`Weekly Hours Worked` == 0,])
# 1533 rows should be re-labelled or dropped
# Break-down of 1533 rows by year
table(data[data$`Employment Status` == 'Employed' & data$`Weekly Earnings` > 0 & data$`Weekly Hours Worked` == 0, 'Year'])
# Results
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 125  163  160  197  229  234  218  207

nrow(data[data$`Employment Status` == 'Employed' & data$`Weekly Earnings` == 0 & data$`Weekly Hours Worked` > 0,])
# 4181 rows should be re-labelled or dropped
table(data[data$`Employment Status` == 'Employed' & data$`Weekly Earnings` == 0 & data$`Weekly Hours Worked` > 0, 'Year'])
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 612  539  549  539  496  502  444  500 

# As the dirty rows make up for less than 10% (<800) of the total observations within a year, we can drop it
# Removal of first dirty subset
newdata <- data[!(data$`Employment Status`=="Employed" & data$`Weekly Earnings` == 0 & data$`Weekly Hours Worked` == 0),]
# Removal of second dirty subset
newdata <- newdata[!(newdata$`Employment Status`=="Employed" & newdata$`Weekly Earnings` == 0 & newdata$`Weekly Hours Worked` > 0),]
# Removal of third dirty subset
newdata <- newdata[!(newdata$`Employment Status`=="Employed" & newdata$`Weekly Earnings` > 0 & newdata$`Weekly Hours Worked` == 0),]


# Finding the different yearly median for weekly earnings among Employed;
for (year in c(2005,2006,2007,2008,2009,2010,2011,2012)){
  print(paste("The median for weekly earnings", year, "is", summary(newdata[newdata$Year == year & newdata$`Employment Status` == 'Employed', 'Weekly Earnings'])[3]))
}

# [1] "The median for weekly earnings 2005 is Median : 648.0  "
# [1] "The median for weekly earnings 2006 is Median : 650.0  "
# [1] "The median for weekly earnings 2007 is Median : 692.0  "
# [1] "The median for weekly earnings 2008 is Median : 720.0  "
# [1] "The median for weekly earnings 2009 is Median : 701.0  "
# [1] "The median for weekly earnings 2010 is Median : 750.0  "
# [1] "The median for weekly earnings 2011 is Median : 750.0  "
# [1] "The median for weekly earnings 2012 is Median : 760.0  "

for (year in c(2005,2006,2007,2008,2009,2010,2011,2012)){
  print(paste("The median for weekly hours worked for", year, "is", summary(newdata[newdata$Year == year & newdata$`Employment Status` == 'Employed', 'Weekly Hours Worked'])[3]))
}

# [1] "The median for weekly hours worked for 2005 is Median : 40.00  "
# [1] "The median for weekly hours worked for 2006 is Median : 40.00  "
# [1] "The median for weekly hours worked for 2007 is Median : 40.0  "
# [1] "The median for weekly hours worked for 2008 is Median : 40.00  "
# [1] "The median for weekly hours worked for 2009 is Median : 40.00  "
# [1] "The median for weekly hours worked for 2010 is Median : 40.0  "
# [1] "The median for weekly hours worked for 2011 is Median : 40.00  "
# [1] "The median for weekly hours worked for 2012 is Median : 40.00  "

# Correlation Checks

# Checking the correlation Matrix between the time variables
# From Sleeping to Volunteering
library(corrplot)

corrplot(cor(newdata[,11:24]),type = "lower",diag = FALSE, addCoef.col = "black")

# There is correlation between:
# 1) Caring for children - Parenting children - correlation = 0.69
# 2) Socializing and Relaxing - Television - correlation = 0.71

year2005 <- newdata[newdata$Year == '2005',]
corrplot(cor(year2005[,11:24]),type = "lower",diag = FALSE, addCoef.col = "black")
plot(year2005$`Caring for Children`,year2005$`Playing with Children`)
plot(year2005$`Socializing & Relaxing`,year2005$Television)

year2012 <- newdata[newdata$Year == '2012',]
corrplot(cor(year2012[,11:24]),type = "lower",diag = FALSE, addCoef.col = "black")
plot(year2012$`Caring for Children`,year2012$`Playing with Children`)
plot(year2012$`Socializing & Relaxing`,year2012$Television)



# Linear Modeling for prediction

colnames(newdata)

# For caring for children and playing with children
# Logically speaking, caring for children is a bigger subset for playing with children. Caring does not only include playing but also other activities.

# But, we know that there is a correlation, therefore, we should be able to predict the time spent playing with children given the amount of time
# spent caring for children.

model1 = lm(year2005$`Playing with Children` ~ year2005$`Caring for Children`)
summary(model1)
confint(model1, level = 0.95)

plot(year2005$`Playing with Children`, year2005$`Caring for Children`)
abline(model1, col = 'red')

model2 = lm(year2005$`Playing with Children` ~ year2005$`Caring for Children` + I(((year2005$`Caring for Children`)^2)) + I(((year2005$`Caring for Children`)^3)))
summary(model2)
plot(year2005$`Caring for Children`, year2005$`Playing with Children`)
points(year2005$`Caring for Children`, fitted(model2), col='red')
lines(sort(year2005$`Caring for Children`), fitted(model2)[order(year2005$`Caring for Children`)], col = 'red')

  
# Calculating the confidence intervals using Bootstrap
library("boot")

func_1 <- function(data, index){return(coef(lm(`Playing with Children` ~ `Caring for Children` + I(((`Caring for Children`)^2)) + I(((`Caring for Children`)^3)),
                                               data =  year2005),
                                            subset=index))
  }

bootobject <- boot(year2005 ,func_1 ,1000)
bootobject$t
boot.ci(boot.out, conf = 0.95)

# Using Boot strap coefficients
  calculation = function(beta0, beta1, beta2, beta3, x){
    y1 = beta0 + beta1*x
    y2 = beta2 + beta3*x
    k = 0
    for(i in 1:22586){
      if(sh[i] >y1[i] & sh[i] < y2[i]) {k = k + 1}
    }
    return(k)
  }
  
  calculation(27.79585,1.468144, 
              29.47479,1.534122, gr)