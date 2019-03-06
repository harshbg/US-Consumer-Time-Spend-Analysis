# Cleaning the workspace
rm(list = ls())

# Setting the required working directory
setwd("~/Desktop/IAS")

library(data.table)

# Getting data
data = fread('Training.csv', header = TRUE)
# data1 = fread('Test.csv', header = TRUE)
str(data)

# Changing data type
data$Education_Level <- as.factor(data$Education_Level)
data$Age_Range <- as.factor(data$Age_Range)
data$Employment_Status <- as.factor(data$Employment_Status)
data$Gender <- as.factor(data$Gender)
data$Year <- as.factor(data$Year)

# data1$Education_Level <- as.factor(data1$Education_Level)
# data1$Age_Range <- as.factor(data1$Age_Range)
# data1$Gender <- as.factor(data1$Gender)
# data1$Year <- as.factor(data1$Year)

# Re-checking newly-formatted data types
str(data)
#/
#/
#/
# Cleaning data Process - Step 1
summary(data)
summary(data$Employment_Status)

# Taking a closer look at Employed individuals
subset1 <- data[data$Employment_Status == 'Employed',]
summary(subset1)
# We see that the minimum value for weekly hours worked and weekly earnings is 0. Something is wrong here.
subset2 <- data[data$Employment_Status == 'Unemployed',]
summary(subset2)
# Weekly hours and weekly earnings for unemployed are all 0. This is as we suspected.
subset3 <- data[data$Employment_Status == 'Not in labor force',]
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
nrow(data[data$Employment_Status == 'Employed' & data$Weekly_Earnings == 0 & data$Weekly_Hours_Worked == 0,])
# 599 rows should be re-labelled or dropped
table(data[data$Employment_Status == 'Employed' & data$Weekly_Earnings == 0 & data$Weekly_Hours_Worked == 0,'Year'])
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 51   62   53   69   81  106   87   90 

nrow(data[data$Employment_Status == 'Employed' & data$Weekly_Earnings > 0 & data$Weekly_Hours_Worked == 0,])
# 1533 rows should be re-labelled or dropped
# Break-down of 1533 rows by year
table(data[data$Employment_Status == 'Employed' & data$Weekly_Earnings > 0 & data$Weekly_Hours_Worked == 0, 'Year'])
# Results
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 125  163  160  197  229  234  218  207

nrow(data[data$Employment_Status == 'Employed' & data$Weekly_Earnings == 0 & data$Weekly_Hours_Worked > 0,])
# 4181 rows should be re-labelled or dropped
table(data[data$Employment_Status == 'Employed' & data$Weekly_Earnings == 0 & data$Weekly_Hours_Worked > 0, 'Year'])
# 2005 2006 2007 2008 2009 2010 2011 2012 
# 612  539  549  539  496  502  444  500 

# As the dirty rows make up for less than 10% (<800) of the total observations within a year, we can drop it
# Removal of first dirty subset
newdata <- data[!(data$Employment_Status == 'Employed' & data$Weekly_Earnings == 0 & data$Weekly_Hours_Worked == 0),]
# Removal of second dirty subset
newdata <- newdata[!(newdata$Employment_Status=="Employed" & newdata$Weekly_Earnings == 0 & newdata$Weekly_Hours_Worked > 0),]
# Removal of third dirty subset
newdata <- newdata[!(newdata$Employment_Status=="Employed" & newdata$Weekly_Earnings > 0 & newdata$Weekly_Hours_Worked == 0),]


# Finding the different yearly median for weekly earnings among Employed;
for (year in c(2005,2006,2007,2008,2009,2010,2011,2012)){
  print(paste("The median for weekly earnings", year, "is", summary(newdata[newdata$Year == year & newdata$Employment_Status == 'Employed', Weekly_Earnings])[3]))
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
  print(paste("The median for weekly hours worked for", year, "is", summary(newdata[newdata$Year == year & newdata$Employment_Status == 'Employed', Weekly_Hours_Worked])[3]))
}

# [1] "The median for weekly hours worked for 2005 is Median : 40 "
# [1] "The median for weekly hours worked for 2006 is Median : 40 "
# [1] "The median for weekly hours worked for 2007 is Median : 40 "
# [1] "The median for weekly hours worked for 2008 is Median : 40 "
# [1] "The median for weekly hours worked for 2009 is Median : 40 "
# [1] "The median for weekly hours worked for 2010 is Median : 40 "
# [1] "The median for weekly hours worked for 2011 is Median : 40 "
# [1] "The median for weekly hours worked for 2012 is Median : 40 "

# Descriptive Analysis

# Mean
for (year in c(2005,2006,2007,2008,2009,2010,2011,2012)){
  print(paste("The mean for", year, "is",names(newdata[,11:24]), "is", ceiling(sapply(newdata[newdata$Year == year,11:24], mean, na.rm = TRUE)), 'minutes'))
}

# Median
for (year in c(2005,2006,2007,2008,2009,2010,2011,2012)){
  print(paste("The median for", year, "is",names(newdata[,11:24]), "is", ceiling(sapply(newdata[newdata$Year == year,11:24], median, na.rm = TRUE)), 'minutes'))
}

names(newdata)
newdata1 <-newdata[,-c("Id", "Age_Range", "Total")] 
library(psych)
summary <- describe.by(newdata1, list(newdata$Employment_Status, newdata1$Year), mat = TRUE, digits = 2)

# Frequencies and Cross-Tabulations (for Categorical Variables)
str(newdata)

# Employment Status - Education - Year
mytable2 <- xtabs(~newdata$Employment_Status + newdata$Education_Level + newdata$Year)
ftable(mytable2)
summary(mytable2) #Chi-square shows that certain groups have too little observations. Need to reclassify

#Reclassification

newdata$Education_Level <- as.character(newdata$Education_Level)
# data1$Education_Level <- as.character(data1$Education_Level)

New_Education <- NULL
classEducation <- function(Education_Level) {
   if(Education_Level == '10th grade' | Education_Level == '11th grade' | Education_Level == '12th grade' | Education_Level == '9th grade' | Education_Level == 'High School') {return('High School')}
   else if(Education_Level == 'Associate Degree' | Education_Level == 'Bachelor') {return('Undergraduate Studies')}
   else if(Education_Level == 'Doctoral Degree' | Education_Level == 'Master'| Education_Level == 'Prof. Degree') {return('Postgraduate Studies')}
   else {return('Some College Education')}
}

New_Education <- sapply(newdata$Education_Level, classEducation, simplify = TRUE, USE.NAMES = TRUE)
# New_Education <- sapply(data1$Education_Level, classEducation, simplify = TRUE, USE.NAMES = TRUE)

newdata$Education_Level<- as.factor(New_Education)
# data1$Education_Level<- as.factor(New_Education)

table(unique(newdata$Education_Level))
str(newdata)

# Employment Status - Education - Year
mytable3 <- xtabs(~newdata$Employment_Status + newdata$Education_Level + newdata$Year)
ftable(mytable3)
summary(mytable3)

# Employment Status - Education
mytable4 <- xtabs(~newdata$Employment_Status + newdata$Education_Level)
ftable(mytable4)
summary(mytable4)

library(gmodels)
crosstable <- CrossTable(newdata$Employment_Status, newdata$Education_Level)
print(crosstable)

# Correlation Checks

# Checking the correlation Matrix between the time variables
# From Sleeping to Volunteering
library(corrplot)

corrplot(cor(newdata[,11:24]),type = "lower",diag = FALSE, addCoef.col = "black")

# There is correlation between:
# 1) Caring for children - Parenting children - correlation = 0.69
# 2) Socializing and Relaxing - Television - correlation = 0.71

#Testing for normality
library(nortest)
lapply(newdata[,c('Sleeping', "Grooming", "Housework", "Food _Drink_Prep", "Caring_for_Children", "Playing_with_Children", "Job_Searching", "Shopping", "Eating_and_Drinking", "Socializing_Relaxing", "Television", "Golfing", "Running", "Volunteering")], function(x) cvm.test(x))

# Performing T-test to see differences in mean between Genders
# Assumes Normal Distribution
lapply(newdata[,c('Sleeping', "Grooming", "Housework", "Food _Drink_Prep", "Caring_for_Children", "Playing_with_Children", "Job_Searching", "Shopping", "Eating_and_Drinking", "Socializing_Relaxing", "Television", "Golfing", "Running", "Volunteering")], function(x) t.test(x ~ newdata$Gender, var.equal = TRUE))
# All variables have a difference in mean between Genders (Gender is a significiant variable)

# Performing wilcoxon test to see differences in mean between Genders
# Does not assume Normal Distribution
lapply(newdata[,c('Sleeping', "Grooming", "Housework", "Food _Drink_Prep", "Caring_for_Children", "Playing_with_Children", "Job_Searching", "Shopping", "Eating_and_Drinking", "Socializing_Relaxing", "Television", "Golfing", "Running", "Volunteering")], function(x) wilcox.test(x ~ newdata$Gender))
# Male and female do not have the same continouous distribution

# Running ANOVA test to see differences between the 3 different populations in employment status
lapply(newdata[,c('Sleeping', "Grooming", "Housework", "Food _Drink_Prep", "Caring_for_Children", "Playing_with_Children", "Job_Searching", "Shopping", "Eating_and_Drinking", "Socializing_Relaxing", "Television", "Golfing", "Running", "Volunteering")], function(x) anova(lm(x ~ newdata$Employment_Status)))
# There is no significant difference in the mean of Golfing and Shopping among the different groups of Employment Status
lapply(newdata[,c('Shopping')], function(x) anova(lm(x ~ newdata$Gender)))
lapply(newdata[,c('Shopping')], function(x) anova(lm(x ~ newdata$Age_Range)))
lapply(newdata[,c('Shopping')], function(x) anova(lm(x ~ newdata$Education_Level)))

# Running ANOVA to see if there is a difference in childcare
lapply(newdata[,c('Caring_for_Children')], function(x) anova(lm(x ~ newdata$Gender)))
lapply(newdata[,c('Caring_for_Children')], function(x) anova(lm(x ~ newdata$Age_Range)))
lapply(newdata[,c('Caring_for_Children')], function(x) anova(lm(x ~ newdata$Education_Level)))

# Seeing time spent-patterns
hist(newdata$Sleeping, breaks=12, col="red")
summary(newdata$Sleeping)
quantile(newdata$Sleeping, 0.98)
# Removing outliers from sleeping data (max = 840 mins = 14 hours)
sum(newdata$Sleeping >= 840) #Total of 1257 rows
newdata <- newdata[newdata$Sleeping <= quantile(newdata$Sleeping, 0.98),]
# While it is deemed impossible for a person to sleep more than 14 hours on the average, it will also be deemed impossible for a person to sleep less than 4 hours
quantile(newdata$Sleeping, 0.02)
sum(newdata$Sleeping <= quantile(newdata$Sleeping, 0.02)) #Total of 1147 rows
newdata <- newdata[newdata$Sleeping >= quantile(newdata$Sleeping, 0.02),]
hist(newdata$Sleeping, breaks=12, col="red")

hist(data1$Sleeping, breaks=12, col="red")
summary(data1$Sleeping)
quantile(data1$Sleeping, 0.98)
# Removing outliers from sleeping data (max = 840 mins = 14 hours)
sum(data1$Sleeping >= 840) #Total of 1257 rows
data1 <- data1[data1$Sleeping <= quantile(data1$Sleeping, 0.98),]
# While it is deemed impossible for a person to sleep more than 14 hours on the average, it will also be deemed impossible for a person to sleep less than 4 hours
quantile(data1$Sleeping, 0.02)
sum(data1$Sleeping <= quantile(data1$Sleeping, 0.02)) #Total of 1147 rows
data1 <- data1[data1$Sleeping >= quantile(data1$Sleeping, 0.02),]
hist(data1$Sleeping, breaks=12, col="red")


# Grooming is different from sleeping. Grooming is leisure, while sleeping is a necessity.
hist(newdata$Grooming, breaks = 12, col = 'yellow')
grooming <- newdata[which(newdata$Grooming != 0),]
summary(grooming$Grooming)
# The maximum time spent on grooming is 1043 minutes, which is deemed ridiculous
quantile(grooming$Grooming, 0.98) #140 mins is within the 98% percentile
sum(grooming$Grooming >= quantile(grooming$Grooming, 0.98)) #908 rows exceeding
newdata <- newdata[newdata$Grooming <= quantile(grooming$Grooming, 0.98),]
quantile(grooming$Grooming, 0.01) #5 mins minimum
newdata <- newdata[newdata$Grooming >= 5 | newdata$Grooming == 0,]
hist(newdata$Grooming, breaks = 12, col = 'yellow')
grooming <- grooming[grooming$Grooming <= 140 & grooming$Grooming >= 5,]
hist(grooming$Grooming, breaks = 12, col = 'yellow')

hist(data1$Grooming, breaks = 12, col = 'yellow')
grooming <- data1[which(data1$Grooming != 0),]
summary(grooming$Grooming)
# The maximum time spent on grooming is 1043 minutes, which is deemed ridiculous
quantile(grooming$Grooming, 0.98) #140 mins is within the 98% percentile
sum(grooming$Grooming >= quantile(grooming$Grooming, 0.98)) #908 rows exceeding
data1 <- data1[data1$Grooming <= quantile(grooming$Grooming, 0.98),]
quantile(grooming$Grooming, 0.01) #5 mins minimum
data1 <- data1[data1$Grooming >= 5 | data1$Grooming == 0,]
hist(data1$Grooming, breaks = 12, col = 'yellow')
grooming <- grooming[grooming$Grooming <= 140 & grooming$Grooming >= 5,]
hist(grooming$Grooming, breaks = 12, col = 'yellow')

# Housework
names(newdata)
hist(newdata$Housework, breaks = 12, col = 'green')
housework <- newdata[which(newdata$Housework != 0),]
summary(housework$Housework)
# Maximum time is 1030 minutes = impossible
quantile(housework$Housework, 0.98) #Set at 400 mins
sum(housework$Housework >= quantile(housework$Housework, 0.98)) #447 rows exceeding
newdata <- newdata[newdata$Housework <= quantile(housework$Housework, 0.98),]
quantile(housework$Housework, 0.02) # 5 mins minimum
newdata <- newdata[newdata$Housework >= 5 | newdata$Housework == 0,]
housework <- housework[housework$Housework <= 408.04 & housework$Housework >= 5,]
hist(housework$Housework, breaks = 12, col = 'green')

hist(data1$Housework, breaks = 12, col = 'green')
housework <- data1[which(data1$Housework != 0),]
summary(housework$Housework)
# Maximum time is 1030 minutes = impossible
quantile(housework$Housework, 0.98) #Set at 400 mins
sum(housework$Housework >= quantile(housework$Housework, 0.98)) #447 rows exceeding
data1 <- data1[data1$Housework <= quantile(housework$Housework, 0.98),]
quantile(housework$Housework, 0.02) # 5 mins minimum
data1 <- data1[data1$Housework >= 5 | data1$Housework == 0,]
housework <- housework[housework$Housework <= 408.04 & housework$Housework >= 5,]
hist(housework$Housework, breaks = 12, col = 'green')


#Food _Drink_Prep
names(newdata)
hist(newdata$`Food _Drink_Prep`, breaks = 12, col = 'cyan')
Food_Drink_Prep <- newdata[which(newdata$`Food _Drink_Prep` != 0),]
summary(Food_Drink_Prep$`Food _Drink_Prep`)
# Maximum time is 755 minutes = impossible
quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.98) #Set at 230 mins
sum(Food_Drink_Prep$`Food _Drink_Prep` >= quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.98)) #630 rows exceeding
newdata <- newdata[newdata$`Food _Drink_Prep` <= quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.98),]
quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.02) # 4 mins minimum
newdata <- newdata[newdata$`Food _Drink_Prep` >= 4 | newdata$`Food _Drink_Prep` == 0,]
Food_Drink_Prep <- Food_Drink_Prep[Food_Drink_Prep$`Food _Drink_Prep` <= 230 & Food_Drink_Prep$`Food _Drink_Prep` >= 4,]
hist(Food_Drink_Prep$`Food _Drink_Prep`, breaks = 12, col = 'cyan')

hist(data1$`Food _Drink_Prep`, breaks = 12, col = 'cyan')
Food_Drink_Prep <- data1[which(data1$`Food _Drink_Prep` != 0),]
summary(Food_Drink_Prep$`Food _Drink_Prep`)
# Maximum time is 755 minutes = impossible
quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.98) #Set at 230 mins
sum(Food_Drink_Prep$`Food _Drink_Prep` >= quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.98)) #630 rows exceeding
data1 <- data1[data1$`Food _Drink_Prep` <= quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.98),]
quantile(Food_Drink_Prep$`Food _Drink_Prep`, 0.02) # 4 mins minimum
data1 <- data1[data1$`Food _Drink_Prep` >= 4 | data1$`Food _Drink_Prep` == 0,]
Food_Drink_Prep <- Food_Drink_Prep[Food_Drink_Prep$`Food _Drink_Prep` <= 230 & Food_Drink_Prep$`Food _Drink_Prep` >= 4,]
hist(Food_Drink_Prep$`Food _Drink_Prep`, breaks = 12, col = 'cyan')


#Caring_for_Children
names(newdata)
hist(newdata$Caring_for_Children, breaks = 12, col = 'blue')
Caring <- newdata[which(newdata$Caring_for_Children != 0),]
summary(Caring$Caring_for_Children)
# Maximum time is 973 minutes = impossible
quantile(Caring$Caring_for_Children, 0.98) #Set at 435 mins
sum(Caring$Caring_for_Children >= quantile(Caring$Caring_for_Children, 0.98)) #289 rows exceeding
newdata <- newdata[newdata$Caring_for_Children <= quantile(Caring$Caring_for_Children, 0.98),]
quantile(Caring$Caring_for_Children, 0.02) # 2 mins minimum
newdata <- newdata[newdata$Caring_for_Children >= 2 | newdata$Caring_for_Children == 0,]
Caring <- Caring[Caring$Caring_for_Children <= 435 & Caring$Caring_for_Children >= 2,]
hist(Caring$Caring_for_Children, breaks = 12, col = 'blue')

hist(data1$Caring_for_Children, breaks = 12, col = 'blue')
Caring <- data1[which(data1$Caring_for_Children != 0),]
summary(Caring$Caring_for_Children)
# Maximum time is 973 minutes = impossible
quantile(Caring$Caring_for_Children, 0.98) #Set at 435 mins
sum(Caring$Caring_for_Children >= quantile(Caring$Caring_for_Children, 0.98)) #289 rows exceeding
data1 <- data1[data1$Caring_for_Children <= quantile(Caring$Caring_for_Children, 0.98),]
quantile(Caring$Caring_for_Children, 0.02) # 2 mins minimum
data1 <- data1[data1$Caring_for_Children >= 2 | data1$Caring_for_Children == 0,]
Caring <- Caring[Caring$Caring_for_Children <= 435 & Caring$Caring_for_Children >= 2,]
hist(Caring$Caring_for_Children, breaks = 12, col = 'blue')

#Playing with Children
names(newdata)
hist(newdata$Playing_with_Children, breaks = 12, col = 'orange')
Playing <- newdata[which(newdata$Playing_with_Children != 0),]
summary(Playing$Playing_with_Children)
# Maximum time is 420 minutes... maybe possible but highly unlikely
quantile(Playing$Playing_with_Children, 0.98) #Set at 305 mins
sum(Playing$Playing_with_Children >= quantile(Playing$Playing_with_Children, 0.98)) #82 rows exceeding
newdata <- newdata[newdata$Playing_with_Children <= quantile(Playing$Playing_with_Children, 0.98),]
quantile(Playing$Playing_with_Children, 0.02) # 15 mins minimum
newdata <- newdata[newdata$Playing_with_Children >= 15 | newdata$Playing_with_Children == 0,]
Playing <- Playing[Playing$Playing_with_Children <= 305 & Playing$Playing_with_Children >= 15,]
hist(Playing$Playing_with_Children, breaks = 12, col = 'orange')

hist(data1$Playing_with_Children, breaks = 12, col = 'orange')
Playing <- data1[which(data1$Playing_with_Children != 0),]
summary(Playing$Playing_with_Children)
# Maximum time is 420 minutes... maybe possible but highly unlikely
quantile(Playing$Playing_with_Children, 0.98) #Set at 305 mins
sum(Playing$Playing_with_Children >= quantile(Playing$Playing_with_Children, 0.98)) #82 rows exceeding
data1 <- data1[data1$Playing_with_Children <= quantile(Playing$Playing_with_Children, 0.98),]
quantile(Playing$Playing_with_Children, 0.02) # 15 mins minimum
data1 <- data1[data1$Playing_with_Children >= 15 | data1$Playing_with_Children == 0,]
Playing <- Playing[Playing$Playing_with_Children <= 305 & Playing$Playing_with_Children >= 15,]
hist(Playing$Playing_with_Children, breaks = 12, col = 'orange')


#Job Searching
names(newdata)
hist(newdata$Job_Searching, breaks = 12, col = 'pink')
Job_Search <- newdata[which(newdata$Job_Searching != 0),]
summary(Job_Search$Job_Searching)
# Maximum time is 983 minutes... maybe possible but highly unlikely
quantile(Job_Search$Job_Searching, 0.98) #Set at 545.52 mins
sum(Job_Search$Job_Searching >= quantile(Job_Search$Job_Searching, 0.98)) #15 rows exceeding
newdata <- newdata[newdata$Job_Searching <= quantile(Job_Search$Job_Searching, 0.98),]
quantile(Job_Search$Job_Searching, 0.02) # 7 mins minimum
newdata <- newdata[newdata$Job_Searching >= 7 | newdata$Job_Searching == 0,]
Job_Search <- Job_Search[Job_Search$Job_Searching <= 545.52 & Job_Search$Job_Searching >= 7,]
hist(Job_Search$Job_Searching, breaks = 12, col = 'pink')

names(data1)
hist(data1$Job_Searching, breaks = 12, col = 'pink')
Job_Search <- data1[which(data1$Job_Searching != 0),]
summary(Job_Search$Job_Searching)
# Maximum time is 983 minutes... maybe possible but highly unlikely
quantile(Job_Search$Job_Searching, 0.98) #Set at 545.52 mins
sum(Job_Search$Job_Searching >= quantile(Job_Search$Job_Searching, 0.98)) #15 rows exceeding
data1 <- data1[data1$Job_Searching <= quantile(Job_Search$Job_Searching, 0.98),]
quantile(Job_Search$Job_Searching, 0.02) # 7 mins minimum
data1 <- data1[data1$Job_Searching >= 7 | data1$Job_Searching == 0,]
Job_Search <- Job_Search[Job_Search$Job_Searching <= 545.52 & Job_Search$Job_Searching >= 7,]
hist(Job_Search$Job_Searching, breaks = 12, col = 'pink')


#Shopping
names(newdata)
hist(newdata$Shopping, breaks = 12, col = 'magenta')
Shop <- newdata[which(newdata$Shopping != 0),]
summary(Shop$Shopping)
# Maximum time is 640 minutes... maybe possible but highly unlikely
quantile(Shop$Shopping, 0.98) #Set at 240 mins
sum(Shop$Shopping >= quantile(Shop$Shopping, 0.98)) #506 rows exceeding
newdata <- newdata[newdata$Shopping <= quantile(Shop$Shopping, 0.98),]
quantile(Shop$Shopping, 0.02) # 3 mins minimum
newdata <- newdata[newdata$Shopping >= 3 | newdata$Shopping == 0,]
Shop <- Shop[Shop$Shopping <= quantile(Shop$Shopping, 0.98) & Shop$Shopping >= quantile(Shop$Shopping, 0.02),]
hist(Shop$Shopping, breaks = 12, col = 'magenta')

names(data1)
hist(data1$Shopping, breaks = 12, col = 'magenta')
Shop <- data1[which(data1$Shopping != 0),]
summary(Shop$Shopping)
# Maximum time is 640 minutes... maybe possible but highly unlikely
quantile(Shop$Shopping, 0.98) #Set at 240 mins
sum(Shop$Shopping >= quantile(Shop$Shopping, 0.98)) #506 rows exceeding
data1 <- data1[data1$Shopping <= quantile(Shop$Shopping, 0.98),]
quantile(Shop$Shopping, 0.02) # 3 mins minimum
data1 <- data1[data1$Shopping >= 3 | data1$Shopping == 0,]
Shop <- Shop[Shop$Shopping <= quantile(Shop$Shopping, 0.98) & Shop$Shopping >= quantile(Shop$Shopping, 0.02),]
hist(Shop$Shopping, breaks = 12, col = 'magenta')


#Eating_and_Drinking like sleeping is a necessity. It is impossible for anyone to spend 0 mins eating and drinking
names(newdata)
hist(newdata$Eating_and_Drinking, breaks = 12, col = 'salmon')
EatDrink <- newdata[which(newdata$Eating_and_Drinking != 0),]
summary(EatDrink$Eating_and_Drinking)
# Maximum time is 895 minutes... impossible
quantile(EatDrink$Eating_and_Drinking, 0.98) #Set at 210 mins
sum(EatDrink$Eating_and_Drinking >= quantile(EatDrink$Eating_and_Drinking, 0.98)) #1080 rows exceeding
newdata <- newdata[newdata$Eating_and_Drinking <= quantile(EatDrink$Eating_and_Drinking, 0.98),]
quantile(EatDrink$Eating_and_Drinking, 0.02) # 10 mins minimum
newdata <- newdata[newdata$Eating_and_Drinking >= quantile(EatDrink$Eating_and_Drinking, 0.02),]
EatDrink <- EatDrink[EatDrink$Eating_and_Drinking <= quantile(EatDrink$Eating_and_Drinking, 0.98) & EatDrink$Eating_and_Drinking >= quantile(EatDrink$Eating_and_Drinking, 0.02),]
hist(EatDrink$Eating_and_Drinking, breaks = 12, col = 'salmon')

hist(data1$Eating_and_Drinking, breaks = 12, col = 'salmon')
EatDrink <- data1[which(data1$Eating_and_Drinking != 0),]
summary(EatDrink$Eating_and_Drinking)
# Maximum time is 895 minutes... impossible
quantile(EatDrink$Eating_and_Drinking, 0.98) #Set at 210 mins
sum(EatDrink$Eating_and_Drinking >= quantile(EatDrink$Eating_and_Drinking, 0.98)) #1080 rows exceeding
data1 <- data1[data1$Eating_and_Drinking <= quantile(EatDrink$Eating_and_Drinking, 0.98),]
quantile(EatDrink$Eating_and_Drinking, 0.02) # 10 mins minimum
data1 <- data1[data1$Eating_and_Drinking >= quantile(EatDrink$Eating_and_Drinking, 0.02),]
EatDrink <- EatDrink[EatDrink$Eating_and_Drinking <= quantile(EatDrink$Eating_and_Drinking, 0.98) & EatDrink$Eating_and_Drinking >= quantile(EatDrink$Eating_and_Drinking, 0.02),]
hist(EatDrink$Eating_and_Drinking, breaks = 12, col = 'salmon')

#Socializing_Relaxing
names(newdata)
hist(newdata$Socializing_Relaxing, breaks = 12, col = 'lightseagreen')
social <- newdata[which(newdata$Socializing_Relaxing != 0),]
summary(social$Socializing_Relaxing)
# Maximum time is 1180 minutes... impossible
quantile(social$Socializing_Relaxing, 0.98) #Set at 797 mins
sum(social$Socializing_Relaxing >= quantile(social$Socializing_Relaxing, 0.98)) #919 rows exceeding
newdata <- newdata[newdata$Socializing_Relaxing <= quantile(social$Socializing_Relaxing, 0.98),]
quantile(social$Socializing_Relaxing, 0.02) # 30 mins minimum
newdata <- newdata[newdata$Socializing_Relaxing >= quantile(social$Socializing_Relaxing, 0.02) | newdata$Socializing_Relaxing == 0,]
social <- social[social$Socializing_Relaxing <= quantile(social$Socializing_Relaxing, 0.98) & social$Socializing_Relaxing >= quantile(social$Socializing_Relaxing, 0.02),]
hist(social$Socializing_Relaxing, breaks = 12, col = 'lightseagreen')

names(data1)
hist(data1$Socializing_Relaxing, breaks = 12, col = 'lightseagreen')
social <- data1[which(data1$Socializing_Relaxing != 0),]
summary(social$Socializing_Relaxing)
# Maximum time is 1180 minutes... impossible
quantile(social$Socializing_Relaxing, 0.98) #Set at 797 mins
sum(social$Socializing_Relaxing >= quantile(social$Socializing_Relaxing, 0.98)) #919 rows exceeding
data1 <- data1[data1$Socializing_Relaxing <= quantile(social$Socializing_Relaxing, 0.98),]
quantile(social$Socializing_Relaxing, 0.02) # 30 mins minimum
data1 <- data1[data1$Socializing_Relaxing >= quantile(social$Socializing_Relaxing, 0.02) | data1$Socializing_Relaxing == 0,]
social <- social[social$Socializing_Relaxing <= quantile(social$Socializing_Relaxing, 0.98) & social$Socializing_Relaxing >= quantile(social$Socializing_Relaxing, 0.02),]
hist(social$Socializing_Relaxing, breaks = 12, col = 'lightseagreen')


#Television
names(newdata)
hist(newdata$Television, breaks = 12, col = 'indianred')
TV <- newdata[which(newdata$Television != 0),]
summary(TV$Television)
# Maximum time is 795 minutes... is highly improbable
quantile(TV$Television, 0.98) #Set at 620 mins
sum(TV$Television >= quantile(TV$Television, 0.98)) #783 rows exceeding
newdata <- newdata[newdata$Television <= quantile(TV$Television, 0.98),]
quantile(TV$Television, 0.02) # 28 mins minimum
newdata <- newdata[newdata$Television >= quantile(TV$Television, 0.02) | newdata$Television == 0,]
TV <- TV[TV$Television <= quantile(TV$Television, 0.98) & TV$Television >= quantile(TV$Television, 0.02),]
hist(TV$Television, breaks = 12, col = 'indianred')

names(data1)
hist(data1$Television, breaks = 12, col = 'indianred')
TV <- data1[which(data1$Television != 0),]
summary(TV$Television)
# Maximum time is 795 minutes... is highly improbable
quantile(TV$Television, 0.98) #Set at 620 mins
sum(TV$Television >= quantile(TV$Television, 0.98)) #783 rows exceeding
data1 <- data1[data1$Television <= quantile(TV$Television, 0.98),]
quantile(TV$Television, 0.02) # 28 mins minimum
data1 <- data1[data1$Television >= quantile(TV$Television, 0.02) | data1$Television == 0,]
TV <- TV[TV$Television <= quantile(TV$Television, 0.98) & TV$Television >= quantile(TV$Television, 0.02),]
hist(TV$Television, breaks = 12, col = 'indianred')


#Golfing
names(newdata)
hist(newdata$Golfing, breaks = 12, col = 'khaki')
golf <- newdata[which(newdata$Golfing != 0),]
summary(golf$Golfing)
# Maximum time is 600 minutes... highly improbable
quantile(golf$Golfing, 0.98) #Set at 417.52 mins
sum(golf$Golfing >= quantile(golf$Golfing, 0.98)) #6 rows exceeding
newdata <- newdata[newdata$Golfing <= quantile(golf$Golfing, 0.98),]
quantile(golf$Golfing, 0.02) # 30 mins minimum
newdata <- newdata[newdata$Golfing >= quantile(golf$Golfing, 0.02) | newdata$Golfing == 0,]
golf <- golf[golf$Golfing <= quantile(golf$Golfing, 0.98) & golf$Golfing >= quantile(golf$Golfing, 0.02),]
hist(golf$Golfing, breaks = 12, col = 'khaki')

names(data1)
hist(data1$Golfing, breaks = 12, col = 'khaki')
golf <- data1[which(data1$Golfing != 0),]
summary(golf$Golfing)
# Maximum time is 600 minutes... highly improbable
quantile(golf$Golfing, 0.98) #Set at 417.52 mins
sum(golf$Golfing >= quantile(golf$Golfing, 0.98)) #6 rows exceeding
data1 <- data1[data1$Golfing <= quantile(golf$Golfing, 0.98),]
quantile(golf$Golfing, 0.02) # 30 mins minimum
data1 <- data1[data1$Golfing >= quantile(golf$Golfing, 0.02) | data1$Golfing == 0,]
golf <- golf[golf$Golfing <= quantile(golf$Golfing, 0.98) & golf$Golfing >= quantile(golf$Golfing, 0.02),]
hist(golf$Golfing, breaks = 12, col = 'khaki')


#Running
names(newdata)
hist(newdata$Running, breaks = 12, col = 'mediumorchid')
Ran <- newdata[which(newdata$Running != 0),]
summary(Ran$Running)
# Maximum time is 505 minutes... highly improbable
quantile(Ran$Running, 0.98) #Set at 145.4 mins
sum(Ran$Running >= quantile(Ran$Running, 0.98)) #12 rows exceeding
newdata <- newdata[newdata$Running <= quantile(Ran$Running, 0.98),]
quantile(Ran$Running, 0.02) # 10 mins minimum
newdata <- newdata[newdata$Running >= quantile(Ran$Running, 0.02) | newdata$Running == 0,]
Ran <- Ran[Ran$Running <= quantile(Ran$Running, 0.98) & Ran$Running >= quantile(Ran$Running, 0.02),]
hist(Ran$Running, breaks = 12, col = 'mediumorchid')

names(data1)
hist(data1$Running, breaks = 12, col = 'mediumorchid')
Ran <- data1[which(data1$Running != 0),]
summary(Ran$Running)
# Maximum time is 505 minutes... highly improbable
quantile(Ran$Running, 0.98) #Set at 145.4 mins
sum(Ran$Running >= quantile(Ran$Running, 0.98)) #12 rows exceeding
data1 <- data1[data1$Running <= quantile(Ran$Running, 0.98),]
quantile(Ran$Running, 0.02) # 10 mins minimum
data1 <- data1[data1$Running >= quantile(Ran$Running, 0.02) | data1$Running == 0,]
Ran <- Ran[Ran$Running <= quantile(Ran$Running, 0.98) & Ran$Running >= quantile(Ran$Running, 0.02),]
hist(Ran$Running, breaks = 12, col = 'mediumorchid')


#Volunteering
names(newdata)
hist(newdata$Volunteering, breaks = 12, col = 'midnightblue')
Volunteers <- newdata[which(newdata$Volunteering != 0),]
summary(Volunteers$Volunteering)
# Maximum time is 877 minutes... highly improbable
quantile(Volunteers$Volunteering, 0.98) #Set at 506.14 mins
sum(Volunteers$Volunteering >= quantile(Volunteers$Volunteering, 0.98)) #67 rows exceeding
newdata <- newdata[newdata$Volunteering <= quantile(Volunteers$Volunteering, 0.98),]
quantile(Volunteers$Volunteering, 0.02) # 5 mins minimum
newdata <- newdata[newdata$Volunteering >= quantile(Volunteers$Volunteering, 0.02) | newdata$Volunteering == 0,]
Volunteers <- Volunteers[Volunteers$Volunteering <= quantile(Volunteers$Volunteering, 0.98) & Volunteers$Volunteering >= quantile(Volunteers$Volunteering, 0.02),]
hist(Volunteers$Volunteering, breaks = 12, col = 'midnightblue')

names(data1)
hist(data1$Volunteering, breaks = 12, col = 'midnightblue')
Volunteers <- data1[which(data1$Volunteering != 0),]
summary(Volunteers$Volunteering)
# Maximum time is 877 minutes... highly improbable
quantile(Volunteers$Volunteering, 0.98) #Set at 506.14 mins
sum(Volunteers$Volunteering >= quantile(Volunteers$Volunteering, 0.98)) #67 rows exceeding
data1 <- data1[data1$Volunteering <= quantile(Volunteers$Volunteering, 0.98),]
quantile(Volunteers$Volunteering, 0.02) # 5 mins minimum
data1 <- data1[data1$Volunteering >= quantile(Volunteers$Volunteering, 0.02) | data1$Volunteering == 0,]
Volunteers <- Volunteers[Volunteers$Volunteering <= quantile(Volunteers$Volunteering, 0.98) & Volunteers$Volunteering >= quantile(Volunteers$Volunteering, 0.02),]
hist(Volunteers$Volunteering, breaks = 12, col = 'midnightblue')


#Weekly Hours Worked
names(newdata)
hist(newdata$Weekly_Hours_Worked, breaks = 12, col = 'blue')
WeeklyHours <- newdata[which(newdata$Weekly_Hours_Worked != 0),]
summary(WeeklyHours$Weekly_Hours_Worked)
# Maximum time is 160 hours... highly improbable
quantile(WeeklyHours$Weekly_Hours_Worked, 0.98) #Set at 68 hours
sum(WeeklyHours$Weekly_Hours_Worked >= quantile(WeeklyHours$Weekly_Hours_Worked, 0.98)) #549 rows exceeding
newdata <- newdata[newdata$Weekly_Hours_Worked <= quantile(WeeklyHours$Weekly_Hours_Worked, 0.98),]
quantile(WeeklyHours$Weekly_Hours_Worked, 0.02) # 10 hours minimum
newdata <- newdata[newdata$Weekly_Hours_Worked >= quantile(WeeklyHours$Weekly_Hours_Worked, 0.02) | newdata$Weekly_Hours_Worked == 0,]
WeeklyHours <- WeeklyHours[WeeklyHours$Weekly_Hours_Worked <= quantile(WeeklyHours$Weekly_Hours_Worked, 0.98) & WeeklyHours$Weekly_Hours_Worked >= quantile(WeeklyHours$Weekly_Hours_Worked, 0.02),]
hist(WeeklyHours$Weekly_Hours_Worked, breaks = 12, col = 'midnightblue')

names(data1)
hist(data1$Weekly_Hours_Worked, breaks = 12, col = 'blue')
WeeklyHours <- data1[which(data1$Weekly_Hours_Worked != 0),]
summary(WeeklyHours$Weekly_Hours_Worked)
# Maximum time is 160 hours... highly improbable
quantile(WeeklyHours$Weekly_Hours_Worked, 0.98) #Set at 68 hours
sum(WeeklyHours$Weekly_Hours_Worked >= quantile(WeeklyHours$Weekly_Hours_Worked, 0.98)) #549 rows exceeding
data1 <- data1[data1$Weekly_Hours_Worked <= quantile(WeeklyHours$Weekly_Hours_Worked, 0.98),]
quantile(WeeklyHours$Weekly_Hours_Worked, 0.02) # 10 hours minimum
data1 <- data1[data1$Weekly_Hours_Worked >= quantile(WeeklyHours$Weekly_Hours_Worked, 0.02) | data1$Weekly_Hours_Worked == 0,]
WeeklyHours <- WeeklyHours[WeeklyHours$Weekly_Hours_Worked <= quantile(WeeklyHours$Weekly_Hours_Worked, 0.98) & WeeklyHours$Weekly_Hours_Worked >= quantile(WeeklyHours$Weekly_Hours_Worked, 0.02),]
hist(WeeklyHours$Weekly_Hours_Worked, breaks = 12, col = 'midnightblue')


#Weekly Earnings
names(newdata)
hist(newdata$Weekly_Earnings, breaks = 12, col = 'blue')
summary(newdata$Weekly_Earnings)
WeeklyEarnings <- newdata[which(newdata$Weekly_Earnings != 0),]
summary(WeeklyEarnings$Weekly_Earnings)
# While it is possible to have earned the maximum of $2885/week but there must be a minimum earnings given the minimum wage policies
# To do this, we need to create another variable called Earnings/Hours

newdata$Hourly_Earnings <- (newdata$Weekly_Earnings/newdata$Weekly_Hours_Worked)
newdata$Hourly_Earnings <- ifelse(newdata$Hourly_Earnings == 'NaN',0, round(newdata$Hourly_Earnings, digits = 2))
summary(newdata$Hourly_Earnings)

hist(newdata$Hourly_Earnings, breaks = 12, col = 'blue')
HourlyEarnings <- newdata[which(newdata$Hourly_Earnings != 0),]
summary(HourlyEarnings$Hourly_Earnings)

quantile(HourlyEarnings$Hourly_Earnings, 0.02) #Set at $4.572 (minimum wage policy)
sum(HourlyEarnings$Hourly_Earnings < quantile(HourlyEarnings$Hourly_Earnings, 0.02)) #538 rows exceeding
newdata <- newdata[newdata$Hourly_Earnings >= quantile(HourlyEarnings$Hourly_Earnings, 0.02) | newdata$Hourly_Earnings == 0,]
HourlyEarnings <- HourlyEarnings[HourlyEarnings$Hourly_Earnings >= quantile(HourlyEarnings$Hourly_Earnings, 0.02),]
hist(HourlyEarnings$Hourly_Earnings, breaks = 12, col = 'midnightblue')

names(data1)
hist(data1$Weekly_Earnings, breaks = 12, col = 'blue')
summary(data1$Weekly_Earnings)
WeeklyEarnings <- data1[which(data1$Weekly_Earnings != 0),]
summary(WeeklyEarnings$Weekly_Earnings)
# While it is possible to have earned the maximum of $2885/week but there must be a minimum earnings given the minimum wage policies
# To do this, we need to create another variable called Earnings/Hours

data1$Hourly_Earnings <- (data1$Weekly_Earnings/data1$Weekly_Hours_Worked)
data1$Hourly_Earnings <- ifelse(data1$Hourly_Earnings == 'NaN',0, round(data1$Hourly_Earnings, digits = 2))
summary(data1$Hourly_Earnings)

hist(data1$Hourly_Earnings, breaks = 12, col = 'blue')
HourlyEarnings <- data1[which(data1$Hourly_Earnings != 0),]
summary(HourlyEarnings$Hourly_Earnings)

quantile(HourlyEarnings$Hourly_Earnings, 0.02) #Set at $4.572 (minimum wage policy)
sum(HourlyEarnings$Hourly_Earnings < quantile(HourlyEarnings$Hourly_Earnings, 0.02)) #538 rows exceeding
data1 <- data1[data1$Hourly_Earnings >= quantile(HourlyEarnings$Hourly_Earnings, 0.02) | data1$Hourly_Earnings == 0,]
HourlyEarnings <- HourlyEarnings[HourlyEarnings$Hourly_Earnings >= quantile(HourlyEarnings$Hourly_Earnings, 0.02),]
hist(HourlyEarnings$Hourly_Earnings, breaks = 12, col = 'midnightblue')


# For question 1-6:
write.csv(newdata, file = "initial_newdata.csv")
write.csv(data1, file = "initial_testdata.csv")


# Linear Modeling for prediction

library(lmtest)
library(sandwich)

colnames(newdata)

# For caring for children and playing with children
# Logically speaking, caring for children is a bigger subset for playing with children. Caring does not only include playing but also other activities.

# But, we know that there is a correlation, therefore, we should be able to predict the time spent playing with children given the amount of time
# spent caring for children.

# Because we want to study the relationship between Caring and Playing, we should exclude those who do not play
newdata2 <- newdata[newdata$Playing_with_Children!=0 ,]
acf(newdata2$Playing_with_Children)
model1 = lm(newdata2$Playing_with_Children ~ newdata2$Caring_for_Children)
summary(model1)
confint(model1, level = 0.95)
plot(newdata2$Caring_for_Children, newdata2$Playing_with_Children, col = 'grey')
abline(model1, col = 'red')
coeftest(model1, vcov. = NeweyWest(model1,lag = NULL))
confint(model1, vcov = NeweyWest(model1, lag = NULL),level = 0.95)

newdata$UL_Playing <- (newdata$Caring_for_Children*0.4532799) + 22.3713459
newdata$Mean_Playing <- (newdata$Caring_for_Children*0.435771) + 18.880603
newdata$LL_Playing <- (newdata$Caring_for_Children*0.4182614) + 15.3898607

lines(newdata$Caring_for_Children,newdata$UL_Playing,col="green")
lines(newdata$Caring_for_Children,newdata$LL_Playing,col="green")


nrow(newdata[newdata$Playing_with_Children!=0,])
newdata$Corrected_Playing_with_Children <- ifelse(newdata$Playing_with_Children != 0 & newdata$Playing_with_Children <= newdata$UL_Playing & newdata$Playing_with_Children >= newdata$LL_Playing, 0, newdata$Playing_with_Children)
nrow(newdata[newdata$Corrected_Playing_with_Children!=0,])
newdata$Corrected_Playing_with_Children <- ifelse(newdata$Playing_with_Children != 0 & newdata$Playing_with_Children > newdata$UL_Playing, newdata$Playing_with_Children - newdata$Mean_Playing, newdata$Corrected_Playing_with_Children)
nrow(newdata[newdata$Corrected_Playing_with_Children!=0,])
newdata$Corrected_Playing_with_Children <- ifelse(newdata$Playing_with_Children != 0 & newdata$Playing_with_Children < newdata$LL_Playing, newdata$Playing_with_Children, newdata$Corrected_Playing_with_Children)
nrow(newdata[newdata$Corrected_Playing_with_Children!=0,])

newdata$Corrected_Playing_with_Children <- ceiling(newdata$Corrected_Playing_with_Children)

data1$UL_Playing <- (data1$Caring_for_Children*0.4532799) + 22.3713459
data1$Mean_Playing <- (data1$Caring_for_Children*0.435771) + 18.880603
data1$LL_Playing <- (data1$Caring_for_Children*0.4182614) + 15.3898607

nrow(data1[data1$Playing_with_Children!=0,])
data1$Corrected_Playing_with_Children <- ifelse(data1$Playing_with_Children != 0 & data1$Playing_with_Children <= data1$UL_Playing & data1$Playing_with_Children >= data1$LL_Playing, 0, data1$Playing_with_Children)
nrow(data1[data1$Corrected_Playing_with_Children!=0,])
data1$Corrected_Playing_with_Children <- ifelse(data1$Playing_with_Children != 0 & data1$Playing_with_Children > data1$UL_Playing, data1$Playing_with_Children - data1$Mean_Playing, data1$Corrected_Playing_with_Children)
nrow(data1[data1$Corrected_Playing_with_Children!=0,])
data1$Corrected_Playing_with_Children <- ifelse(data1$Playing_with_Children != 0 & data1$Playing_with_Children < data1$LL_Playing, data1$Playing_with_Children, data1$Corrected_Playing_with_Children)
nrow(data1[data1$Corrected_Playing_with_Children!=0,])

data1$Corrected_Playing_with_Children <- ceiling(data1$Corrected_Playing_with_Children)

# Now for housework and food&drink prep. Food&drink prep can be counted as housework but not all housework has to do with food&drink prep

# Because we want to study the relationship between Housework and Food&Drink Prep, we should exclude those who do not participate in Food&Drink Prep

newdata3 <- newdata[newdata$`Food _Drink_Prep`!=0 ,]
acf(newdata3$`Food _Drink_Prep`)
model2 = lm(newdata3$`Food _Drink_Prep` ~ newdata3$Housework)
summary(model2)
confint(model2, level = 0.95)
plot(newdata3$Housework, newdata3$`Food _Drink_Prep`)
abline(model1, col = 'red')
coeftest(model2, vcov. = NeweyWest(model2,lag = NULL))
confint(model2, vcov = NeweyWest(model2, lag = NULL),level = 0.95)

newdata$UL_FoodDrinkPrep <- (newdata$`Food _Drink_Prep`*0.1158568) + 53.1969622
newdata$Mean_FoodDrinkPrep <- (newdata$`Food _Drink_Prep`*0.1088764) + 52.5404661
newdata$LL_FoodDrinkPrep <- (newdata$`Food _Drink_Prep`*0.1018959) + 51.8839701

nrow(newdata[newdata$`Food _Drink_Prep`!=0,])
newdata$Corrected_FoodDrinkPrep <- ifelse(newdata$`Food _Drink_Prep` != 0 & newdata$`Food _Drink_Prep` <= newdata$UL_FoodDrinkPrep & newdata$`Food _Drink_Prep` >= newdata$LL_FoodDrinkPrep, 0, newdata$`Food _Drink_Prep`)
nrow(newdata[newdata$Corrected_FoodDrinkPrep != 0,])
newdata$Corrected_FoodDrinkPrep <- ifelse(newdata$`Food _Drink_Prep` != 0 & newdata$`Food _Drink_Prep` > newdata$UL_FoodDrinkPrep, newdata$`Food _Drink_Prep` - newdata$Mean_FoodDrinkPrep, newdata$Corrected_FoodDrinkPrep)
nrow(newdata[newdata$Corrected_FoodDrinkPrep!=0,])
newdata$Corrected_FoodDrinkPrep <- ifelse(newdata$`Food _Drink_Prep` != 0 & newdata$`Food _Drink_Prep` < newdata$LL_FoodDrinkPrep, newdata$`Food _Drink_Prep`, newdata$Corrected_FoodDrinkPrep)
nrow(newdata[newdata$Corrected_FoodDrinkPrep!=0,])

newdata$Corrected_FoodDrinkPrep <- ceiling(newdata$Corrected_FoodDrinkPrep)

data1$UL_FoodDrinkPrep <- (data1$`Food _Drink_Prep`*0.1158568) + 53.1969622
data1$Mean_FoodDrinkPrep <- (data1$`Food _Drink_Prep`*0.1088764) + 52.5404661
data1$LL_FoodDrinkPrep <- (data1$`Food _Drink_Prep`*0.1018959) + 51.8839701

nrow(data1[data1$`Food _Drink_Prep`!=0,])
data1$Corrected_FoodDrinkPrep <- ifelse(data1$`Food _Drink_Prep` != 0 & data1$`Food _Drink_Prep` <= data1$UL_FoodDrinkPrep & data1$`Food _Drink_Prep` >= data1$LL_FoodDrinkPrep, 0, data1$`Food _Drink_Prep`)
nrow(data1[data1$Corrected_FoodDrinkPrep != 0,])
data1$Corrected_FoodDrinkPrep <- ifelse(data1$`Food _Drink_Prep` != 0 & data1$`Food _Drink_Prep` > data1$UL_FoodDrinkPrep, data1$`Food _Drink_Prep` - data1$Mean_FoodDrinkPrep, data1$Corrected_FoodDrinkPrep)
nrow(data1[data1$Corrected_FoodDrinkPrep!=0,])
data1$Corrected_FoodDrinkPrep <- ifelse(data1$`Food _Drink_Prep` != 0 & data1$`Food _Drink_Prep` < data1$LL_FoodDrinkPrep, data1$`Food _Drink_Prep`, data1$Corrected_FoodDrinkPrep)
nrow(data1[data1$Corrected_FoodDrinkPrep!=0,])

data1$Corrected_FoodDrinkPrep <- ceiling(data1$Corrected_FoodDrinkPrep)

# Now for Socializing&Relaxing and Television. Television can be counted as Socializing&Relaxing but not all Socializing&Relaxing has to do with Television

# Because we want to study the relationship between Socializing&Relaxing and Television, we should exclude those who do not participate in Television
newdata4 <- newdata[newdata$Television!=0 ,]
acf(newdata4$Television)
model3 = lm(newdata4$Television ~ newdata4$Socializing_Relaxing)
summary(model3)
confint(model3, level = 0.95)
plot(newdata4$Socializing_Relaxing, newdata4$Television)
abline(model1, col = 'red')
coeftest(model3, vcov. = NeweyWest(model3,lag = NULL))
confint(model3, vcov = NeweyWest(model2, lag = NULL),level = 0.95)

newdata$UL_TV <- (newdata$Television*0.5266691) + 37.6805498
newdata$Mean_TV <- (newdata$Television*0.5208844) + 35.6209855
newdata$LL_TV <- (newdata$Television*0.5150998) + 33.5614213

nrow(newdata[newdata$Television!=0,])
newdata$Corrected_TV <- ifelse(newdata$Television != 0 & newdata$Television <= newdata$UL_TV & newdata$Television >= newdata$LL_TV, 0, newdata$Television)
nrow(newdata[newdata$Corrected_TV != 0,])
newdata$Corrected_TV <- ifelse(newdata$Television != 0 & newdata$Television > newdata$UL_TV, newdata$Television - newdata$Mean_TV, newdata$Corrected_TV)
nrow(newdata[newdata$Corrected_TV!=0,])
newdata$Corrected_TV <- ifelse(newdata$Television != 0 & newdata$Television < newdata$LL_TV, newdata$Television, newdata$Corrected_TV)
nrow(newdata[newdata$Corrected_TV!=0,])

newdata$Corrected_TV <- ceiling(newdata$Corrected_TV)

newdata$Corrected_Total <- (newdata$Sleeping + newdata$Grooming + newdata$Housework + newdata$Corrected_FoodDrinkPrep + newdata$Caring_for_Children + newdata$Corrected_Playing_with_Children + newdata$Job_Searching + newdata$Shopping + newdata$Eating_and_Drinking + newdata$Socializing_Relaxing + newdata$Corrected_TV + newdata$Golfing + newdata$Running + newdata$Volunteering)/60
newdata$Corrected_Total <- round(newdata$Corrected_Total, digits = 2)

data1$UL_TV <- (data1$Television*0.5266691) + 37.6805498
data1$Mean_TV <- (data1$Television*0.5208844) + 35.6209855
data1$LL_TV <- (data1$Television*0.5150998) + 33.5614213

nrow(data1[data1$Television!=0,])
data1$Corrected_TV <- ifelse(data1$Television != 0 & data1$Television <= data1$UL_TV & data1$Television >= data1$LL_TV, 0, data1$Television)
nrow(data1[data1$Corrected_TV != 0,])
data1$Corrected_TV <- ifelse(data1$Television != 0 & data1$Television > data1$UL_TV, data1$Television - data1$Mean_TV, data1$Corrected_TV)
nrow(data1[data1$Corrected_TV!=0,])
data1$Corrected_TV <- ifelse(data1$Television != 0 & data1$Television < data1$LL_TV, data1$Television, data1$Corrected_TV)
nrow(data1[data1$Corrected_TV!=0,])

data1$Corrected_TV <- ceiling(data1$Corrected_TV)

data1$Corrected_Total <- (data1$Sleeping + data1$Grooming + data1$Housework + data1$Corrected_FoodDrinkPrep + data1$Caring_for_Children + data1$Corrected_Playing_with_Children + data1$Job_Searching + data1$Shopping + data1$Eating_and_Drinking + data1$Socializing_Relaxing + data1$Corrected_TV + data1$Golfing + data1$Running + data1$Volunteering)/60
data1$Corrected_Total <- round(data1$Corrected_Total, digits = 2)


# Interesting Patterns

max(newdata$Corrected_Total)
nrow(newdata[newdata$Corrected_Total > 24,])
nrow(newdata[newdata$Corrected_Total > 24 & newdata$Children != 0 & newdata$Sleeping >= 480,])
nrow(newdata[newdata$Corrected_Total > 24 & newdata$Children != 0 & newdata$Socializing_Relaxing >= 360,])

nrow(newdata[((newdata$Sleeping + newdata$Socializing_Relaxing)/60) >= 20,])
study_subset1 <- newdata[((newdata$Sleeping + newdata$Socializing_Relaxing)/60) >= 20,]

nrow(newdata[newdata$Children != 0 & newdata$Caring_for_Children == 0,])
nrow(newdata[newdata$Children != 0 & newdata$Playing_with_Children == 0,])

nrow(newdata[newdata$Children != 0 & newdata$Caring_for_Children == 0 & newdata$Age >= 15 & newdata$Age <= 50 & (newdata$Employment_Status == 'Unemployed' | newdata$Employment_Status == 'Not in labor force'),])
study_subset2 <- newdata[newdata$Children != 0 & newdata$Caring_for_Children == 0 & newdata$Age >= 15 & newdata$Age <= 50 & (newdata$Employment_Status == 'Unemployed' | newdata$Employment_Status == 'Not in labor force') & newdata$Sleeping >= quantile(newdata$Sleeping, 0.98),]

# Using Weekly_Hours_Worked to determine sleep time
corrplot(cor(newdata[,-c('UL_Socializing','LL_Socializing','Mean_Socializing','Id', 'Education_Level', 'Age_Range', 'Employment_Status', 'Gender', 'Year' ,'UL_Playing', 'Mean_Playing', 'LL_Playing', 'UL_FoodDrinkPrep', 'Mean_FoodDrinkPrep', 'LL_FoodDrinkPrep', 'UL_TV', 'Mean_TV', 'LL_TV', 'Total', 'Corrected_Total', 'UL_Sleeping', 'Mean_Sleeping', 'LL_Sleeping')]),type = "lower",diag = FALSE, addCoef.col = "black")
colnames(newdata)

acf(newdata$Sleeping)
subset1 <- newdata[newdata$Weekly_Hours_Worked!=0,]
model4 <- lm(subset1$Sleeping ~ subset1$Weekly_Hours_Worked)
summary(model4)
confint(model4, level = 0.95)
plot(subset1$Weekly_Hours_Worked, subset1$Sleeping)
abline(model4, col = 'red')
coeftest(model4, vcov. = NeweyWest(model4,lag = NULL))
confint(model4, vcov = NeweyWest(model4, lag = NULL),level = 0.95)

newdata$UL_Sleeping <- (newdata$Weekly_Hours_Worked*-1.072797) + 564.035675
newdata$Mean_Sleeping <- (newdata$Weekly_Hours_Worked*-1.200297) + 558.708995
newdata$LL_Sleeping <- (newdata$Weekly_Hours_Worked*-1.327797) + 553.382316

nrow(newdata[newdata$Sleeping!=0,])
newdata$Corrected_Sleeping <- ifelse(newdata$Sleeping <= newdata$UL_Sleeping & newdata$Weekly_Hours_Worked != 0, newdata$Sleeping, ifelse(newdata$Sleeping > newdata$UL_Sleeping & newdata$Weekly_Hours_Worked != 0, newdata$UL_Sleeping, newdata$Sleeping))

newdata$Corrected_Sleeping <- ceiling(newdata$Corrected_Sleeping)

newdata$Corrected_Total <- (newdata$Corrected_Sleeping + newdata$Grooming + newdata$Housework + newdata$Corrected_FoodDrinkPrep + newdata$Caring_for_Children + newdata$Corrected_Playing_with_Children + newdata$Job_Searching + newdata$Shopping + newdata$Eating_and_Drinking + newdata$Socializing_Relaxing + newdata$Corrected_TV + newdata$Golfing + newdata$Running + newdata$Volunteering)/60
newdata$Corrected_Total <- round(newdata$Corrected_Total, digits = 2)

data1$UL_Sleeping <- (data1$Weekly_Hours_Worked*-1.072797) + 564.035675
data1$Mean_Sleeping <- (data1$Weekly_Hours_Worked*-1.200297) + 558.708995
data1$LL_Sleeping <- (data1$Weekly_Hours_Worked*-1.327797) + 553.382316

nrow(data1[data1$Sleeping!=0,])
data1$Corrected_Sleeping <- ifelse(data1$Sleeping <= data1$UL_Sleeping & data1$Weekly_Hours_Worked != 0, data1$Sleeping, ifelse(data1$Sleeping > data1$UL_Sleeping & data1$Weekly_Hours_Worked != 0, data1$UL_Sleeping, data1$Sleeping))

data1$Corrected_Sleeping <- ceiling(data1$Corrected_Sleeping)

data1$Corrected_Total <- (data1$Corrected_Sleeping + data1$Grooming + data1$Housework + data1$Corrected_FoodDrinkPrep + data1$Caring_for_Children + data1$Corrected_Playing_with_Children + data1$Job_Searching + data1$Shopping + data1$Eating_and_Drinking + data1$Socializing_Relaxing + data1$Corrected_TV + data1$Golfing + data1$Running + data1$Volunteering)/60
data1$Corrected_Total <- round(data1$Corrected_Total, digits = 2)


nrow(newdata[newdata$Corrected_Total > 24,])
study_subset3 <- newdata[newdata$Corrected_Total > 24,]
colnames(study_subset3)
study_subset3 <- study_subset3[,-c('Sleeping','Playing_with_Children','Food _Drink_Prep','Television','UL_Playing','Mean_Playing', 'LL_Playing', 'UL_FoodDrinkPrep', 'Mean_FoodDrinkPrep','LL_FoodDrinkPrep','UL_TV','Mean_TV','LL_TV','UL_Sleeping','Mean_Sleeping','LL_Sleeping')]
summary(study_subset3)
study_subset3[study_subset3$Corrected_Total == 28.92,]

# Using Weekly_Hours_Worked to determine Socializing & Relaxing

subset2 <- newdata[newdata$Weekly_Hours_Worked != 0 | newdata$Socializing_Relaxing != 0,]
model5 <- lm(subset2$Socializing_Relaxing ~ subset2$Weekly_Hours_Worked)
summary(model5)
confint(model5, level = 0.95)
plot(subset2$Weekly_Hours_Worked, subset2$Socializing_Relaxing)
abline(model5, col = 'red')
coeftest(model5, vcov. = NeweyWest(model5,lag = NULL))
confint(model5, vcov = NeweyWest(model5, lag = NULL),level = 0.95)

newdata$UL_Socializing <- (newdata$Weekly_Hours_Worked*-3.028677) + 368.119347
newdata$Mean_Socializing <- (newdata$Weekly_Hours_Worked*-3.102957) + 365.693796
newdata$LL_Socializing <- (newdata$Weekly_Hours_Worked*-3.177238) + 363.268245

newdata$Corrected_Socializing <- ifelse(newdata$Socializing_Relaxing <= newdata$UL_Socializing & newdata$Weekly_Hours_Worked != 0, newdata$Socializing_Relaxing, ifelse(newdata$Socializing_Relaxing > newdata$UL_Socializing & newdata$Weekly_Hours_Worked != 0, newdata$UL_Socializing, newdata$Socializing_Relaxing))
newdata$Corrected_Socializing <- ceiling(newdata$Corrected_Socializing)

newdata$Corrected_Total <- (newdata$Corrected_Sleeping + newdata$Grooming + newdata$Housework + newdata$Corrected_FoodDrinkPrep + newdata$Caring_for_Children + newdata$Corrected_Playing_with_Children + newdata$Job_Searching + newdata$Shopping + newdata$Eating_and_Drinking + newdata$Corrected_Socializing + newdata$Corrected_TV + newdata$Golfing + newdata$Running + newdata$Volunteering)/60
newdata$Corrected_Total <- round(newdata$Corrected_Total, digits = 2)

data1$UL_Socializing <- (data1$Weekly_Hours_Worked*-3.028677) + 368.119347
data1$Mean_Socializing <- (data1$Weekly_Hours_Worked*-3.102957) + 365.693796
data1$LL_Socializing <- (data1$Weekly_Hours_Worked*-3.177238) + 363.268245

data1$Corrected_Socializing <- ifelse(data1$Socializing_Relaxing <= data1$UL_Socializing & data1$Weekly_Hours_Worked != 0, data1$Socializing_Relaxing, ifelse(data1$Socializing_Relaxing > data1$UL_Socializing & data1$Weekly_Hours_Worked != 0, data1$UL_Socializing, data1$Socializing_Relaxing))
data1$Corrected_Socializing <- ceiling(data1$Corrected_Socializing)

data1$Corrected_Total <- (data1$Corrected_Sleeping + data1$Grooming + data1$Housework + data1$Corrected_FoodDrinkPrep + data1$Caring_for_Children + data1$Corrected_Playing_with_Children + data1$Job_Searching + data1$Shopping + data1$Eating_and_Drinking + data1$Corrected_Socializing + data1$Corrected_TV + data1$Golfing + data1$Running + data1$Volunteering)/60
data1$Corrected_Total <- round(data1$Corrected_Total, digits = 2)


nrow(newdata[newdata$Corrected_Total > 24,])

study_subset4 <- newdata[newdata$Corrected_Total > 24,]
colnames(study_subset4)
study_subset4 <- study_subset4[,-c('Socializing_Relaxing','UL_Socializing','LL_Socializing', 'Mean_Socializing','Sleeping','Playing_with_Children','Food _Drink_Prep','Television','UL_Playing','Mean_Playing', 'LL_Playing', 'UL_FoodDrinkPrep', 'Mean_FoodDrinkPrep','LL_FoodDrinkPrep','UL_TV','Mean_TV','LL_TV','UL_Sleeping','Mean_Sleeping','LL_Sleeping')]
summary(study_subset4)
study_subset4[study_subset4$Corrected_Total == 28.78,]


# Using Age, Number of Children and Weekly Hours Worked to determine Caring for Children

# model6 <- lm(newdata$Caring_for_Children ~ newdata$Age + newdata$Children + newdata$Weekly_Hours_Worked)
# summary(model6)
# confint(model6, level = 0.95)
# coeftest(model6, vcov. = NeweyWest(model6,lag = NULL))
# confint(model6, vcov = NeweyWest(model6, lag = NULL),level = 0.95)
# 
# newdata$UL_Caring <- (newdata$Weekly_Hours_Worked*-0.1395882 + newdata$Children*25.0677057 + newdata$Age*-0.2696688) + 27.9694117
# newdata$Mean_Caring <- (newdata$Weekly_Hours_Worked*-0.167225 + newdata$Children*24.510294 + newdata$Age*-0.307253) + 25.618407
# newdata$LL_Caring <- (newdata$Weekly_Hours_Worked*-0.1948614 + newdata$Children*23.9528818 + newdata$Age*-0.3448382) + 23.2674033
# 
# newdata$Corrected_Caring <- ifelse(newdata$Caring_for_Children <= newdata$UL_Caring & newdata$Children != 0, newdata$Caring_for_Children, ifelse(newdata$Caring_for_Children > newdata$UL_Caring & newdata$Children != 0, newdata$UL_Caring, newdata$Caring_for_Children))
# newdata$Corrected_Caring <- ceiling(newdata$Corrected_Caring)
# 
# newdata$Corrected_Total <- (newdata$Corrected_Sleeping + newdata$Grooming + newdata$Housework + newdata$Corrected_FoodDrinkPrep + newdata$Corrected_Caring + newdata$Corrected_Playing_with_Children + newdata$Job_Searching + newdata$Shopping + newdata$Eating_and_Drinking + newdata$Corrected_Socializing + newdata$Corrected_TV + newdata$Golfing + newdata$Running + newdata$Volunteering)/60
# newdata$Corrected_Total <- round(newdata$Corrected_Total, digits = 2)
# 
# nrow(newdata[newdata$Corrected_Total > 24,])
# study_subset5 <- newdata[newdata$Corrected_Total > 24,]
# colnames(study_subset5)
# study_subset5 <- study_subset5[,-c('Caring_for_Children','LL_Caring','Mean_Caring','UL_Caring','Socializing_Relaxing','UL_Socializing','LL_Socializing', 'Mean_Socializing','Sleeping','Playing_with_Children','Food _Drink_Prep','Television','UL_Playing','Mean_Playing', 'LL_Playing', 'UL_FoodDrinkPrep', 'Mean_FoodDrinkPrep','LL_FoodDrinkPrep','UL_TV','Mean_TV','LL_TV','UL_Sleeping','Mean_Sleeping','LL_Sleeping')]
# summary(study_subset5)
# study_subset5[study_subset5$Corrected_Total == 28.37,]

####


final_newdata <- newdata[newdata$Corrected_Total <= 24, -c('Socializing_Relaxing','UL_Socializing','LL_Socializing', 'Mean_Socializing','Sleeping','Playing_with_Children','Food _Drink_Prep','Television','UL_Playing','Mean_Playing', 'LL_Playing', 'UL_FoodDrinkPrep', 'Mean_FoodDrinkPrep','LL_FoodDrinkPrep','UL_TV','Mean_TV','LL_TV','UL_Sleeping','Mean_Sleeping','LL_Sleeping') ]
final_testdata <- data1[data1$Corrected_Total <= 24, -c('Socializing_Relaxing','UL_Socializing','LL_Socializing', 'Mean_Socializing','Sleeping','Playing_with_Children','Food _Drink_Prep','Television','UL_Playing','Mean_Playing', 'LL_Playing', 'UL_FoodDrinkPrep', 'Mean_FoodDrinkPrep','LL_FoodDrinkPrep','UL_TV','Mean_TV','LL_TV','UL_Sleeping','Mean_Sleeping','LL_Sleeping') ]

# Printing out in CSV format
write.csv(final_newdata, file = "final_newdata.csv")
write.csv(final_testdata, file = "final_testdata.csv")


####### MACHINE - LEARNING ######

rm(list = ls())

setwd("~/Desktop/IAS")

library(data.table)

context1 <- fread('combined_newdata.csv', header = TRUE)

context1 <- context1[,-c('Id', 'Total', 'Hourly_Earnings')]

head(context1, 10)

str(context1)

context1$Education_Level <- as.factor(context1$Education_Level)
context1$Age_Range <- as.factor(context1$Age_Range)
context1$Employment_Status <- as.factor(context1$Employment_Status)
context1$Gender <- as.factor(context1$Gender)

str(context1)

table(context1$Gender)/length(context1$Gender)
pie(table(context1$Gender))
barplot(table(context1$Gender))

# KNN - K nearest neighbor (to find common characteristics between employment status)
table(context1$Employment_Status)

# Ensuring that the employment status is mixed up
head(context1)

colnames(context1)
# Because KNN uses distance of each point, we have to normalize to remove any undue influence
normalize <- function(x){
  return( (x - min(x))/(max(x) - min(x)))
}

context2 <- as.data.frame(lapply(context1[,10:23], normalize))
summary(context2)

data_train <- context2[1:43420,]
data_test <- context2[43421:76690,]
data_train_target <- context1[1:43420,4]
data_test_target <- context1[43421:76690,4]

library(class)
sqrt(76690)
k <- 277
length(data_train_target)
length(data_train)

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  model1 <- knn(train = data_train, test = data_test, cl=as.factor(unlist(data_train_target)), k = 277)
  
}
close(pb)

# These are the predictions for the test dataset
model1

write.csv(model1, 'predictions.csv')

context1 <- fread('initial_newdata.csv', header = TRUE)

library(caret)

str(context1)

context1 <- context1[,-c('V1', 'Id', 'Age_Range', 'Total', 'Hourly_Earnings', 'Education_Level', 'Age', 'Children', 'Gender', 'Weekly_Earnings', 'Weekly_Hours_Worked', 'Year')]

str(context1)

context1$Employment_Status <- as.factor(context1$Employment_Status)

training <- context1

context2 <- fread('Test.csv', header = TRUE)

context2 <- context2[,-c('id', 'Age_Range', 'Total', 'Education_Level', 'Age', 'Children', 'Gender', 'Weekly_Earnings', 'Weekly_Hours_Worked', 'Year')]

context2$Employment_Status <- as.factor(context2$Employment_Status)

testing <- context2

dim(training)
# [1] 43420    15
dim(testing)
# [1] 33270    15

# Checking for NaN values just in case
anyNA(context1)

summary(context1)
str(training)
str(testing)

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  trctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
  set.seed(3333)
  knn_fit <- train(Employment_Status ~., data = training, method = 'knn', trControl = trctrl, preProcess = c('center','scale'), tuneLength = 10)
  
}
close(pb)

knn_fit
# k-Nearest Neighbors 
# 
# 43420 samples
# 14 predictor
# 3 classes: 'Employed', 'Not in labor force', 'Unemployed' 
# 
# Pre-processing: centered (14), scaled (14) 
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 39078, 39079, 39078, 39079, 39078, 39078, ... 
# Resampling results across tuning parameters:
#   
#   k   Accuracy   Kappa    
# 5  0.6516815  0.2873060
# 7  0.6617995  0.3031023
# 9  0.6655843  0.3084299
# 11  0.6689852  0.3132140
# 13  0.6711732  0.3164040
# 15  0.6724934  0.3185967
# 17  0.6738753  0.3207379
# 19  0.6751879  0.3229723
# 21  0.6760324  0.3243231
# 23  0.6775985  0.3275279
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 23.

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  test_pred <- predict(knn_fit, newdata = testing)
  
}
close(pb)

test_pred

write.csv(test_pred, 'predictions_knn_cv.csv')

# Using GBM:

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), interaction.depth = c(1, 3, 7, 10), n.minobsinnode = c(2, 5, 10), n.trees = c(100, 300, 500, 1000))
  
  gbm.fit <- train(Employment_Status ~., data = training, method = 'gbm', trControl = trctrl, tuneGrid = tg, verbose = FALSE)
  
}
close(pb)

gbm.fit

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  test_pred1 <- predict(gbm.fit, newdata = testing)
  
}
close(pb)

test_pred1

# Using C5.0:

library(mlbench)

set.seed(3333)
c50.fit <- train(Employment_Status ~., data = training, method = 'C5.0', metric = 'Accuracy', trControl = trctrl)

c50.fit

# C5.0 
# 
# 43420 samples
# 14 predictor
# 3 classes: 'Employed', 'Not in labor force', 'Unemployed' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 39078, 39079, 39078, 39079, 39078, 39078, ... 
# Resampling results across tuning parameters:
#   
# model  winnow  trials   Accuracy   Kappa    
# rules  FALSE    1      0.6830339  0.3363769
# rules  FALSE   10      0.6887223  0.3543402
# rules  FALSE   20      0.6920234  0.3637557
# rules   TRUE    1      0.6746659  0.3116469
# rules   TRUE   10      0.6696838  0.2816410
# rules   TRUE   20      0.6696838  0.2816410
# tree   FALSE    1      0.6733225  0.3282682
# tree   FALSE   10      0.6838707  0.3434268
# tree   FALSE   20      0.6890909  0.3524005
# tree    TRUE    1      0.6740135  0.3131108
# tree    TRUE   10      0.6747351  0.3128579
# tree    TRUE   20      0.6746660  0.3126396
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 20, model = rules and winnow = FALSE.

test_pred2 <- predict(c50.fit, newdata = testing)
test_pred2

write.csv(test_pred2, 'predictions_c50.csv')

# c5.0 with pre-processing
set.seed(3333)

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  trctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
  c501.fit <- train(Employment_Status ~., data = training, method = 'C5.0', metric = 'Accuracy', trControl = trctrl, preProcess = c('center','scale'))
  
}
close(pb)

c501.fit

# 43420 samples
# 14 predictor
# 3 classes: 'Employed', 'Not in labor force', 'Unemployed' 
# 
# Pre-processing: centered (14), scaled (14) 
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 39077, 39079, 39078, 39078, 39078, 39079, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.6832950  0.3363031
# rules  FALSE   10      0.6868880  0.3470048
# rules  FALSE   20      0.6916246  0.3601479
# rules   TRUE    1      0.6739825  0.3085046
# rules   TRUE   10      0.6673569  0.2669852
# rules   TRUE   20      0.6675642  0.2680222
# tree   FALSE    1      0.6738062  0.3287068
# tree   FALSE   10      0.6816216  0.3349699
# tree   FALSE   20      0.6875403  0.3476412
# tree    TRUE    1      0.6739212  0.3124421
# tree    TRUE   10      0.6723472  0.3006144
# tree    TRUE   20      0.6723472  0.3006144
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 20, model = rules and winnow = FALSE.

plot(c501.fit)
varImp(c501.fit)
# C5.0 variable importance
# 
# Overall
# Television             100.00
# Job_Searching          100.00
# Socializing_Relaxing   100.00
# Sleeping               100.00
# `Food_Drink_Prep`       99.89
# Caring_for_Children     99.80
# Housework               99.70
# Grooming                99.70
# Eating_and_Drinking     99.63
# Shopping                98.65
# Volunteering            93.99
# Running                 88.51
# Playing_with_Children   84.58
# Golfing                 66.65

varImp(c501.fit, metric = 'splits')

# C5.0 variable importance
# 
# Overall
# Socializing_Relaxing  100.000
# Sleeping               82.289
# `Food                  65.200
# Eating_and_Drinking    60.257
# Caring_for_Children    52.627
# Housework              46.555
# Grooming               41.845
# Job_Searching          39.471
# Shopping               35.422
# Television             30.323
# Volunteering           22.071
# Running                14.402
# Playing_with_Children  12.534
# Golfing                 2.997

summary(c501.fit)

# Evaluation on training data (43420 cases):
#   
#   Trial	        Rules     
# -----	  ----------------
#   No      Errors
# 
# 0	   122 12208(28.1%)
# 1	    82 13062(30.1%)
# 2	    78 14116(32.5%)
# 3	    90 13502(31.1%)
# 4	    91 14106(32.5%)
# 5	    96 13944(32.1%)
# 6	    87 14937(34.4%)
# 7	    99 14931(34.4%)
# 8	    93 14650(33.7%)
# 9	   101 14973(34.5%)
# 10	    71 15157(34.9%)
# 11	    69 14786(34.1%)
# 12	    83 15749(36.3%)
# 13	   127 15281(35.2%)
# 14	   124 13862(31.9%)
# 15	   189 13625(31.4%)
# 16	   135 13210(30.4%)
# 17	   184 12352(28.4%)
# 18	   217 12228(28.2%)
# 19	   245 12053(27.8%)
# boost	      10484(24.1%)   <<
#   
#   
#   (a)   (b)   (c)    <-classified as
# ----  ----  ----
#   22986  3251    86    (a): class Employed
# 5076  9549    59    (b): class Not in labor force
# 1306   706   401    (c): class Unemployed
# 
# 
# Attribute usage:
#   
# 100.00%	Job_Searching
# 100.00%	Socializing_Relaxing
# 100.00%	Television
# 100.00%	Sleeping
# 99.89%	`Food _Drink_Prep`
# 99.80%	Caring_for_Children
# 99.70%	Housework
# 99.70%	Grooming
# 99.63%	Eating_and_Drinking
# 98.65%	Shopping
# 93.99%	Volunteering
# 88.51%	Running
# 84.58%	Playing_with_Children
# 66.65%	Golfing

summary(c501.fit)

write(capture.output(summary(c501.fit)), "c50model.txt")

test_pred3 <- predict(c501.fit, newdata = testing, preProcess = c('center','scale'))
test_pred3

write.csv(test_pred3, 'predictions_c50_preprocess.csv')

# Using Stacking:
  
library(caretEnsemble)

trctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE)

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)

algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')

set.seed(7777)
models <- caretList(Employment_Status~., data = training, trControl = trctrl, methodlist = algorithmList)

}
close(pb)

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)

set.seed(7777)
rf.stack <- caretStack(models, method = 'rf', metric = 'Accuracy', trControl = trctrl)

}
close(pb)

test_pred4 <- predict(rf.stack, newdata = testing)
test_pred4

write.csv(test_pred4, 'stacked_ensemble.csv')

# Setting the working directory
getwd()
setwd("D:/New folder/Fwd__IAS-_Intelligence_Analytics_Challenge_3.0_-_Let's_Begin")

# Loading the required libraries
library(caret)
library(dummies) # for dummies
library(class) # for knn
library(e1071) # for SVM
library(randomForest) # for Randomforest
library(pROC) # For ROC and AUC
library(rminer) 

# Getting the data
data = read.csv('final_newdata.csv')
colnames(data)
# No NAs
sum(is.na(data))

# [1] "X"                               "Id"                             
# [3] "Education_Level"                 "Age"                            
# [5] "Age_Range"                       "Employment_Status"              
# [7] "Gender"                          "Children"                       
# [9] "Weekly_Earnings"                 "Year"                           
# [11] "Weekly_Hours_Worked"             "Grooming"                       
# [13] "Housework"                       "Caring_for_Children"            
# [15] "Job_Searching"                   "Shopping"                       
# [17] "Eating_and_Drinking"             "Golfing"                        
# [19] "Running"                         "Volunteering"                   
# [21] "Total"                           "Hourly_Earnings"                
# [23] "Corrected_Playing_with_Children" "Corrected_FoodDrinkPrep"        
# [25] "Corrected_TV"                    "Corrected_Total"                
# [27] "Corrected_Sleeping"              "Corrected_Socializing" 

str(data)

# Dont need X, ID etc fields for predictions
newdata = data[,-c(1,2,5,21,22)]
# Removed the fields:
# X, ID : Not required
# Age_range : Not required when using Age in the model
# Total : is the incorrect total, so not required when
# you have corrected version of it
# Removing Hourly wages variable as well
# since we have weekly and hrs per week

colnames(newdata)
str(newdata)

# Let us create dummy variables for 2 factor variables we have
newdata$Gender = ifelse(newdata$Gender == 'Female',0,1)
Edu_level = dummy(newdata$Education_Level)
newdata = cbind(newdata,Edu_level[,1:3])

# removing Education_level
newdata = newdata[,-1]
colnames(newdata)
str(newdata)

# Creating data partitions for train and test
set.seed(1601)
sample = createDataPartition(data$Employment_Status,
                             times = 4,
                             p = 0.75)

knn_con = list()

for(i in 1:4) {
  train = newdata[sample[[i]],]
  test = newdata[-sample[[i]],]
  
  # Let us do scaling for Knn
  train_scale = scale(train[,-2]) # Excluding response
  test_scale = scale(test[,-2]) # Excluding response
  
  set.seed(1601)
  pred_knn = knn(train = train_scale,
                 test = test_scale,
                 cl = train[,2],
                 k = 200,
                 prob = TRUE)
  
  knn_con[[i]] = confusionMatrix(data = pred_knn, reference = test[,2])
}
knn_con

multiclass.roc(response = test[,2],
               predictor = as.numeric(pred_knn))

# Area Under the curve 80%

# Used k = 200, since 200 will be
# the sqrt value of total 40000 records

# Some amount of confusion in predicting
# unemployed people...but thats okay.
# Performance not good on unemployed 
# predictions, may be because of less samples 
# in the data of this class

# Otherwise, Good predictions, precisions and recalls



###################### SVM
colnames(train)

svm_con = list()

for(i in 1:4){
  
  train = newdata[sample[[i]],]
  test = newdata[-sample[[i]],]
  
  train_scale = scale(train[,-2]) # Excluding response
  test_scale = scale(test[,-2]) # Excluding response
  
  train_svm = cbind(train_scale,train$Employment_Status)
  test_svm = cbind(test_scale,test$Employment_Status)
  
  train_svm = as.data.frame(train_svm)
  test_svm = as.data.frame(test_svm)
  
  colnames(train_svm)[25] = 'Employment_Status'
  colnames(test_svm)[25] = 'Employment_Status'
  
  train_svm$Employment_Status = as.factor(train_svm$Employment_Status)
  test_svm$Employment_Status = as.factor(test_svm$Employment_Status)
  
  set.seed(1601)
  sv_model = svm(Employment_Status~., data = train_svm, kernel = "radial")
  sv_model
  summary(sv_model)
  
  pred_sv = predict(sv_model,test_svm)
  
  svm_con[[i]] = confusionMatrix(reference = test_svm$Employment_Status, data = pred_sv)
  
}

svm_con
multiclass.roc(response = test_svm$Employment_Status,
               predictor = as.numeric(pred_sv))

# tuning my model
# tune.out = tune(method = svm,
#                 train.x = Employment_Status~.,
#                 data = train_svm,
#                 kernel = "radial",
#                 ranges = list(cost = 2^seq(-5,15,2),
#                               gamma = 2^seq(-15,3,2)))
# 
# tune.out
# summary(tune.out$best.model)
# best = tune.out$best.model

###################### Random Forest

rf_con = list()

for(i in  1:4){
  
  train = newdata[sample[[i]],]
  test = newdata[-sample[[i]],]
  
  train_scale = scale(train[,-2]) # Excluding response
  test_scale = scale(test[,-2]) # Excluding response
  
  train_rf = cbind(train_scale,train$Employment_Status)
  test_rf = cbind(test_scale,test$Employment_Status)
  
  train_rf = as.data.frame(train_rf)
  test_rf = as.data.frame(test_rf)
  
  colnames(train_rf)[25] = 'Employment_Status'
  colnames(test_rf)[25] = 'Employment_Status'
  
  train_rf$Employment_Status = as.factor(train_rf$Employment_Status)
  test_rf$Employment_Status = as.factor(test_rf$Employment_Status)
  
  set.seed(1601)
  rf_model = randomForest(x = train_rf[,-25],
                          y = train_rf$Employment_Status,
                          ntree = 500)
  
  # Predicting the Test set results
  pred_rf = predict(rf_model, newdata = test_rf[,-25])
  
  rf_con[[i]] = confusionMatrix(reference = test_rf$Employment_Status, data = pred_rf)
}

rf_con
multiclass.roc(response = test_rf$Employment_Status,
               predictor = as.numeric(pred_rf))   


############################ PREDICTIONS ############################
# Getting the test dataset
final = read.csv('final_testdata.csv')
colnames(final)
# The test data set has only 23095 rows left
# after the cleaning process is done.
# This cleaning process the same as the 
# cleaning process done on the training dataset

ID = final$id
# Storing the Id field in a variable inorder
# to tag the predictions back to the test dataset

# Removing the same fields which are removed when
# creating the model for predictions
# X, ID : Not required
# Age_range : Not required when using Age in the model
# Total : is the incorrect total, so not required when
# you have corrected version of it
# Removing Hourly wages variable as well
# since we have weekly and hrs per week
newfinal = final[,-c(1,2,5,21,22)]
colnames(newfinal)
str(newfinal)

# Let us create dummy variables for 2 factor variables we have
newfinal$Gender = ifelse(newfinal$Gender == 'Female',0,1)
Edu_level = dummy(newfinal$Education_Level)
newfinal_dummy = cbind(newfinal,Edu_level[,1:3])

# Removing the Education level field since
# we have the dummy variables for this variable
newfinal_dummy = newfinal_dummy[,-1]
str(newfinal_dummy)

# Scaling the data set excluding the 
# Employment status field
final_scale = scale(newfinal_dummy[,-2])
str(final_scale)
class(final_scale)

# Converting the matrix to  dataframe
final_scale = as.data.frame(final_scale)
str(final_scale)

# Using the SVM model to generate the predictions 
# on the test dataset
pred_final = predict(sv_model,final_scale)
head(pred_final)
table(pred_final)

Employ_status = ifelse(pred_final == 1,'Employed',
                       ifelse(pred_final == 2,'Not in labor force','Unemployed'))
head(Employ_status)

final$Employment_Status = Employ_status

# Saving the predictions in the workspace
# as Predictions.csv
# Excluding the X variable
write.csv(final[,-1],'Predictions.csv',row.names = FALSE)

# To find the importance of variables
M <- fit(Employment_Status~., 
         data = train_svm, 
         model="svm", 
         C=1,
         gamma = 0.04) 


svm.imp <- Importance(M, data=train_svm)
svm.imp
imp = svm.imp$imp[1:25]
index = colnames(train_svm[1:25])
imp_frame = data.frame(index,imp)

imp_frame = imp_frame[order(imp_frame$imp,decreasing = TRUE),2:1]
imp_frame = imp_frame[,-1]

# Writing the importance of variables in the output
write.csv(imp_frame,'Importance_of_variables.csv',
          row.names = FALSE)