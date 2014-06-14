# Code for OpenIntro Statistics textbook lab 1, accessed at the following URL on 6/14/2014:
# http://www.openintro.org/download.php?file=os2_lab_01A&referrer=/stat/labs.php

source("http://www.openintro.org/stat/data/cdc.R")

names(cdc)

summary(cdc$weight)

# "table" can be used as a "summary" for categorical data
table(cdc$smoke100)

# relative frequency distribution
table(cdc$smoke100)/20000

barplot(table(cdc$smoke100))
# alternatively:
smoke <- table(cdc$smoke100)
barplot(smoke)

summary(cdc$height)
# interquartile range:
70 - 64
table(cdc$gender) / 20000
table(cdc$genhlth) / 20000
barplot(table(cdc$genhlth) / 20000)

100*(table(cdc$gender, cdc$smoke100) / 20000)
mosaicplot(table(cdc$gender, cdc$smoke100))

dim(cdc)
cdc[567,6]
mdata <- subset(cdc, cdc$gender == "m")

summary(cdc$height)
boxplot(cdc$height)
boxplot(cdc$height ~ cdc$gender)

bmi <- (cdc$weight/cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)
boxplot(bmi ~ cdc$gender)
boxplot(bmi ~ cdc$age)

hist(cdc$age)
hist(bmi)
hist(bmi, breaks = 50) # set number of bins

#Make a scatterplot of weight versus desired weight. Describe the relationship between these two variables.
plot(cdc$weight ~ cdc$wtdesire)
plot(cdc$wtdesire ~ cdc$weight)
# positive linear relationship, althought desired weight usually lower than weight

"
Let’s consider a new variable: the difference between desired weight (wtdesire) and current weight
(weight). Create this new variable by subtracting the two columns in the data frame and assigning
them to a new object called wdiff.
"

wdiff = cdc$wtdesire - cdc$weight

"
. What type of data is wdiff? If an observation wdiff is 0, what does this mean about the person’s
weight and desired weight. What if wdiff is positive or negative?
"

# integer, comfirm by typeof(wdiff)
# the same
# negative means individual desires to be at a weight lower than their actual weight

"
Describe the distribution of wdiff in terms of its center, shape, and spread, including any plots you
use. What does this tell us about how people feel about their current weight?
"

hist(wdiff, breaks = 50)
summary(wdiff)
sd(wdiff)
boxplot(wdiff)
wdiff_is_pos = wdiff >= 0
table(wdiff_is_pos) / 20000

# most people (~64%) want to be below their current weight - mean = ~ -15 lbs, st.dev. 24lbs
# mostly normal with right skew

"
Using numerical summaries and a side-by-side box plot, determine if men tend to view their weight
differently than women.
"
boxplot(wdiff ~ cdc$gender)
boxplot(wdiff ~ cdc$gender, horizontal = T)
summary(subset(wdiff, cdc$gender=="m"))
summary(subset(wdiff, cdc$gender=="f"))
# women tend to desire a weight that is further below their actual weight than men
# mean for F ~-18 while mean for M ~-11

"
Now it’s time to get creative. Find the mean and standard deviation of weight and determine what
proportion of the weights are within one standard deviation of the mean.
"
mean(cdc$weight)
sd(cdc$weight)
hist(cdc$weight)
# weights are normally distributed around the mean of 170, sd 40
# in a normal distribution, 68.2% of values are within 1 sd of mean

