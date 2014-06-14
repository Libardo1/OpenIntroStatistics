# Code for OpenIntro Statistics textbook lab 2, accessed at the following URL on 6/14/2014:
# http://www.openintro.org/download.php?file=os2_lab_03A&referrer=/stat/labs.php

download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")

head(bdims)

mdims <- subset(bdims, bdims$sex == 1)
fdims <- subset(bdims, bdims$sex == 0)

# Make a histogram of men’s heights and a histogram of women’s heights. How would you compare the various aspects of the two distributions?
hist(mdims$hgt)
hist(fdims$hgt)
# Males normal dist around mean of ~180cms, females normal dist around mean of ~165cms
boxplot(bdims$hgt ~ bdims$sex)

# test normality
fhgtmean <- mean(fdims$hgt)
fhgtsd <- sd(fdims$hgt)
hist(fdims$hgt, probability = TRUE) # plot density histo
x <- 140:190
y <- dnorm(x = x, mean = fhgtmean, sd = fhgtsd)
lines(x = x, y = y, col = "blue")
# test in another way, via Q-Q plot
qqnorm(fdims$hgt)
qqline(fdims$hgt)
# test in a third way, simulate a norm dist and compare
sim_norm <- rnorm(n = length(fdims$hgt), mean = fhgtmean, sd = fhgtsd)
qqnorm(sim_norm)
qqline(sim_norm)
qqnormsim(fdims$hgt) # run a bunch of simulations. function was loaded with data set

# test normality for female weights
fwgtmean <- mean(fdims$wgt)
fwgtsd <- sd(fdims$wgt)
hist(fdims$wgt)
hist(fdims$wgt, probability = TRUE) # plot density histo
x <- 40:120
y <- dnorm(x = x, mean = fwgtmean, sd = fwgtsd)
lines(x = x, y = y, col = "blue") # little right skewed
# test via Q-Q plot
qqnorm(fdims$wgt)
qqline(fdims$wgt)
# test via simulating and comparing
sim_norm <- rnorm(n = length(fdims$wgt), mean = fhgtmean, sd = fwgtsd)
hist(sim_norm)
qqnorm(sim_norm)
qqline(sim_norm)
qqnormsim(fdims$wgt) 


