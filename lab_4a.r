# Code for OpenIntro Statistics textbook lab 2, accessed at the following URL on 6/14/2014:
# http://www.openintro.org/download.php?file=os2_lab_04A&referrer=/stat/labs.php

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

head(ames)
names(ames)

"
For this lab, we’ll restrict our attention to just two of the variables: the above ground living area of the house in
square feet (Gr.Liv.Area) and the sale price (SalePrice). 
"
area <- ames$Gr.Liv.Area
price <- ames$SalePrice

summary(area)
hist(area)
# looks fairly normal around mean of 1500, but w/ long tail (left skew)
# test normality:
qqnorm(area)
qqline(area)

# take random sample of 50
samp1 <- sample(area, 50)
hist(samp1)
summary(samp1)
samp2 <- sample(area, 50)
hist(samp2)
summary(samp2)

# simulate a sampling distribution. Take 5000 samples, get mean of each
sample_means50 <- rep(0, 5000)
for (i in 1:5000) {
  samp <- sample(area, 50)
  sample_means50[i] <- mean(samp)
}
hist(sample_means50)
hist(sample_means50, breaks=25)

sample_means10 <- rep(0, 5000)
sample_means100 <- rep(0, 5000)
for (i in 1:5000) {
  samp <- sample(area, 10)
  sample_means10[i] <- mean(samp)
  samp <- sample(area, 100)
  sample_means100[i] <- mean(samp)
}

hist(sample_means10)
hist(sample_means100)

# see the effect of sample size on the sampling distribution
par(mfrow = c(3, 1))
xlimits = range(sample_means10)
hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

"
Take a random sample of size 50 from price. Using this sample, what is your best point estimate of
the population mean?
"
s_price <- sample(price, 50)
mean(s_price)
mean(ames$SalePrice)

"
Since you have access to the population, simulate the sampling distribution for x¯price by taking 5000
samples from the population of size 50 and computing 5000 sample means. Store these means in a
vector called sample means50. Plot the data, then describe the shape of this sampling distribution.
Based on this sampling distribution, what would you guess the mean home price of the population
to be? Finally, calculate and report the population mean.
"
sample_means50 <- rep(0, 5000)
for (i in 1:5000) {
  samp <- sample(price, 50)
  sample_means50[i] <- mean(samp)
}
hist(sample_means50)
mean(sample_means50)
mean(ames$SalePrice)

"
Change your sample size from 50 to 150, then compute the sampling distribution using the same
method as above, and store these means in a new vector called sample means150. Describe the shape
of this sampling distribution, and compare it to the sampling distribution for a sample size of 50.
Based on this sampling distribution, what would you guess to be the mean sale price of homes in
Ames?
"
sample_means150 = rep(0, 5000)
for (i in 1:5000) {
  samp <- sample(price, 150)
  sample_means150[i] <- mean(samp)
}
hist(sample_means150)
mean(sample_means150)
mean(sample_means50)
mean(ames$SalePrice)

abs(mean(sample_means50) - mean(ames$SalePrice))
abs(mean(sample_means150) - mean(ames$SalePrice))
# larger sample = more reflective of population

"
Of the sampling distributions from 2 and 3, which has a smaller spread? If we’re concerned with
making estimates that are more often close to the true value, would we prefer a distribution with a
large or small spread?
"
max(sample_means50) - min(sample_means50)
max(sample_means150) - min(sample_means150)
# the larger sample has a smaller spread. This is preferred - tighter around the mean
