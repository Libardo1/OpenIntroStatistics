download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

population <- ames$Gr.Liv.Area
samp <- sample(population, 60)

hist(samp)
boxplot(samp, horizontal=T)
summary(samp)
sd(samp)
qqnorm(samp)
qqline(samp)

"
Exercise 1 Describe the distribution of your sample. What would you say is the “typical” size
within your sample? Also state precisely what you interpreted “typical” to mean.

~1,500, as this is the mean. Distribution is right skewed and not really normal.

Exercise 2 Would you expect another student’s distribution to be identical to yours? Would
you expect it to be similar? Why, or why not?

Not identical, but similiar because we are sampling randomally.
"

# We can calculate a 95% conﬁdence interval for a sample mean by adding and subtracting 1.96 standard errors to the point estimate

sample_mean = mean(samp)
se <- sd(samp)/sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se
c(lower, upper)

mean(population)

# use loops to create many samples and investigate variance
samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60

for(i in 1:50){
  samp <- sample(population, n) # obtain a sample of size n = 60 from the population
  samp_mean[i] <- mean(samp) # save sample mean in ith element of samp_mean
  samp_sd[i] <- sd(samp) # save sample sd in ith element of samp_sd
}

lower_vector <- samp_mean - 1.96 * samp_sd/sqrt(n)
upper_vector <- samp_mean + 1.96 * samp_sd/sqrt(n)

c(lower_vector[1], upper_vector[1])

"
Using the following function (which was downloaded with the data set), plot all intervals. What
proportion of your conﬁdence intervals include the true population mean? Is this proportion exactly
equal to the conﬁdence level? If not, explain why.

plot_ci(lower_vector, upper_vector, mean(population))
"
length(lower_vector)
3/50
# 94%
