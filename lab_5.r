# Code for OpenIntro Statistics textbook lab 4b, accessed at the following URL on 6/21/2014:
# http://www.openintro.org/download.php?file=os2_lab_05A&referrer=/stat/labs.php

download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")

names(nc)
summary(nc)
nrow(nc)
nc$complete = complete.cases(nc)
nrow(subset(nc, nc$complete==F))

"
Exercise 2 Make a side-by-side boxplot of habit and weight. What does the plot highlight
about the relationship between these two variables?
"
boxplot(nc$habit, nc$weight, horizontal=T)
with(nc, plot(weight ~ habit))
# babies of smokers weigh less, on average. lots of outliers in the "nonsmoker" category
smoker = subset(nc, habit=="smoker")
nonsmoker = subset(nc, habit=="nonsmoker")
t.test(smoker$weight, nonsmoker$weight)

# another way to compare, the by function
by(nc$weight, nc$habit, mean)

"
Exercise 3 Check if the conditions necessary for inference are satisﬁed. Note that you will
need to obtain sample sizes to check the conditions. You can compute the group size using the
same by command above but replacing mean with length.
"
by(nc$weight, nc$habit, length)
hist(smoker$weight)
hist(nonsmoker$weight)
# left skew in both but fairly normal. also check via quantile plots:
qqnorm(smoker$weight)
qqline(smoker$weight)
qqnorm(nonsmoker$weight)
qqline(nonsmoker$weight)

"
Exercise 4 Write the hypotheses for testing if the average weights of babies born to smoking
and non-smoking mothers are different.
"
# null: There is no difference in the average weights of babies born to smoking mothers and babies born to non-smoking mothers.
# hypothesis: On average, babies born to smoking mothers weigh less than babies born to non-smoking mothers.

# run "inference" function that came with the data set
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

