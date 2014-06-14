# Code for OpenIntro Statistics textbook lab 2, accessed at the following URL on 6/14/2014:
# http://www.openintro.org/download.php?file=os2_lab_02A&referrer=/stat/labs.php

download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.RData")
head(kobe)

# function calc_streak was loaded w/ the data
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))

# simulate a player with independent P's for each shot (i.e., definitely doesn't have hot hands)
outcomes <- c("heads", "tails")
sample(outcomes, size = 1, replace = TRUE)

sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
table(sim_fair_coin)
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))
table(sim_unfair_coin)

outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 1, replace = TRUE)
# reflect a shooting %age of 45%
sim_basket <- sample(outcomes, size = 1, replace = TRUE, prob = c(0.45,0.65))
# Make this adjustment, then run a simulation to sample 133 shots. Assign the output of this simulation to a new object called sim basket.
sim_basket = sample(outcomes, size = 133, replace = TRUE, prob = c(0.45,0.65))
table(sim_basket) / 133
# compare to Kobe:
table(kobe$basket) / 133

"
Describe the distribution of streak lengths. What is the typical streak length for this simulated
independent shooter with a 45% shooting percentage? How long is the player’s longest streak of
baskets in 133 shots?
"
sim_streak <- calc_streak(sim_basket)
barplot(table(sim_streak))
# 0, 4

"
If you were to run the simulation of the independent shooter a second time, how would you expect
its streak distribution to compare to the distribution from the question above? Exactly the same?
Somewhat similar? Totally different? Explain your reasoning.
"
# very similiar but not exact - different random sample from the same population
sim_basket = sample(outcomes, size = 133, replace = TRUE, prob = c(0.45,0.65))
table(sim_basket) / 133


"
How does Kobe Bryant’s distribution of streak lengths from page 2 compare to the distribution of
streak lengths for the simulated shooter? Using this comparison, do you have evidence that the hot
hand model ﬁts Kobe’s shooting patterns? Explain.
"
sim_streak <- calc_streak(sim_basket) 
plot_sim = barplot(table(sim_streak))
barplot(table(kobe_streak))
# They are very similiar, so we do not have evidence to support the hot hand model.

