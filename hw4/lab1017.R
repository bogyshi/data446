# Continuous random variables
#############################



# Uniform(min,max) distribution
###############################

# PDF f(x) is given by 1/(max-min) if min <= x <= max
dunif(3, min = 2, max = 4)
dunif(5, min = 2, max = 4)

# CDF F(x) is given by (x-min)/(max-min) if min <= x <= max
punif(1, min = 2, max = 4)
punif(3, min = 2, max = 4)
punif(5, min = 2, max = 4)

# Generate samples from uniform distribution
generated_samples <- runif(n = 10000, min = 2, max = 4)

hist(generated_samples)



# Normal(mean, sd) distribution
###############################

# PDF and CDF have complicated expressions
dnorm(0, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1)

generated_samples <- rnorm(n = 10000, mean = 0, sd = 1)
hist(generated_samples, breaks = 100)



# Exponential(rate) distribution
################################

# PDF f(x) = rate * exp(-rate * x) for x > 0
dexp(2, rate = 2)

# CDF F(x) = 1 - exp(-rate * x) for x > 0
pexp(1/2, rate = 2)
pexp(100, rate = 2)

# Use set.seed function to obtain same results when generating random numbers
set.seed(0)
rexp(10, rate = 2)










# Working with the normal distribution
######################################
# (a) Only using random samples generated from the Normal(0,1) distribution,
# generate samples from the Normal(-5,5) distribution.



# (b) To verify these are indeed samples from the Normal(-5,5) distribution,
# estimate the probability that a random variable with this distribution will be
# less than -10 and compare it to its theoretical value.



# (c) Only using the CDF of a Normal(0,1) random variable, can you calculate the
# probability desired in part (b): P(X < -10) where X~Normal(-5,5)







# 68-95-99.7% rule
##################
# Verify the 68-95-99.7% rule for the normal distribution (Theorem 5.4.5) by
# showing that the probability a normal random variable falling within one s.d.
# of its mean is roughly 68%, two s.d.s of its mean is approximately 95%, etc.
# Don't proceed by simulation, and note it suffices to show the rule holds for
# the standard normal N(0,1).







# (BH Exercise 5.15+)
#####################
# Let U ~ Unif(-2,2). Using samples drawn from U, obtain samples from
# X ~ Exponential(lambda) (choose your own value for lambda > 0).



# Calculate P(X > 2/lambda | X > 1/lambda) theoretically and verify by simulation.


