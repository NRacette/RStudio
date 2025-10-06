###########################################################################
# NAME: Nick Racette                                                      #
# DATE: 9-20-25                                                           #
# PURPOSE: Assignment 4 Analysis of categorical data                      #
# NOTES:                                                                  #
###########################################################################

# Question 1
n <- 220
w <- 91
p0 <- 0.4
p.hat <- w/n
Z <- (p.hat - p0) / sqrt(p0*(1-p0)/n)
Z
P.value <- 1-pnorm(Z)


# Question 2
n <- 48
w <- 29
p0 <- 0.5
p.hat <- w/n
Z <- (p.hat - p0) / sqrt(p0*(1-p0)/n)
Z
2*(1-pnorm(Z))

# Question 3
n <- 227
w <- 52
p0 <- 0.20
p.hat <- w/n
Z <- (p.hat - p0) / sqrt(p0*(1-p0)/n)
Z
P.value <- 1-pnorm(Z)
P.value
qnorm(0.95)


# Question 4
n <- 4581
w <- 4581*0.47
p0 <- 0.5
p.hat <- w/n
Z <- (p.hat - p0) / sqrt(p0*(1-p0)/n)
Z
P.value <- 1-pnorm(Z)
P.value
qnorm(0.95)

# Question 5
n<-98
w<-74
p0<-0.8
p.hat<-w/n
Z<-(p.hat-p0)/sqrt(p0*(1-p0)/n)
Z
qnorm(0.95)

# Question 6
p_hat <- 0.57
p0 <- 0.50
n <- 1060
Z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
Z
qnorm(0.995)

# Question 7
# a)
n<-1000
w<-119
p.hat<-w/n
p0<-0.10
Z<-(p.hat-p0)/sqrt(p0*(1-p0)/n)
Z
qnorm(0.975)
# b)
2*(1-pnorm(Z))

wald_ci_lower <- p.hat - qnorm(0.975) * sqrt(p.hat * (1 - p.hat) / n)
wald_ci_upper <- p.hat + qnorm(0.975) * sqrt(p.hat * (1 - p.hat) / n)
cat(sprintf("95%% Wald CI: [%.3f, %.3f]\n", wald_ci_lower, wald_ci_upper))


# Question 8

sum.y<-6
n<-15
pi<-0.5
Lik_correct <- dbinom(x = sum.y, size = n, prob = pi)
Lik_correct