###########################################################################
# NAME: Nick Racette                                                      #
# DATE: 9-20-25                                                           #
# PURPOSE: Assignment 5 Analysis of categorical data                      #
# NOTES:                                                                  #
###########################################################################

# Question 1
c.table<-array(data = c(1757, 887, 4279, 2440), dim = c(2,2), 
               dimnames = list(First = c("Men", "Women"),
                               Second = c("Overturned", "Not Overturned")))
list(First = c("Men", "Women"), Second = c("Overturned", "Not Overturned"))  # See dimnames
c.table  # Whole table


# Question 2
c.table
c.table[1,1]
c.table[1,]
sum(c.table[1,])
rowSums(c.table)  
pi.hat.table<-c.table/rowSums(c.table)
pi.hat.table


# Question 3

w1<-1757
n1<-6036
w2<-887
n2<-3327
alpha<-0.05
pi.hat1<-w1/n1
pi.hat2<-w2/n2
var.wald<-pi.hat1*(1-pi.hat1) / n1 +  pi.hat2*(1-pi.hat2) / n2
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) *  sqrt(var.wald)

# Question 4
library(package = PropCIs)
# Agresti-Caffo
wald2ci(x1 = c.table[1,1], n1 = sum(c.table[1,]), x2 = c.table[2,1], 
        n2 = sum(c.table[2,]),
        conf.level = 0.95, adjust = "AC")


# Question 5
alpha<-0.05
pi1<-0.7
pi2<-0.3
n1<-25
n2<-25
numb.bin.samples<-2000

set.seed(82621)
w1<-rbinom(n = numb.bin.samples, size = n1, prob = pi1)
w2<-rbinom(n = numb.bin.samples, size = n2, prob = pi2)

pi.hat1<-w1/n1
pi.hat2<-w2/n2

# Wald
var.wald<-pi.hat1*(1-pi.hat1) / n1 + pi.hat2*(1-pi.hat2) / n2
lower<-pi.hat1 - pi.hat2 - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper<-pi.hat1 - pi.hat2 + qnorm(p = 1-alpha/2) * sqrt(var.wald)

# Intervals 1-5
data.frame(w1, w2, lower, upper)[1:5,]

# Calculate estimated true confidence level
save<-ifelse(test = pi1-pi2 > lower,
             yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
save[1:5]
true.conf<-mean(save)
round(true.conf,4)

# Question 6
pi.tilde1<-(w1+1)/(n1+2)
pi.tilde2<-(w2+1)/(n2+2)
var.AC<-pi.tilde1*(1-pi.tilde1) / (n1+2) + pi.tilde2*(1-pi.tilde2) / (n2+2)
lower.AC<-pi.tilde1 - pi.tilde2 - qnorm(p = 1-alpha/2) * sqrt(var.AC)
upper.AC<-pi.tilde1 - pi.tilde2 + qnorm(p = 1-alpha/2) * sqrt(var.AC)
save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), 
                no = 0)
save.AC[1:10]
true.conf.AC<-mean(save.AC)
round(true.conf.AC,3)

# Question 7
w.all<-expand.grid(w1 = 0:n1, w2 = 0:n2)

# All possible combinations of pi^_1 and pi^_2
pi.hat1<-(0:n1)/n1
pi.hat2<-(0:n2)/n2
pi.hat.all<-expand.grid(pi.hat1 = pi.hat1, pi.hat2 = pi.hat2)

# Find joint probability for w1 and w2
prob.w1<-dbinom(x = 0:n1, size = n1, prob = pi1)
prob.w2<-dbinom(x = 0:n2, size = n2, prob = pi2)
prob.all<-expand.grid(prob.w1 = prob.w1, prob.w2 = prob.w2)
pmf<-prob.all$prob.w1*prob.all$prob.w2

# Joint probability of observing w1 and w2 (i.e., P(W1 = w1, W2 = w2))
head(data.frame(w.all, pmf = round(pmf,4)))
tail(data.frame(w.all, pmf = round(pmf,4)))

# Wald
var.wald<-pi.hat.all[,1]*(1-pi.hat.all[,1]) / 
  n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2
lower<-pi.hat.all[,1] - pi.hat.all[,2] - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper<-pi.hat.all[,1] - pi.hat.all[,2] + qnorm(p = 1-alpha/2) * sqrt(var.wald)
save<-ifelse(test = pi1-pi2 > lower,
             yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
sum(save*pmf)

# Question 8
pi1tilde<-(0:n1+1)/(n1+2)
pi2tilde<-(0:n2+1)/(n2+2)
pi.all.tilde<-expand.grid(pi1tilde = pi1tilde, pi2tilde = pi2tilde)
var.ac<-pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
  pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2)
lower.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] - 
  qnorm(p = 1-alpha/2) * sqrt(var.ac)
upper.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] + 
  qnorm(p = 1-alpha/2) * sqrt(var.ac)
save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), 
                no = 0)
sum(save.AC*pmf)

# Question 9
w1<-123
n1<-175
w2<-131
n2<-145
alpha<-0.05
pi.hat1<-w1/n1
pi.hat2<-w2/n2
var.wald<-pi.hat1*(1-pi.hat1) / n1 +  pi.hat2*(1-pi.hat2) / n2
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) *  sqrt(var.wald)

# Question 10
w1 <- 7813
n1 <- 61909
w2 <- 17775
n2 <- 161566
alpha <- 0.02
pi_tilde1 <- (w1 + 1) / (n1 + 2)
pi_tilde2 <- (w2 + 1) / (n2 + 2)

var_AC <- pi_tilde1 * (1 - pi_tilde1) / (n1 + 2) + pi_tilde2 * (1 - pi_tilde2) / (n2 + 2)
se_AC <- sqrt(var_AC)
pi_tilde1 - pi_tilde2 + qnorm(p = c(alpha/2, 1-alpha/2)) * se_AC