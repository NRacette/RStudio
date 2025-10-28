beta0 <- 0
beta1 <- 0.1

x <- seq(-10, 10, 0.1)
pi_x <- exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))

plot(x, pi_x, type = "l", lwd = 2, col = "blue",
     main = expression(paste("Logistic Regression Model: ", beta[1], " = 0.1")),
     ylab = expression(pi(x)),
     xlab = "x")

beta0 <- 0
beta1 <- 0.9

x <- seq(-10, 10, 0.1)
pi_x <- exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))

plot(x, pi_x, type = "l", lwd = 2, col = "red",
     main = expression(paste("Logistic Regression Model: ", beta[1], " = 0.9")),
     ylab = expression(pi(x)),
     xlab = "x")


age <- c(12, 15, 42, 52, 59, 73, 82, 91, 96, 105, 114, 120, 121, 128, 130,
         139, 139, 157, 1, 1, 2, 8, 11, 18, 22, 31, 37, 61, 72, 81, 97, 112,
         118, 127, 131, 140, 151, 159, 177, 206)

kyphosis <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

kyphosisdf <- data.frame(age, kyphosis)

mod.kyphosis <- glm(kyphosis ~ age, data = kyphosisdf, family = binomial)

summary(mod.kyphosis)

b0 <- -0.572693
b1 <- 0.004296
age_val <- 150

p_hat <- exp(b0 + b1 * age_val) / (1 + exp(b0 + b1 * age_val))
p_hat

b0 <- -0.572693
b1 <- 0.004296

curve(exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)),
      from = 0, to = 220,
      xlab = "Age (months)",
      ylab = "Estimated Probability of Kyphosis",
      main = "Logistic Regression Model for Kyphosis vs. Age")

points(kyphosisdf$age, kyphosisdf$kyphosis, pch = 19, col = "blue")

V <- vcov(mod.kyphosis)
var_b0 <- V["(Intercept)","(Intercept)"]
var_b1 <- V["age","age"]
var_b0
var_b1

se_b0 <- sqrt(var_b0)
se_b1 <- sqrt(var_b1)
se_b0
se_b1


# 1 = hired, 0 = not hired
y <- c(0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
       1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1)

# Education, years
x1 <- c(6, 4, 6, 6, 4, 8, 4, 4, 6, 8, 4, 8, 4, 6,
        4, 6, 8, 6, 4, 4, 4, 6, 8, 4, 8, 6, 4, 6)

# Experience, years
x2 <- c(2, 0, 6, 3, 1, 3, 2, 4, 1, 10, 2, 5, 2, 7,
        5, 4, 0, 1, 7, 1, 5, 0, 5, 9, 1, 1, 10, 12)

# 1 = male, 0 = female
x3 <- c(0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
        1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0)

discriminationdf <- data.frame(y, x1, x2, x3)
discriminationdf

mod.disc1 <- glm(y ~ x1 + x2, data = discriminationdf, family = binomial)
summary(mod.disc1)

b0 <- -5.4419
b1 <- 0.5404
b2 <- 0.3717

x2_val <- 0
edu_levels <- c(4, 6, 8)

prob <- exp(b0 + b1 * edu_levels + b2 * x2_val) / 
  (1 + exp(b0 + b1 * edu_levels + b2 * x2_val))

data.frame(Education = edu_levels, Probability = round(prob, 4))

mod.disc2 <- glm(y ~ x2, data = discriminationdf, family = binomial)
summary(mod.disc2)

b0 <- -2.1325
b1 <- 0.3219
x2_val <- 10
p_hat <- exp(b0 + b1*x2_val) / (1 + exp(b0 + b1*x2_val))
round(p_hat, 4)

mod.disc3 <- glm(y ~ x1 + x2 + x3, data = discriminationdf, family = binomial)
summary(mod.disc3)

b0 <- -14.2483
b1 <- 1.1549
b2 <- 0.9098
b3 <- 5.6037

x1 <- 4; x2 <- 0; x3 <- 1
p_hat <- exp(b0 + b1*x1 + b2*x2 + b3*x3) / (1 + exp(b0 + b1*x1 + b2*x2 + b3*x3))
round(p_hat, 4)

vcov(mod.disc3)

LI <- c(8, 8, 10, 10, 12, 12, 12, 14, 14, 14, 16, 16, 16, 18, 20, 20, 20,
        22, 22, 24, 26, 28, 32, 34, 38, 38, 38)

# Cancer remission (1 = yes, 0 = no)
y <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0,
       1, 1, 1, 0)

cancerdf <- data.frame(LI, y)
cancerdf

mod.cancer <- glm(y ~ LI, data = cancerdf, family = binomial)
summary(mod.cancer)

b0 <- -3.77714
b1 <- 0.14486
LI_val <- 20
p_hat <- exp(b0 + b1*LI_val) / (1 + exp(b0 + b1*LI_val))
round(p_hat, 4)