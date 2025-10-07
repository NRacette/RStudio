# Question 1 
n1 <- 1107
n2 <- 919
w1 <- 352
w2 <- 290
p_hat1 <- w1 / n1
p_hat2 <- w2 / n2
pi_diff <- p_hat1 - p_hat2
print(pi_diff)

pi_combined <- (w1 + w2) / (n1 + n2)
standard_error <- sqrt(pi_combined * (1 - pi_combined) * (1/n1 + 1/n2))
z_score <- (p_hat1 - p_hat2) / standard_error
print(z_score)

p_value <- 2 * pnorm(abs(z_score), lower.tail = FALSE)
print(p_value)

alpha <- 0.01
z_critical <- qnorm(1 - alpha/2) 
print(z_critical)

alpha <- 0.01
se_wald <- sqrt( (p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2) )
var.wald <- se_wald^2

# Lower and Upper bounds
lower <- pi_diff - qnorm(p = 1 - alpha/2) * sqrt(var.wald)
upper <- pi_diff + qnorm(p = 1 - alpha/2) * sqrt(var.wald)
print(lower)
print(upper)

# Question 2
w1 <- 23 
n1 <- 240 
w2 <- 65 
n2 <- 520 

test <- prop.test(x = c(w1, w2), n = c(n1, n2), correct = FALSE)
print(test)

alpha <- 0.01
z_critical <- qnorm(1 - alpha/2)

p1_tilde <- (x1 + 1) / (n1 + 2)
p2_tilde <- (x2 + 1) / (n2 + 2)
pidif_tilde <- p1_tilde - p2_tilde

se_ac <- sqrt(p1_tilde * (1 - p1_tilde) / (n1 + 2) + p2_tilde * (1 - p2_tilde) / (n2 + 2))
margin_of_error_ac <- z_critical * se_ac
lower_ac <- pidif_tilde - margin_of_error_ac
upper_ac <- pidif_tilde + margin_of_error_ac
print(lower_ac)
print(upper_ac)


# Question 3
n1 <- 2823
w1 <- 31
n2 <- 7765
w2 <- 16
c.table <- matrix(c(w1, n1 - w1, w2, n2 - w2), nrow = 2, byrow = TRUE)

pi_bar <- sum(c.table[, 1]) / sum(c.table)

p_hat1 <- c.table[1, 1] / sum(c.table[1, ])
p_hat2 <- c.table[2, 1] / sum(c.table[2, ])

log.Lamda.terms <- c.table[1, 1] * log(p_hat1 / pi_bar) +
                   c.table[1, 2] * log((1 - p_hat1) / (1 - pi_bar)) +
                   c.table[2, 1] * log(p_hat2 / pi_bar) +
                   c.table[2, 2] * log((1 - p_hat2) / (1 - pi_bar))

test_stat <- 2 * log.Lamda.terms
crit.value <- qchisq(0.95, df = 1)
p.value <- pchisq(test_stat, df = 1, lower.tail = FALSE)
round(data.frame(pi_bar, test_stat, crit.value, p.value), 3)

alpha <- 0.05
pi_diff <- p_hat1 - p_hat2

se_wald <- sqrt( (p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2) )
margin_of_error <- qnorm(1 - alpha/2) * se_wald

lower <- pi_diff - margin_of_error
upper <- pi_diff + margin_of_error
print(lower)
print(upper)


# Question 4
n1 <- 73
w1 <- 67
n2 <- 83
w2 <- 60
alpha <- 0.01

p_hat1 <- w1 / n1
p_hat2 <- w2 / n2
pi_diff <- p_hat1 - p_hat2
pi_bar <- (w1 + w2) / (n1 + n2)

se_score <- sqrt(pi_bar * (1 - pi_bar) * (1/n1 + 1/n2))
z_score <- (p_hat1 - p_hat2) / se_score
p_value_score <- 2 * (1 - pnorm(abs(z_score)))

chi_sq <- z_score^2
p_value_chi <- 1 - pchisq(chi_sq, df = 1)

log_lambda_terms <- w1 * log(p_hat1 / pi_bar) +
  (n1 - w1) * log((1 - p_hat1) / (1 - pi_bar)) +
  w2 * log(p_hat2 / pi_bar) +
  (n2 - w2) * log((1 - p_hat2) / (1 - pi_bar))

test_stat <- 2 * log_lambda_terms
print(test_stat)
p_value <- pchisq(test_stat, df = 1, lower.tail = FALSE)
print(p_value)

z_crit <- qnorm(1 - alpha / 2)
se_wald <- sqrt((p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2))
moe <- z_crit * se_wald
lower <- pi_diff - moe
upper <- pi_diff + moe
print(lower)
print(upper)

# Question 5
n1 <- 20
w1 <- 10
n2 <- 2000 
w2 <- 1404
alpha <- 0.05

p_hat1 <- w1 / n1
p_hat2 <- w2 / n2
pi_diff <- p_hat1 - p_hat2

pi_bar <- (w1 + w2) / (n1 + n2)

se_score <- sqrt(pi_bar * (1 - pi_bar) * (1/n1 + 1/n2))
z_score <- (p_hat1 - p_hat2) / se_score
p_value <- 2 * (1 - pnorm(abs(z_score)))
print(z_score)
print(p_value)

se_wald <- sqrt((p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2))
margin_of_error <- qnorm(0.975) * se_wald
lower <- pi_diff - margin_of_error
upper <- pi_diff + margin_of_error
print(lower)
print(upper)

# Question 6
w1 <- 112
n1 <- 200
w2 <- 88
n2 <- 200

p_hat1 <- w1 / n1
p_hat2 <- w2 / n2
pi_diff <- p_hat1 - p_hat2

alpha <- 0.05
wald <- sqrt( (p_hat1*(1-p_hat1)/n1) + (p_hat2*(1-p_hat2)/n2) )
margin_of_error <- qnorm(1 - alpha/2) * wald
lower_bound <- pi_diff - margin_of_error
upper_bound <- pi_diff + margin_of_error
print(lower_bound)
print(upper_bound)

wald_1 <- sqrt(p_hat1 * (1 - p_hat1) / n1)
margin_of_error_1 <- qnorm(1 - alpha/2) * wald_1
lower_1 <- p_hat1  - margin_of_error_1
upper_1 <- p_hat1  + margin_of_error_1
print(lower_1)
print(upper_1)

wald_2 <- sqrt(p_hat2 * (1 - p_hat2) / n2)
margin_of_error_2 <- qnorm(1 - alpha/2) * wald_2
lower_2 <- p_hat2 - margin_of_error_2
upper_2 <- p_hat2 + margin_of_error_2
print(lower_2)
print(upper_2)

test <- prop.test(x = c(w1, w2), n = c(n1, n2), correct = FALSE)
print(test)