w1 <- 189
n1  <- 10845
w2 <- 104
n2  <- 10933

c.table <- matrix(c(w1, n1,w2, n2), nrow = 2, byrow = TRUE)
rownames(c.table) <- c("Placebo", "Aspirin")
colnames(c.table) <- c("Yes", "No")
c.table

prop_placebo <- w1 / (w1 + n1)
prop_aspirin <- w2 / (w2 + n2)
prop_placebo
prop_aspirin

chisq.test(c.table, correct = FALSE)
expected <- chisq.test(c.table, correct = FALSE)$expected

pi_hat <- (w1 + w2) / (n1 + n2)
pi_hat

crit_value <- qchisq(0.95, df = 1)
crit_value

likelihood_ratio <- 2 * sum(c.table * log(c.table / expected))

likelihood_ratio
p_value <- 1 - pchisq(likelihood_ratio, df = 1)
p_value

RR <- prop_placebo / prop_aspirin
RR

n_placebo <- 11034
n_aspirin <- 11037

SE_log_RR <- sqrt((1 - prop_placebo) / (n_placebo * prop_placebo) + (1 - prop_aspirin) / (n_aspirin * prop_aspirin))
lower_RR <- exp(log(RR) - 1.96 * SE_log_RR)
upper_RR <- exp(log(RR) + 1.96 * SE_log_RR)
c(lower_RR, upper_RR)

placebo_yes <- 189
placebo_no  <- 10845
aspirin_yes <- 104
aspirin_no  <- 10933

OR <- (placebo_yes * aspirin_no) / (placebo_no * aspirin_yes)
OR




#Question 2

c.table <- matrix(
  c(246, 1545-246,   # Ginkgo: Dementia Yes, No
    277, 1524-277),  # Placebo: Dementia Yes, No
  nrow = 2, byrow = TRUE,
  dimnames = list(group = c("Ginkgo", "Placebo"),
                  Dementia = c("Yes", "No"))
)

# Totals and cases
n1 <- rowSums(c.table)[1]; y1 <- c.table["Ginkgo","Yes"]
n2 <- rowSums(c.table)[2]; y2 <- c.table["Placebo","Yes"]

# Proportions
p1.hat <- y1 / n1   # ginkgo risk
p2.hat <- y2 / n2   # placebo risk

# (a) Relative Risk (RR = p1/p2 with index 1 = ginkgo)
RR.hat <- p1.hat / p2.hat
RR.hat

# (b) "times (or % as large)" for ginkgo vs placebo
times_as_large <- RR.hat
percent_as_large <- 100 * RR.hat
c(times_as_large, percent_as_large)

# (c) 95% CI for RR using log-Wald
logRR <- log(RR.hat)
se.logRR <- sqrt((1 - p1.hat)/y1 + (1 - p2.hat)/y2)
z <- qnorm(0.975)
CI.RR <- exp(c(logRR - z*se.logRR, logRR + z*se.logRR))
CI.RR

inv.RR <- 1 / RR.hat
inv.RR

logRR <- log(RR.hat)
se.logRR <- sqrt((1 - p1.hat)/y1 + (1 - p2.hat)/y2)
z <- qnorm(0.975)
CI.RR <- exp(c(logRR - z*se.logRR, logRR + z*se.logRR))
CI.RR

CI.invRR <- 1 / rev(CI.RR)
CI.invRR

OR.hat <- (a * d) / (b * c)
OR.hat
# 0.8525358

# Odds of NOT developing dementia with ginkgo vs placebo ( = 1/OR )
inv.OR <- 1 / OR.hat
inv.OR
# 1.172971

# % reduction in odds of dementia with ginkgo (vs placebo)
pct_reduction_odds <- (1 - OR.hat) * 100
pct_reduction_odds
# 14.74642

# 95% CI for OR using log-OR Wald interval
se.logOR <- sqrt(1/a + 1/b + 1/c + 1/d)
z <- qnorm(0.975)
CI.OR <- exp(log(OR.hat) + c(-1,1) * z * se.logOR)
CI.OR
# 0.7060906 1.0293542