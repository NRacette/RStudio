#Question 1
c.table <- array(data = c(10, 4, 24, 12), dim =c(2,2),
dimnames = list(Subject_exposed_to_yawning = c("yes","no"), 
did_subject_yawn = c("yes","no")))
chi_result <- chisq.test(x = c.table, correct = FALSE)
p_value <- chi_result$p.value / 2
p_value


#Question 2 
c.table <- array(data = c(28, 6, 1, 4), dim =c(2,2),
dimnames = list(Treatment = c("ECMO","CMT"), 
Survival = c("yes","no")))
fisher.test(x=c.table)

#Question 3
c.table <- array(data = c(21, 15, 2, 3), dim =c(2,2),
dimnames = list(Treatment = c("surgery","radiation therapy"), 
controlled = c("cancer controlled","cancer not controlled")))
fisher.test(x = c.table, alternative = "greater")
fisher.test(x = c.table)

#Question 4
c.table <- array(data = c(12, 9, 10, 14), dim =c(2,2),
dimnames = list(drug_1_responce = c("Remains Anesthetized","Not Anesthetized"), 
drug_2_responce = c("Remains Anesthetized","Not Anesthetized")))
n <- sum(c.table)
pi.hat.plus1 <- sum(c.table[,1])/n
pi.hat.1plus <- sum(c.table[1,])/n
pi.hat.plus1
pi.hat.1plus
diff <- pi.hat.plus1 - pi.hat.1plus
diff
library(package = PropCIs)
diffpropci.wald.mp(b = c.table[1,2], c = c.table[2,1],
 n =sum(c.table), comf.level = 0.95)
# used diffpropci.mp diffpropci.wald.mp was not working with the PropCIs package
diffpropci.mp(b = c.table[1, 2], 
              c = c.table[2, 1], 
              n = sum(c.table), 
              conf.level = 0.95)

mcnemar.test(c.table, correct = FALSE)

#Question 5
c.table <- matrix(c(914, 581,
                    46, 735),
                  nrow = 2, byrow = TRUE)
dimnames(c.table) <- list(
  "Alcohol" = c("Yes", "No"),
  "Marijuana" = c("Yes", "No")
)
n <- sum(c.table)
p.plus1 <- sum(c.table[1, ]) / n
p.1plus <- sum(c.table[, 1]) / n
diff <- p.plus1 - p.1plus
diff

se <- sqrt((c.table[1, 2] + c.table[2, 1] - (c.table[1, 2] - c.table[2, 1])^2 / n) / n^2)
wald.ci <- diff + qnorm(c(0.025, 0.975)) * se
wald.ci

mcnemar.test(c.table, correct = FALSE)

z <- qnorm(0.975)
p1.tilde <- (sum(c.table[1, ]) + z^2 / 2) / (n + z^2)
p2.tilde <- (sum(c.table[, 1]) + z^2 / 2) / (n + z^2)
diff.tilde <- p1.tilde - p2.tilde
diff.tilde

se.agr <- sqrt(p1.tilde * (1 - p1.tilde) / n + p2.tilde * (1 - p2.tilde) / n)
agresti.ci <- diff.tilde + c(-1, 1) * z * se.agr
agresti.ci

mcnemar.test(c.table, correct = FALSE)

#Question 6
c.table <- array(data = c(309, 15, 10, 2), dim = c(2, 2),
                 dimnames = list(
                   without_protector = c("No_Fracture", "Fracture"),
                   with_protector    = c("No_Fracture", "Fracture")
                 ))
n <- sum(c.table)

p.plus1    <- sum(c.table[, 1]) / n
p.plus1
p.1plus <- sum(c.table[1, ]) / n
p.1plus
diff <- p.plus1 - p.1plus
diff

library(PropCIs)
diffpropci.mp(b = c.table[2, 1],
            c = c.table[1, 2],
            n = n,
            conf.level = 0.95)

n_tilde <- n + 2 
p1_tilde <- (c.table[1, 2] + 1) / n_tilde
p2_tilde <- (c.table[2, 1] + 1) / n_tilde
diff_tilde <- p2_tilde - p1_tilde
se_tilde <- sqrt(((p1_tilde + p2_tilde) - (p1_tilde - p2_tilde)^2) / n_tilde)
lower_bound <- diff_tilde - z * se_tilde
upper_bound <- diff_tilde + z * se_tilde
