rm(list = ls())


# missing value data set

psi = factor(c(8500,8700,8900,9100))
block = factor(1:6)

dat_4_1 = expand.grid(block=block, psi=psi); dat_4_1

dat_4_1$value = c(90.3, 89.2, 98.2, 93.9, 87.4, 97.9,
                  92.5, 89.5, 90.6, 94.7, 87.0, 95.8,
                  85.5, 90.8, 89.6, 86.2, 88.0, 93.4, 
                  82.5, 89.5, 85.6, 87.4, 78.9, 90.7)

dat_4_1$value[10] <- NA


# estimation missing value

mis_lev <- dat_4_1[is.na(dat_4_1$value),]

a <- length(levels(dat_4_1$psi))
b <- length(levels(dat_4_1$block))

yi._prime <- sum(dat_4_1[dat_4_1$psi == mis_lev[,"psi"],"value"], na.rm = T)
y.j_prime <- sum(dat_4_1[dat_4_1$block == mis_lev[,"block"],"value"], na.rm = T)
y.._prime <- sum(dat_4_1$value, na.rm = T)

x_imp <- (a*yi._prime + b*y.j_prime - y.._prime)/((a-1)*(b-1))

dat_4_1$value[is.na(dat_4_1$value)] <- x_imp

# fit anova

fit <- aov(value ~ psi + block, data = dat_4_1)
aov.fit <- anova(fit)

f0 <- (aov.fit$`Sum Sq`[1]/aov.fit$Df[1])/(aov.fit$`Sum Sq`[3]/(aov.fit$Df[3]-1))

1 - pf(f0,3, 14)
pf(f0,3, 14, lower.tail = F)



############
## BIBD
# table 4.22
trt <- factor(1:4)
block <- factor(1:4)

dat_4_22 <- expand.grid(block = block, trt = trt)
dat_4_22$value <- c(73,74,NA,71,
                    NA,75,67,72,
                    73,75,68,NA,
                    75,NA,72,75)

# method1 using aov drop1
basic_fit <- aov(value ~ trt + block, data = dat_4_22)
anova(basic_fit)
basic_fit2 <- aov(value ~ block + trt, data = dat_4_22)
anova(basic_fit2)

bibd_fit <- drop1(basic_fit, test = "F")

# method2 using packages
library(ibd)
library(multcompView)

bibd_fit2 <- aov.ibd(value ~ trt + block, data = dat_4_22, specs = "trt")



# ch 5 Factorial experiments
# 5.1 basic definitions and principles

A <- factor(0:1)
B <- factor(0:1)

dat <- expand.grid(A=A, B=B); dat;
dat$value <- c(20,40,30,52)

# 1) Main effect
eff_A <- mean(dat$value[dat$A == 1]) - mean(dat$value[dat$A == 0])
eff_B <- mean(dat$value[dat$B == 1]) - mean(dat$value[dat$B == 0])

# 2) Interaction effect
eff_AB <- mean(dat$value[dat$A == dat$B]) - mean(dat$value[dat$A != dat$B])

c(eff_A,eff_B,eff_AB)

# Interaction plot

interaction.plot(dat$A, dat$B, response = dat$value,
                 ylim = c(10,60), xaxt = "n",
                 xlab = "Factor A", ylab = "response",
                 main = "Interaction plot in figure 5.3",
                 legend = F, col = c("blue","red"))

axis(1, at = c(1,2), labels = c("-","+"), lwd.ticks = 3)
legend("topleft", c("B-","B+"), lty = 2:1, col = c("blue","red"))
clist <- c("blue","blue","red","red")

for(i in 1:4){
  points(dat$A[i],dat$value[i], cex = 1.3, pch = 1, col = clist[i])
}

identify(dat$A[dat$B == 0], dat$value[dat$B==0], labels = "B-")
identify(dat$A[dat$B == 1], dat$value[dat$B==1], labels = "B+")




# 5.3 Two-factor factorial design
mtype <- factor(1:3)
temp <- factor(c(15, 70, 125))
times <- 1:4

dat_5_3 <- expand.grid(times = times, temp = temp, mtype = mtype)

dat_5_3$value <- c(130, 155, 74, 180, 34, 40, 80, 75, 20, 70, 82, 58,
                   150, 188, 159, 126,136, 122, 106, 115, 25, 70, 58, 45,
                   138, 110, 168, 160,174, 120, 150, 139, 96, 104, 82, 60)

# method 1
fit_fac <- aov(value ~ mtype + temp + mtype:temp, data = dat_5_3)
anova(fit_fac)

# method 2
fit_fac2 <- aov(value ~ mtype*temp, data = dat_5_3)
anova(fit_fac2)

# method 3
fit_fac3 <- aov(value ~ mtype + temp + mtype*temp, data = dat_5_3)
anova(fit_fac3)


# interaction plot
interaction.plot(dat_5_3$temp, dat_5_3$mtype, response = dat_5_3$value,
                 ylim = c(0,175), xaxt = "n",
                 xlab = "Temperature",
                 ylab = "Average life",
                 main = "Interaction between Material type and temperature",
                 legend = F, col = c("blue","red","green"))
axis(1, at=c(1,2,3), labels = c("15","70","125"), lwd.ticks = 3)
legend("topright", paste0("Type", 1:3), lty = 3:1, col = c("blue","red","green"))

text(locator(1), "Type 1", col = "blue")
text(locator(1), "Type 2", col = "red")
text(locator(2), "Type 3", col = "green")

# The Assumption of No interaction
fit_noint <- aov(value ~ mtype + temp, data = dat_5_3)
anova(fit_noint)


# Question
A = c("15", 20, 25, 30, 35)
times <- 1:5

dat <- expand.grid(A = A, times = times)
dat$value <- c(7, 12, 14, 19, 7,
               7, 17, 19, 25, 10,
               15, 12, 19, 22, 11,
               11, 18, 18, 19, 15,
               9, 18, 18, 23, 11)
fit <- aov(value ~ A, data = dat)

library(agricolae)

sch_res <- scheffe.test(fit, trt = "A")
LSD_res <- LSD.test(fit, trt = "A")
