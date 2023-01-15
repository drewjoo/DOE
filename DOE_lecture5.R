rm(list=ls())

# example 4.1 (create data set(example 4.1))
psi <- factor(c(8500, 8700, 8900, 9100))
block = factor(1:6)

a <- length(psi)
b <- length(block)

dat_4_1 = expand.grid(block = block, psi = psi)

dat_4_1$value <- c(90.3, 89.2, 98.2, 93.9, 87.4, 97.9,
                   92.5, 89.5, 90.6, 94.7, 87, 95.8,
                   85.5, 90.8, 89.6, 86.2, 88, 93.4,
                   82.5, 89.5, 85.6, 87.4, 78.9, 90.7)

# compute sum of row and column using tapply
block_sum <- tapply(dat_4_1$value, dat_4_1$block, sum)
trt_sum <- tapply(dat_4_1$value, dat_4_1$psi, sum)
total_y <- sum(dat_4_1$value)


# compute Sum of square
ss_trt <- sum(trt_sum^2)/b - total_y^2/a/b
ss_block <- sum(block_sum^2)/a - total_y^2/a/b
ss_total <- sum(dat_4_1$value^2) - total_y^2 /a/ b
ss_error <- ss_total - ss_trt - ss_block

# RCBD design
fit_rcbd <- aov(value ~ psi + block, data = dat_4_1)
anova(fit_rcbd)



# RCBD design vs CRD design
fit_CRD <- aov(value ~ psi, data = dat_4_1)
anova(fit_CRD)



# model checking
x11()
par(mfrow = c(2,2))

plot(fit_rcbd)
shapiro.test(fit_rcbd$residuals)

# Some other plots
Resid = fit_rcbd$residuals ## resid(fit_rcbd)

par(mfrow=c(1,2))

plot(Resid ~ as.character(dat_4_1$psi), 
     xlab = "Treatments", ylab= "Residuals", 
     main = "Residuals by Treatments", pch = 4)
abline(h=0, col="red")

plot(Resid ~ as.character(dat_4_1$block), 
     xlab = "Blocks", ylab = "Residuals", 
     main = "Residuals by Blocks", pch = 2)
abline(h=0, col="blue")


# random eff
## compute sigma beta square estimation
anova_fit <- anova(fit_rcbd)
sigma_beta_2 <- (anova_fit$`Mean Sq`[2] - anova_fit$`Mean Sq`[3])/a


# latin square design
batch <- factor(1:5)
batch2 <- factor(1:5)

## Method 1 (Direct input)
trt <- c("A","B","C","D","E",
         "B","C","D","E","A",
         "C","D","E","A","B",
         "D","E","A","B","C",
         "E","A","B","C","D")

dat <- expand.grid(block1 = batch, block2 = batch2)
dat$trt <- trt

dat$value <- c(24, 17, 18, 26, 22,
               20, 24, 38, 31, 30,
               19, 30, 26, 26, 20,
               24, 27, 27, 23, 29,
               24, 36, 21, 22, 31)
## Method 2 (input using for function[위로 올리는 방식])

### Method 2-1(dual for)
rp_fac = c("A","B","C","D","E")
rp = c(rp_fac,rp_fac[1:4])

design2 <- c()

for(i in 1:5){
  for(j in 1:5)
  {
    design2[5*(i-1)+j] = rp[i+j-1]
  }
}
design2 = as.factor(design2)

### Method 2-2(single for)

design3 = c()
for(i in 1:5)
{
  design3[5*(i-1)+1:5] = rp[1:5+i-1]
}
design3 = as.factor(design3)



# fitting latin square design
fit_latin <- aov(value ~ trt + block1 + block2, data = dat)
anova(fit_latin)




# exercise


# 1-1 create data set
distance <- factor(c(4,6,8,10))
person <- factor(1:5)

dat <- expand.grid(distance = distance, person = person)
dat$value <- c(10,7,5,6,
               6,6,3,4,
               6,6,3,4,
               6,1,2,2,
               6,6,5,3)

## fitting anova
fit_rcbd_2 <- aov(value ~ distance + person, data = dat)
anova(fit_rcbd_2) # reject h0


# 1-2 model checking
x11();par(mfrow = c(2,2))
plot(fit_rcbd_2)

resid <- fit_rcbd_2$residuals

# some other plot
x11(); par(mfrow = c(1,2))
plot(resid ~ as.character(dat$distance))
abline(h = 0, col = "red", lwd = 3)

plot(resid ~ as.character(dat$person))
abline(h = 0, col = "red", lwd = 3)


# 1-3 compute sigma beta square estimation



## using aov function
aov_rcbd_2 <- anova(fit_rcbd_2)
sigma_beta_2 <- (aov_rcbd_2$`Mean Sq`[2] - aov_rcbd_2$`Mean Sq`[3]) / length(distance) 



## compute (direct)
a <- length(distance)
b <- length(person)

sum_trt <- tapply(dat$value, dat$distance, sum)
sum_block <- tapply(dat$value, dat$person, sum)

y_total <- sum(dat$value)

ss_total <- sum((dat$value)^2) - y_total^2/a/b
ss_trt <- sum(sum_trt^2)/b - y_total^2/a/b
ss_block <- sum(sum_block^2)/a - y_total^2/a/b
ss_err <- ss_total - ss_trt - ss_block

ms_trt <- ss_trt / (a-1)
ms_block <- ss_block / (b-1)
ms_err <- ss_err / (a*b - 1 - (a - 1) - (b - 1))

sigma_beta_2_2 <- (ms_block - ms_err)/a




#1-4
## using lme4 packages
install.packages("lme4")
library(lme4)
lme_fit <- lmer(value ~ distance + (1|person), data = dat)
vc <- as.data.frame(VarCorr(lme_fit))

# compute (direct)



# compare the methods
c(sigma_beta_2, sigma_beta_2_2,vc[1,4]) # equal result



# 2.

# create data set
order <- factor(1:4)
person <- factor(1:4)



# input using for function[아래로 내리는 방식]
rp <- c("C","B","A","D")
rp <- c(rp,rp[1:3])

trt <- c()

for(i in 1:4){
  for(j in 1:4){
    trt[4*(i-1) + j] <- rp[(9 - i)%%4 + j]
  }
}

trt2 <- c()
for(i in 1:4){
  trt2[4*(i-1) + 1:4] <- rp[(9-i)%%4 + 1:4]
}

trt;trt2

dat_latin <- expand.grid(order = order , person = person)

dat_latin$trt <- factor(trt2)

# input y value
dat_latin$value <- c(10, 7, 5, 10,
                     14, 18, 10, 10,
                     7, 11, 11, 12,
                     8, 8, 9, 14)

# fit latin square design
fit_latin_2 <- aov(value ~ person + order + trt,
                   data = dat_latin)

# check anova table
anova(fit_latin_2)


