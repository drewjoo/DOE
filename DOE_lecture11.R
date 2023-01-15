rm(list=ls())

# Chapter 6: Two-Level Factorial Experiments ######
# 2^2 - design
# Orthogonal coding
A = c(-1,1) # A: reactant concentration (1: high , -1: low)
B = c(-1,1) # B: catalyst amount (1: high , -1: low)
times = 1:3 # n: replicates

dat6_2 = expand.grid(times=times, A=A, B=B)

dat6_2$y = c(28,25,27, #y: recovery
             36,32,32,
             18,19,23,
             31,30,29)
dat6_2


# Estimation of Factor effects using orthogonal contrasts
# e.g. eff_A = (contrast_A) / (2n)
cont = model.matrix( ~ -1 + A * B, data=dat6_2)
eff_vec = dat6_2$y %*% cont / (2*3); eff_vec


# Sum of squares of source variables using orthogonal contrast
# e.g. SS_A = (contrast_A)^2/ (4n)
ss_vec = (dat6_2$y %*% cont)^2 / (4*3)
summary(aov(y ~ A * B, data=dat6_2))


# Fitted regression model (Corrected Model)
fit_6_2 = lm(y ~ A + B, data=dat6_2)
summary(fit_6_2)


# Residuals and Model Adequacy
par(mfrow=c(2,2))
plot(fit_6_2)
par(mfrow=c(1,1))


# install.packages("rsm")
library(rsm)
persp(fit_6_2, B ~ A, zlim = c(15,35))
contour(fit_6_2, B ~ A, levels = seq(23,33,2))


# Chapter 6: Two-Level Factorial Experiments (Part2)

# The 2^3 Factorial Design
# Example 6.1
A = c(-1,1)
B = c(-1,1)
C = c(-1,1)
times = 1:2

dat6_1 = expand.grid(times=times, A=A, B=B, C=C)
dat6_1$y = c(550,604,
             669,650,
             633,601,
             642,635,
             1037,1052,
             749,868,
             1075,1063,
             729,860)

cont = model.matrix( ~ -1 + A * B * C, data=dat6_1)

eff_vec = dat6_1$y %*% cont / (2^2 * 2 ); eff_vec
ss_vec = (dat6_1$y %*% cont)^2 / (2^3 * 2); ss_vec

summary(aov(y ~ A * B * C, data=dat6_1))  # Full model
summary(aov(y ~ A * C, data=dat6_1))  # Reduced model

fit6_1 = lm(y ~ A * C, data=dat6_1)
eff_vec/2

library(rsm)
persp(fit6_1, C ~ A, zlim = c(500,1057))
contour(fit6_1, C ~ A, levels = seq(673.625,980.125,length.out=5))

summary(fit6_1)


#method 1 
dat6_1$idx <- 1:dim(dat6_1)[1]

res <- c()
for(i in 1:dim(dat6_1)[1]){
  pre_fit <- lm(y ~ A*C, data = dat6_1, subset = idx != i)
  res[i] <- dat6_1[i,'y']- predict(pre_fit, newdata = dat6_1[i,])
}

sum(res^2)

# method 2 
#install.packages("qpcR")
library(qpcR)
res = PRESS(fit6_1); res

sum(res$residuals^2)


# The General 2^k Factorial Design
# A Single Replicate of the 2^k  Design
# Example 6.2
A = c(-1,1); B = c(-1,1); C = c(-1,1); D = c(-1,1) 

dat6_2 = expand.grid(A=A, B=B, C=C, D=D)

dat6_2$y = c(45,71,48,65,
             68,60,80,65,
             43,100,45,104,
             75,86,70,96)

cont = model.matrix( ~ -1 + A * B * C * D, data=dat6_2); cont

eff_vec = dat6_2$y %*% cont / 8 ; eff_vec   # 2^(4-1)
ss_vec = (dat6_2$y %*% cont)^2 / 16; ss_vec # 2^4

# Normal Probability Plot of the Effects
qvalue = qqnorm(eff_vec, ylim=c(-20,25))
qqline(eff_vec)
text(x = qvalue$x, y = qvalue$y + 2, label = colnames(eff_vec))

# Main effect plot for A
subdat6_2 = tapply(dat6_2$y, dat6_2$A, mean)
plot(x=c(-1,1), y=subdat6_2, type='b',
     xlab="A", ylab="Average filtration rate (gal/h)",
     xlim=c(-1.5,1.5), ylim=c(50,90))

# Interaction plots for A and C
interaction.plot(dat6_2$A, dat6_2$C, dat6_2$y, 
                 legend=F, ylim=c(40,100), main="AC interaction",
                 xlab="A", ylab="Average filtration rate (gal/h)",
                 col=c('blue','red'), xaxt="n")
axis(1, at=c(1,2), labels=c("-","+"), lwd.ticks=0.5)
legend("topleft", legend = c("C = -", "C = +"), col = c("blue","red"), lty = 2:1)

text(locator(1), labels = "C = -", col = "blue")
text(locator(1), labels = "C = +", col = "red")



# ANOVA for response in A,C and D
summary(aov(y ~ A * C * D, data=dat6_2))  # full-model # b가 제거되어 full model로 적합 가능

fit = lm(y ~ A + C + D + A:C + A:D, data=dat6_2)
summary(fit)

coef(fit)[-1]
eff_vec[c(1,3,4,6,8)]/2 # Checking that regression coefficients are same as half of effect estimates

par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(1,1))
contour(lm(y ~ A * C, data = dat6_2, subset = D==1), C ~ A, levels = seq(50, 90, by=10))
contour(lm(y ~ C * D, data = dat6_2, subset = A==1), D ~ C, levels = seq(65, 100, by=5))








