
# main effect plot

main_eff_speed <- tapply(dat_5_3$value, dat_5_3$speed, mean)
plot(main_eff_speed, type = "l", xlim = c(0,3), ylim = c(-2,8))





# 5. Factorial Experiments (Part 2)
# Example 5.4 (Exmpale 5.1 Battery Life Experiment continued)
Mtype = factor(1:3)
temp = factor(c(15,70,125))
times = factor(1:4)

dat_5_4 = expand.grid(times=times, temp=temp, Mtype=Mtype)

dat_5_4$value = c(130,155,74,180,  34,40,80,75,     20,70,82,58,
                  150,188,159,126, 136,122,106,115, 25,70,58,45,
                  138,110,168,160, 174,120,150,139, 96,104,82,60)


fit_origin = aov(value ~ Mtype * temp, data=dat_5_4)
summary(fit_origin)

str(dat_5_4)
dat_5_4$temp = as.numeric(as.character(dat_5_4$temp))
str(dat_5_4)

fit_5_4 = aov(value ~ Mtype * temp + Mtype * I(temp^2), data=dat_5_4)
summary(fit_5_4)

fit_mtype = list()

for(i in 1:3){
  fit_mtype[[i]] = lm(value ~ temp + I(temp^2), data=dat_5_4, subset = Mtype==i)
  print(summary(fit_mtype[[i]]))
} # subset이라는 옵션을 사용하면 True인 행만 가지고 fittinh을 하라는 의미

summary(fit_mtype[[1]])


par(mfrow=c(1,1))

plot(dat_5_4$temp, dat_5_4$value, pch=16,
     ylim=c(min(dat_5_4$value),max(dat_5_4$value)), 
     xaxt='n', yaxt='n',
     ylab="Life", xlab="Temperature")
axis(1, seq(15,125,by=27.5))
axis(2, seq(20,188,by=42))

for(i in 1:3){
  lines(seq(15,125), predict(fit_mtype[[i]], data.frame(temp=seq(15,125))))
}

text(locator(1),"Material type 1")
text(locator(1),"Material type 2")
text(locator(1),"Material type 3")



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













