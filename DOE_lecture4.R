rm(list = ls())

# post anova
power <- c("160", 180, 200, 220)
times <- 1:5

dat <- expand.grid(times = times , power = power)

dat$value = c(575,542,530,539,570,
              565,593,590,579,610,
              600,651,610,637,629,
              725,700,715,685,710)


# 1.orthogonal contrast

c_mat <- matrix(c(1,-1,0,0
                  ,1,1,-1,-1,
                  0,0,1,-1), ncol = 3)


contrasts(dat$power) <- c_mat
attr(dat$power,"contrasts") <- c_mat

fit_cont <- aov(value ~ power, data = dat)

summary(fit_cont, split = list(power = list("C1 : mu_1 = mu_2" = 1,
                                                "C2 : mu_1 + mu_2 = mu_3 + mu_4" = 2,
                                                "C3 : mu_3 = mu_4" = 3)))


# 2. scheffe's method
library(agricolae)
res_scheffe <- scheffe.test(fit_cont, trt = "power")
res_scheffe
plot(res_scheffe)


# 3. LSD : family error rate 원하는 유의수준으로 control하는게 불가능
res_LSD <- LSD.test(fit_cont, trt = "power")
plot(res_LSD)

# 4. Tukey's Test : family error rate control 할 수 있다.
res_Tukey <- TukeyHSD(fit_cont, trt = "power", conf.level = 0.95)
plot(res_Tukey)


# 5. Dunnett test
library(DescTools)
DunnettTest(dat$value, dat$power, control = "220", conf.level = 0.95)



## random effect

looms <- factor(1:4)
times <- 1:4

dat_2 <- expand.grid(times = times, looms = looms)

dat_2$value <- c(98,97,99,96,
                 91,90,93,92,
                 96,95,97,95,
                 95,96,99,98)

fit_2 <- aov(value ~ looms, data = dat_2)
res_fit_2 <- anova(fit_2)

sigma2 <- res_fit_2$`Mean Sq`[2]
sigma_tau2 <- (res_fit_2$`Mean Sq`[1] - res_fit_2$`Mean Sq`[2])/4



c(12 * sigma2 /qchisq(0.975,12), 12 * sigma2 /qchisq(0.025,12)) 


# Exercise
cotton=c("15",20,25,30,35)
times<-1:5
dat<-expand.grid(times=times, cotton=cotton)
dat$value<-c(7,7,15,11,9,
             12,17,12,18,18,
             14,19,19,18,18,
             19,25,22,19,23,
             7,10,11,15,11)
dat

#1. 
fit<-aov(value~cotton, data = dat)
summary(fit)

#2. 
#.LSD
res_LSD=LSD.test(fit, trt = "cotton")
res_LSD
plot(res_LSD)

# Tukey
res_Tukey=TukeyHSD(fit, trt="cotton",conf.level = 0.99)
plot(res_Tukey)

# Scheffe
res_scheffe=scheffe.test(fit, trt = "cotton")
res_scheffe
plot(res_scheffe)

# compare LSD vs. Scheffe
x11();par(mfrow=c(1,2))
plot(res_LSD,main="LSD");plot(res_scheffe,main = "Scheffe")

res_LSD
res_scheffe

# 3.
res_fit<-anova(fit)
sigma2<-res_fit$`Mean Sq`[2];sigma2
sigma_tau2<-(res_fit$`Mean Sq`[1]-res_fit$`Mean Sq`[2])/5;sigma_tau2
c(20 * sigma2 /qchisq(0.975,20), 12 * sigma2 /qchisq(0.025,20)) 


































