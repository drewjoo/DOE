# remove object
rm(list = ls())

# example 3.1
power <- factor(c(160, 180, 200, 220))
times <- 1:5


df <- expand.grid(times = times , power = power)
df$value <- c(575,542,530,539,570,
              565,593,590,579,610,
              600,651,610,637,629,
              725,700,715,685,710)


# boxplot & scatter plot
x11()
par(mfrow = c(1,2))
boxplot(value ~ power, data = df)
plot(as.character(df$power), df$value)


## one way anova
fit1 <- aov(value ~ power, data = df)
summary_fit1 <- anova(fit1)

fit2 <- lm(value ~ power, data = df)
anova(fit2)

fit3 <- lm(value ~ as.numeric(as.character(power)), data = df)
anova(fit3)


## decomposition sum of square
ss_vec <- summary_fit1$`Sum Sq`
ss_total <- sum(ss_vec)


## checking normal assumption

# nomality
x11()
par(mfrow = c(1,1))
qqnorm(fit1$residuals)
qqline(fit1$residuals)

shapiro.test(fit1$residuals)

# independence
x11()
par(mfrow = c(2,2))
plot(fit1)

# constant variance
## levene test
install.packages("lawstat")
library(lawstat)

levene.test(df$value, df$power, location = "mean")

## plot of residuals versus run order or time
set.seed(22123)
idx <- sample(1:20, 20)
idx

fit1$residuals[idx]
plot(fit1$residuals[idx])
abline(h = 0, col = "red", lwd = 3)

# Practice
treat<-factor(LETTERS[1:4])
time=1:4
value=c(3135,3001,2865,2890,
        3210,3311,2975,3150,
        2815,2910,2985,3050,
        2619,2706,2612,2765)
df<-expand.grid(time=time, treat=treat)
df$value<-value
df
head(df)
#1.
boxplot(value~treat, data=df)
#2.
fit1<-aov(value~treat, data = df)
anova(fit1)
#3.
# normality
fit1$residuals
qqnorm(fit1$residuals)
qqline(fit1$residuals, col="red", lwd=2)
shapiro.test(fit1$residuals)

# independence
x11();par(mfrow=c(2,2))
plot(fit1)

# constant variance
levene.test(df$value, df$treat,location = "mean")

set.seed(234)
idx<-sample(1:16, size=16)
idx
plot(fit1$residuals[idx])
abline(h=0, col="red", lwd=2)
