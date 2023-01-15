mtype <- factor(1:3)
temp <- factor(c(15,70,125))
time <- 1:4

dat <- expand.grid(time = time ,mtype = mtype, temp = temp)

dat$value <- c(130, 155, 74, 180,
               150, 188, 159, 126,
               138, 110, 168, 160, 
               34, 40, 80, 75, 
               136, 122, 106, 115,
               174, 120, 150, 139, 
               20, 70, 82, 58, 
               25, 70, 58, 45, 
               96, 104, 82, 60)

# no interaction vs interaction

## interaction

fit <- aov(value ~ temp*mtype, data = dat)
anova(fit)

# no interaction
fit2 <- aov(value ~ temp + mtype, data = dat)
anova(fit2)


x11()
par(mfrow = c(2,2))
plot(fit)


# triple factor factorial design
A <- factor(seq(10,14,2))
B <- factor(c(25,30))
C <- factor(c(200,250))
times = 1:2

dat_5.3 <- expand.grid(times = times, A= A, C = C, B = B)
dat_5.3$value <- c(-3, -1, 0, 1, 5, 4, -1, 0, 2, 1, 7, 6,
                   -1, 0, 2, 3, 7, 9, 1, 1, 6, 5, 10, 11)

triple_fit <- aov(value ~ A + B + C + A:B + A:C + B:C + A:B:C, data = dat_5.3)
anova(triple_fit)
triple_fit2 <- aov(value ~ A*B*C, data = dat_5.3)
anova(triple_fit2)

reduce_fit <- aov(value ~ A + B + C + A:B, data = dat_5.3)
anova(reduce_fit)

reduce_fit2 <- aov(value ~ A*B + C, data = dat_5.3)
anova(reduce_fit2)


# interaction plot
x11(); par(mfrow = c(2,3))
interaction.plot(dat$temp, dat$mtype, response = dat$value, type = "l", main = "Type l")
interaction.plot(dat$temp, dat$mtype, response = dat$value, type = "p", main = "Type p")
interaction.plot(dat$temp, dat$mtype, response = dat$value, type = "b", main = "Type b")
interaction.plot(dat$temp, dat$mtype, response = dat$value, type = "o", main = "Type o")
interaction.plot(dat$temp, dat$mtype, response = dat$value, type = "c", main = "Type C")





















