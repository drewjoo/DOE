rm(list=ls())

# 8.2 One-Half Fraction of the 2^k factorial design
# Defining Relation: I = ABC

make_rname = function(x,nfactor, cname)
{
  subdat = x[,1:nfactor]
  
  rname = rep(0,nrow(x))
  
  for(i in 1:nrow(x))
  {
    bool = subdat[i,] == 1
    if(sum(bool)==0) rname[i] = "(1)"
    else rname[i] = paste0(tolower(cname[bool]),collapse = "")
  }
  return(rname)
}


# Example 8.1 
A = B = C = c(-1,1)

dat8_1 = expand.grid(A=A, B=B, C=C)

dat8_1$D = apply(dat8_1, 1, function(x) prod(x))

rownames(dat8_1) = make_rname(dat8_1, 4, colnames(dat8_1))

dat8_1$y = c(45,100,45,65,75,60,80,96)

cont = model.matrix( ~ -1 + A * B * C * D, data=dat8_1)

# Find Alias Structure
ali_str = alias(lm(y ~ A * B * C * D, data=dat8_1))
ali_str$Complete

eff_vec = (dat8_1$y %*% cont[,1:7]) / 4
eff_vec

ss_vec = (dat8_1$y %*% cont[,1:7])^2 / 8
ss_vec

fit = aov(y ~ A + B + C + D + A:B + A:C + B:C, data=dat8_1)
summary(fit)


# Example 8.2
A = B = C = D = c(-1,1)

dat8_2 = expand.grid(A=A, B=B, C=C, D=D)
dat8_2$E = apply(dat8_2, 1, function(x) prod(x))

rownames(dat8_2) = make_rname(dat8_2, 5, colnames(dat8_2))

dat8_2$y = c(8,9,34,52,
             16,22,45,60,
             6,10,30,50,
             15,21,44,63)

cont = model.matrix( ~ -1 + A * B * C * D * E, data=dat8_2)

ali_str = alias(lm(y ~ A * B * C * D * E, data=dat8_2))
ali_str$Complete

colnames(ali_str$Complete[,-1])

n_eff = ncol(ali_str$Complete) - 1
n_eff

colnames(cont[,1:n_eff])

eff_vec = (dat8_2$y %*% cont[,1:n_eff]) / 8
ss_vec = (dat8_2$y %*% cont[,1:n_eff])^2 / 16

eff_vec; ss_vec

qvalue = qqnorm(eff_vec, ylim=c(-5,40))
qqline(eff_vec)

text(x = qvalue$x, y = eff_vec + 1, labels = colnames(eff_vec))

fit = aov(y ~ A + B + C + A:B, data=dat8_2)
summary(fit)

# refer to summary values 
summary(fit)[[1]][2,2]
summary(fit)[[1]][3,2]
summary(fit)[[1]][2,4]

fit_value = anova(fit)
fit_value$`Sum Sq`
fit_value$`Mean Sq`
fit_value$`F value`

# Diagnostics
par(mfrow=c(2,2))
plot(fit)



# 2^(6-2) design
# Example 8.4
A = B = C = D = c(-1,1)

dat8_4 = expand.grid(A=A, B=B, C=C, D=D)

dat8_4$E = apply(dat8_4, 1, function(x) prod(x[1:3])) # E = ABC
dat8_4$f = apply(dat8_4, 1, function(x) prod(x[2:4])) # F = BCD

rownames(dat8_4) = make_rname(dat8_4, 6, colnames(dat8_4))

cont = model.matrix( ~ -1 + (.)^6, data=dat8_4) # A*B*C*D*E*F == (A+B+C+D+E+F)^6 == (.)^6

dat8_4$y = c(6,10,32,60,
             4,15,26,60,
             8,12,34,60,
             16,5,37,52)

ali_str = alias(lm(y ~ (.)^6, data=dat8_4))
ali_str$Complete

eff_var = colnames(ali_str$Complete)[-1]

which(colnames(cont) %in% eff_var)

vec_idx = which(colnames(cont) %in% eff_var)

eff_vec = (dat8_4$y %*% cont[,vec_idx]) / 8
ss_vec = (dat8_4$y %*% cont[,vec_idx])^2 / 16
reg_coef = eff_vec / 2

eff_vec ; ss_vec; reg_coef

# Q-Q plot for effects
par(mfrow=c(1,1))
qvalue = qqnorm(eff_vec, ylim = c(-10,45))
qqline(eff_vec)

text(x = qvalue$x, y = eff_vec + 1, labels = colnames(eff_vec))

fit = aov(y ~ A * B, data = dat8_4)
summary(fit)

par(mfrow=c(1,2))
qqnorm(fit$residuals)
qqline(fit$residuals)

plot(x = dat8_4$C, y = fit$residuals,
     xlab = 'C', ylab = 'Residuals')
abline(h=0, col='red')