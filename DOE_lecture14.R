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

# Calculation of Dispersion Effects
new_cont = cont[,vec_idx]

res_fit = residuals(fit)

s_ip = s_im = rep(0, ncol(new_cont))

for(i in 1:ncol(new_cont))
{
  s_ip[i] = sd(res_fit[new_cont[,i]==1])
  s_im[i] = sd(res_fit[new_cont[,i]==-1])
}

s_ip; s_im

f_istar = log(s_ip^2/s_im^2)

par(mfrow=c(1,1))
qvalue = qqnorm(f_istar, ylim = c(-1,3))
qqline(f_istar)
text(x = qvalue$x, y = f_istar + 0.2, labels = tolower(colnames(new_cont)))

tapply(dat8_4$y, list(A=dat8_4$A, B=dat8_4$B, C=dat8_4$C), mean)
tapply(dat8_4$y, list(A=dat8_4$A, B=dat8_4$B, C=dat8_4$C), function(x) max(x)- min(x))



# The General 2^(k-p) Fractional Factorial Design

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

# Example : Resolution 4 2^(7-3) Fractional Factorial Design
# basic design
A = B = C = D = c(-1,1)
dat = expand.grid(A=A, B=B, C=C, D=D)

# Defining relation : I=ABCE, I=BCDF, I=ACDG
dat$E = apply(dat, 1, function(x) prod(x[1:3]))  # E = ABC
dat$f = apply(dat, 1, function(x) prod(x[2:4]))  # F = BCD
dat$G = apply(dat, 1, function(x) prod(x[c(1,3,4)]))  # G = ACD



