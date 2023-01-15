rm(list=ls())

# Chapter 7: Blocking and Confounding Systems for Two-Level Factorials

# Example 7.1
A = c(-1,1); B = c(-1,1)
blk = factor(1:3)

dat7_1 = expand.grid(A=A, B=B, blk=blk)
dat7_1$y = c(28,36,18,31,
             25,32,19,30,
             27,32,23,29)

blk_total = tapply(dat7_1$y, dat7_1$blk, function(x) sum(x)); blk_total
ss_blk = sum(blk_total^2/4) - sum(dat7_1$y)^2/12; ss_blk

summary(aov(y ~ A * B + blk, data = dat7_1))


# Confounding the 2^k Factorial Design in Two Block (p=1)
A <- B <- C <- c(-1, 1)
dat = expand.grid(A=A, B=B, C=C)

nfactor = 3
x = dat
cname = colnames(dat)

make_rname = function(x, nfactor, cname)
{
  subdat = x[,1:nfactor]
  rname = rep(0,nrow(x))
  
  for(i in 1:nrow(x))
  {
    bool = subdat[i,] == 1
    if(sum(bool)==0) rname[i] = "(1)"
    else rname[i] = paste(tolower(cname[bool]), collapse = "")
  }
  return(rname)
}

rname = make_rname(dat, 3, colnames(dat))
rownames(dat) = rname


tmp_dat = dat
tmp_dat[tmp_dat == -1] = 0
blk_idx = apply(tmp_dat, 1, function(x) sum(x) %% 2)


blk1 = dat[!as.logical(blk_idx),]; blk1
blk2 = dat[as.logical(blk_idx),]; blk2



# --------------------------------------

rm(list=ls())

# Chapter 7
# Blocking and Confounding Systems for Two-Level Factorials (part 2)

make_rname = function(x, nfactor, cname)
{
  subdat = x[,1:nfactor]
  
  rname = rep(0,nrow(x))
  
  for(i in 1:nrow(x))
  {
    bool = subdat[i,] == 1
    if(sum(bool)==0) rname[i] = "(1)"
    else rname[i] = paste(tolower(cname[bool]),collapse = "")
  }
  return(rname)
}


# Example 7.2
A = B = C = D = c(-1,1)
dat7_2 = expand.grid(A=A,B=B,C=C,D=D)
dat7_2$y = c(25,71,48,45,
             68,40,60,65,
             43,80,25,104,
             55,86,70,76)

rname = make_rname(dat7_2, 4, colnames(dat7_2)[1:4])
rownames(dat7_2) = rname
dat7_2

tmp_dat7_2 = dat7_2[,1:4]
tmp_dat7_2[tmp_dat7_2 == -1] = 0
blk_idx = apply(tmp_dat7_2, 1, function(x) sum(x) %% 2)

blk1 = dat7_2[!as.logical(blk_idx),]; blk1
blk2 = dat7_2[as.logical(blk_idx),]; blk2

cont = model.matrix( ~ -1 + A * B * C * D, data = dat7_2)

effect = dat7_2$y %*% cont / 8; effect
ss = (dat7_2$y %*% cont)^2 / 16; ss

blk_eff = mean(blk1$y) - mean(blk2$y); blk_eff

dat7_2$blk = dat7_2$A * dat7_2$B * dat7_2$C * dat7_2$D
anova(aov(y ~ blk + A + C + D + A:C + A:D, data = dat7_2))

# Confounding the 2^k Factorial Design in Four Blocks
E = c(-1,1)

dat7_3 = expand.grid(A=A, B=B, C=C, D=D ,E=E)

rname2 = make_rname(dat7_3, 5,colnames(dat7_3))
rownames(dat7_3) = rname2
dat7_3

L1 = c(1,0,0,1,1); L2 = c(0,1,1,0,1)

dat7_3[dat7_3 == -1] = 0

L1_idx = apply(dat7_3, 1, function(x) (sum(x * L1)) %% 2)
L2_idx = apply(dat7_3, 1, function(x) (sum(x * L2)) %% 2)

blk1 = dat7_3[(!as.logical(L1_idx)) & (!as.logical(L2_idx)),]; blk1
blk2 = dat7_3[(as.logical(L1_idx)) & (!as.logical(L2_idx)),];  blk2
blk3 = dat7_3[(!as.logical(L1_idx)) & (as.logical(L2_idx)),];  blk3
blk4 = dat7_3[(as.logical(L1_idx)) & (as.logical(L2_idx)),];   blk4


