
rm(list = ls())

# -- set working directory ---
getwd()
setwd("C:/Users/Administrator/OneDrive/바탕 화면/실험계획실습")
setwd("C:/Users/Administrator/OneDrive/바탕 화면/실험계획실습/2주차")


# --- Data load ---
exam1 <- read.table("example_1.txt", sep = '\t', header = T)
exam1

exam2 <- read.csv("example_2.csv", header = T)
exam2

head(exam2)
head(exam2, 3)
tail(exam2, 2)

install.packages("data.table")
library("data.table")
exam3 <- fread("example_1.txt")

# --- Data save ---

write.table(exam1,file="cars.txt", sep="\t", row.names = F)
write.csv(exam2,file="iris.csv",row.names = T)

fwrite(exam1, file = "f_cars.txt", row.names = F)
fwrite(exam2, file = "f_iris.csv", row.names = T)


# --- Value type ---
# 1) Integer
a <- 1:5
a
typeof(a)
class(a)

# 2) Double
b <- c(1.2, 2.2, 3.3, 1.4, 1.6); b
typeof(b)

# 3) Character
d <- c("Kim", "Lee", "Choi", "Pack", "Kang")
d <- c("Kim", "Lee", "Choi", "Pack", 'Kang')
typeof(d)

# 4) Logical
e <- c(TRUE,FALSE,T,T,F); e
typeof(e)


# --- Object ---
# 1) Vector object
x <- c(1,2,3,4,5,6,7); x

x[3]
x[3:5]
x[c(4,2,5,7)]

c(1,2,3,"a")
c(1,2,F)
c(1,"2",F)
c(F)

x[6] <- 9; x
x[8] <- 10; x

# 2) Matrix object
test_matrix <- matrix(1:9, nrow=3, byrow=F, 
                      dimnames = list(c("a","b","c"),c("d","e","f"))); test_matrix




test_matrix[3,1]
test_matrix[1,]
test_matrix[,2]
test_matrix[c(1,3),2]

# 3) Factor object
x_fac <- factor(c("1","2","3","2","1","1"), levels= 1:5); x_fac
x_fac[3]
x_fac[3:5]
x_fac[7] <- "5"
x_fac

# 4) Dataframe Object
dat2 <- data.frame(num = c(rep(0,3),seq(1,2)), 
                   alpha = c("a","b","c","d","e"), stringsAsFactors = F)

dat2
dat2[3,1]
dat2[,1]  
dat2[3,]

dat2$num
dat2$alpha  

# 5) List object
test_list <- list(x = x, dat2 = dat2, mat = test_matrix); test_list

test_list[[1]]
test_list[[1]][3]
test_list$x[3]

test_list2 <- list(x_fac = x_fac, test_list = test_list); test_list2

test_list2[[2]]
test_list2[[2]][[2]]
test_list2[[2]][[2]][,1]


# --- type 확인 ---
x <- 5
is.numeric(x)
is.character(x)


# --- type 변환 ---
as.character(123)
as.numeric(c(T,F,T,T,F))
as.factor(c("apple","banana"))
is.null(NULL)
is.na(NA)

# --- 사칙연산 ---
x <- c(1,2,3,4); x

x + 3
x - 3
x * 3
x / 3
x * (3 + 2)

x^3
x %/% 3 # 해
x %% 3 # 나머지

y <- c(1,3)
z <- c(1,4,8)

x + y
x - z


# --- 비교연산자 ---
x <- c(1,3,6,9)
y <- c(1,2,3)
z <- c(2,4,6,10)

x > y
x >= z
x == z
x != z


# --- 논리연산자 ---
# 1) And 연산자
x <- c(T,T,F)
y <- c(F,T,F)

x & y
x && y

# 2) Or 연산자
x | y
x || y


# --- 조건문 ---
# 1) if statement
if(T) {print("Logic is True")}
if(F) {print("Logic is True")}

if(c(T,F)) {print("Logic is True")}
if(c(F,T)) {print("Logic is True")}

if(all(c(T,F))) {print("Logic is True")}
if(any(c(F,T))) {print("Logic is True")}

if((5>2) & (0>1)) {print("Out")}
if((5>2) | (0>1)) {print("Out")}

# 2) if-else statement 
if(F) {
  print("Logic is True")
} else {
  print("Logic is False")
}

score <- 95

if(score >= 90){
  print("A")
} else if(score >= 80) { 
  print("B")
} else {  
  print("C") 
}

score <- c(95,80)

if(all(score >= 90)){
  print("A")
} else if(all(score >= 80)) { 
  print("B")
} else {  
  print("C") 
}

# 3) ifelse statement
ifelse(c(1,2,3) > c(1,1,1), "Logic is True", "Logic is false")

score <- c(85,95,75)
ifelse(score >= 90,"A",ifelse(score>=80,"B","C"))

# --- 반복문 ---
# 1) for statement
for(i in 1:10)
{
  print(i)
}

for(i in c(1,3,5,7,9))
{
  print(i)
}

for(i in LETTERS)
{
  print(i)
}

sum = 0
for(i in 1:100)
{
  sum = sum + i
}
sum

sum_even = 0
for(i in 1:50)
{
  if(i%%2==0)
  {
    sum_even = sum_even + i
  }
}
sum_even

# 2) while statement
sum2 = 0
i = 1
while(i <= 100)
{
  sum2 = sum2 + i
  i = i + 1
}
sum2

sum2_even = 0
i = 1
while(i <= 50)
{
  if(i%%2==0)
  {
    sum2_even = sum2_even + i
  }
  i = i + 1
}
sum2_even




# ------------------------------------------------ #
# Chapter 2 
rm(list=ls())

# --- Protland Cement Formulation ---
setwd("C:/Users/Administrator/OneDrive/바탕 화면/실험계획실습/2주차")
dat_2_1 <- read.table("example_2_1.txt", sep="\t", header = T)
dat_2_1

# --- Graphical View of the Data ---
# 1) Dot Diagram
x11()
stripchart(list('Unmodified'=dat_2_1$y2, 'Modified'=dat_2_1$y1), 
           ylim=c(1,3), pch=16, xaxt="n", xlab="Strength(kgf/cm^2)")

axis(1, at=seq(16.38,17.36,by=0.14), lwd=1.5)
axis(1, at=seq(16.38,17.36,by=0.14), lwd=1.5, pos=1.9)




# 2) Box Plots
y1 <- dat_2_1$y1  # Modified
y2 <- dat_2_1$y2  # Unmodified

boxplot(list('Modified'=y1, 'Unmodified'=y2), 
        xlab="Mortal formulation",
        ylab=expression(Strength(kgf/cm^2)), yaxt = "n", col = c("white","red"), border = "brown")
pnorm(1.96)

# boxplot(list('Modified'=y1, 'Unmodified'=y2), 
# xlab="Mortal formulation", 
# ylab=expression(Strength(kgf/cm^2)), yaxt = "n", col = c("orange", "red"), border = "brown", at = c(1,4), notch = T)

axis(2, at=seq(16.50,17.5,by=0.25))


# --- Hypothesis Testing Framework ---
# 1) Estimates the population mean 
y1_bar = mean(y1); y1_bar
y2_bar = mean(y2); y2_bar

n1 <- length(y1)
n2 <- length(y2)

sum(y1)/n1; y1_bar
sum(y2)/n2; y2_bar

# 2) Estimates the population variance
y1_sig2 = var(y1); y1_sig2
y2_sig2 = var(y2); y2_sig2

sum((y1 - y1_bar)^2)/(n1-1); y1_sig2
sum((y2 - y2_bar)^2)/(n2-1); y2_sig2

# 3) Estimates the population standard deviation
y1_sig = sd(y1); y1_sig
y2_sig = sd(y2); y2_sig

sqrt(y1_sig2); y1_sig
sqrt(y2_sig2); y2_sig


# --- Probability distribution ---
# 1) Generate random numbers from such distribution
set.seed(1)
x <- rnorm(n=10000, mean=0, sd=1); x

# 2) Density function
dnorm(0, mean=0, sd=1)
1/sqrt(2*pi)*exp(-(0)^2/2)

x2 <- seq(-4,4,length.out=1000)

plot(x2, dnorm(x2, mean=0, sd=1), type="l", main = "X~Normal(0,1)")
lines(density(x),col='red')

# 3) Cumulative Density function

pnorm(1.96,mean=0,sd=1)

# 4) Quantile function
qnorm(0.975,mean=0,sd=1)
qnorm(0.025,mean=0,sd=1)


# --- 1. If the variances are known ---
# Test statistics: Z0 (two-side test)
# If sigma = sigma1 = sigma2 = 0.3
z0 = (y1_bar - y2_bar) / sqrt(0.3^2 * (1/n1 + 1/n2)); z0

qnorm(0.025) # critical value of alpha = 0.05
2*pnorm(z0)  # Reject H0 under alpha = 0.05

# Using BSDA Package
# install.packages("BSDA")
library(BSDA)
z.test(y1, y2, sigma.x=0.3, sigma.y=0.3)


# --- 2. If the variances are unknown ---
# 1) Case 1: n > 30 
#  => Sample sizes were large enough => Z-test
z0 = (y1_bar-y2_bar) / sqrt(y1_sig2/n1 + y2_sig2/n2); z0

qnorm(0.025) ## critical value of alpha = 0.05
2*pnorm(z0) ## Reject H0 under alpha = 0.05

# 2-1) Case 2-1:  n < 30 (Sample sizes were small) & sigma1 == sigma2 
#  => t-test using pooled estimator
n =  n1 + n2 - 2
s_pool2 = ((n1-1)*y1_sig2 + (n2-1)*y2_sig2) / n 

t0 = (y1_bar-y2_bar) / (sqrt(s_pool2) * sqrt(1/n1 + 1/n2)); t0

qt(0.025,n)
2*pt(t0,n)

t_res <- t.test(y1, y2, alternative = "two.sided", var.equal=T) 
t_res

t_res$statistic   # t statistic
t_res$conf.int[1:2] # 95% confidence interval
t_res$p.value # p-value

# 95% confidence interval
# (추정량 - t값 * 추정된 표준오차, 추정량 + t값 * 추정된 표준오차)
diff = y1_bar-y2_bar
t = qt(0.025, df = n, lower.tail=F); t
interval = t * sqrt(s_pool2) * sqrt(1/n1 + 1/n2)

# t = qt(0.025, df = n, lower.tail=T); t
# interval = abs(t) * sqrt(s_pool2) * sqrt(1/n1 + 1/n2)

c(diff - interval, diff + interval) 


# 2-2) Case 2-2:  n < 30 (Sample sizes were small) & sigma1 != sigma2 
t0 = (y1_bar-y2_bar) / sqrt(y1_sig2/n1 + y2_sig2/n2); t0
v = ((y1_sig2/n1) + (y2_sig2/n2))^2 / 
  (((y1_sig2/n1)^2 / (n1-1)) + ((y2_sig2/n2)^2 / (n2-1))); v

t_res2 <- t.test(y1, y2, alternative="two.sided", var.equal=F); t_res2
t_res2$parameter # d.f.

# --- Tests on Variance of Normal Distribution ---
# 1) Case 1: One-sample test on variance of Normal Distribution
# H0 : sigma1^2 = 0.15 vs H1 : sigma1^2 < 0.15
chisq0 = ((n1-1) * y1_sig2) / 0.15; chisq0

# install.packages("EnvStats")
library(EnvStats)
res = varTest(y1, alternative="less", sigma.squared=0.15); res
res$statistic
res$p.value


# H0 : sigma2^2 = 0.05 vs H1 : sigma2^2 > 0.05
varTest(y2, alternative="greater", sigma.squared=0.05)

# 2) Case 2: Two-Sample test on variance of Normal Distribution
# H0 : sigma1^2 = sigma2^2 vs H1 : sigma1^2 != sigma2^2  
f0 = y1_sig2/y2_sig2; f0
var.test(y1, y2, alternative = "two.sided")

# H0 : sigma1^2 = 2 * sigma2^2  vs H1 : sigma1^2 != 2 * sigma2^2
x1 <- rnorm(100, 0, sqrt(2))
x2 <- rnorm(100, 0, 1)
var.test(x1, x2, ratio=2, alternative="two.sided")

?var.test

s1 <- var(x1)
s2 <- var(x2)

s1/s2

length(y1); length(y2)

pf(s1/s2,99,99)






