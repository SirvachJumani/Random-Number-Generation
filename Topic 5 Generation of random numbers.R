################################################################################################################
################################################################################################################

# Our Topic was Topic 5 "Generation of Random Numbers"
# Gruoup members: Carmina Tió, Sirvach Jumani, Soheil Shams, Zhuocheng Xie


# To demonstrate the topic we looked for different methods, which are used to generate random numbers



### Introduction
### Linear Congruential Generator with good and bad parameters

library(plot3D)
library(animation)
par(mfrow=c(1,1))


# Linear Congruential Generator with good parameters

lcg.rand <- function(n) {
  
  rng <- vector(length = n)
  
  m <- 2 ** 32
  a <- 1103515245
  c <- 12345
  d <- as.numeric(Sys.time()) * 1000
  
  for (i in 1:n) {
    d <- (a * d + c) %% m
    rng[i] <- d / m
  }
  
  return(rng)
}


n <- c(100)

for (i in 1:length(n)) {
  x <- lcg.rand(n[i])
  y <- lcg.rand(n[i])
  z <- lcg.rand(n[i])
  
  scatter3D(x, y, z, col= "dodgerblue3", cex.main=3,pch=20, cex = 0.5, theta=20, main = paste('n = ', n[i]))
}


n <- c(1000)

for (i in 1:length(n)) {
  x <- lcg.rand(n[i])
  y <- lcg.rand(n[i])
  z <- lcg.rand(n[i])
  
  scatter3D(x, y, z, col= "dodgerblue3", cex.main=3,pch=20, cex = 0.5, theta=20, main = paste('n = ', n[i]))
}

n <- c(10000)

for (i in 1:length(n)) {
  x <- lcg.rand(n[i])
  y <- lcg.rand(n[i])
  z <- lcg.rand(n[i])
  
  scatter3D(x, y, z, col= "dodgerblue3", cex.main=3,pch=20, cex = 0.5, theta=20, main = paste('n = ', n[i]))
}


# Linear Congruential Generator with bad parameters

lcg.poor <- function(n) {
  
  rng <- vector(length = n)
  
  m <- 2048
  a <- 1229
  c <- 1
  
  d <- as.numeric(Sys.time()) * 1000
  
  for (i in 1:n) {
    d <- (a * d + c) %% m
    rng[i] <- d / m
  }
  
  return(rng)
}


n <- c(100)

for (i in 1:length(n)) {
  x <- lcg.poor(n[i])
  y <- lcg.poor(n[i])
  z <- lcg.poor(n[i])
  
  scatter3D(x, y, z, col= "dodgerblue3", cex.main=3,pch=20, cex = 0.5, theta=20, main = paste('n = ', n[i]))
}


n <- c(1000)

for (i in 1:length(n)) {
  x <- lcg.poor(n[i])
  y <- lcg.poor(n[i])
  z <- lcg.poor(n[i])
  
  scatter3D(x, y, z, col= "dodgerblue3", cex.main=3,pch=20, cex = 0.5, theta=20, main = paste('n = ', n[i]))
}


n <- c(10000)

for (i in 1:length(n)) {
  x <- lcg.poor(n[i])
  y <- lcg.poor(n[i])
  z <- lcg.poor(n[i])
  
  scatter3D(x, y, z, col= "dodgerblue3", cex.main=3,pch=20, cex = 0.5, theta=20, main = paste('n = ', n[i]))
}

################################################################################################################
################################################################################################################


### Methods 


normalized <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

################################################################################################################

### Fibonacci Generator

fibonacci_Method <- function(n,m,num1,num2) {
  
  rng <- vector(length = n)
  
  for (i in 1:n) {
    num3 = (num1 + num2) %% m
    num1 = num2
    num2 = num3
    rng[i] <- num3
  }
  
  return(rng)
}

FBG <- matrix(normalized(fibonacci_Method(n=20000000,m=2^32,num1=12,num2=6)),ncol=2000,nrow=10000)


################################################################################################################

### Inverse Congruential Generator

Inverse_Congruential_Generator <- function(n,m,c,a,x) {
  
  rng <- vector(length = n)
  
  for (i in 1:n) {
    x <- (a * (1/x) + c) %% m
    
    rng[i] <- x
  }
  
  return(rng)
}

ICG <- matrix(normalized(Inverse_Congruential_Generator(20000000,3,14254,65231,421367)),ncol=2000,nrow=10000)


################################################################################################################

### Multiply-with-carry Generator

Multiply_with_carry_Generator <- function(n, a, x,c,m) {
  
  rng <- vector(length = n)
  
  for (i in 1:n) {
    
    x <- (a * x + c ) %% m
    
    c <- (a * x + c ) / m

    rng[i] <- x
  }
  
  return(rng)
}

MwcG <- matrix(normalized(Multiply_with_carry_Generator(20000000, 124785, 14556,1,2^32)),ncol=2000,nrow=10000)


################################################################################################################

### Combinded Generator (by using k-multiplicative Congruential Generator)

k_multiplicative_congruential_Generator <- function(n,x,y,z,u) {
  
  rng <- vector(length = n)
  
  for (i in 1:n) {
    
    x = 40014 * x %% 2147483563
    y = 40692 * y %% 2147483399
    z = (x-y) %% 2147483563
    
    u = 4.656613 * z * 10^-10
    
    rng[i] <- u
  }
  
  return((rng))
}

k_MCG <- matrix(k_multiplicative_congruential_Generator(n=20000000,x=12345,y=67890),ncol=2000,nrow=10000)


################################################################################################################


### Plotting of the Methods

par(mfrow=c(2,2), mai = c(0.5,0.4,0.9,0.4))######

plot((FBG[,1]), col = c("deepskyblue","dodgerblue3"), cex.main = 2,each = 1, pch = 20, cex = 0.1 , main='Fibonacci Generator',xlab='',ylab='')

plot((ICG[,1]), col = c("deepskyblue","dodgerblue3"), cex.main = 2,each = 1, pch = 20, cex = 0.1 , main='  Inversive \n Congruential Generator',xlab='',ylab='')

plot((MwcG[,1]), col = c("deepskyblue","dodgerblue3"), cex.main = 2,each = 1, pch = 20, cex = 0.1 , main='  Multiply with \n carry Generator',xlab='',ylab='')

plot((k_MCG[,1]), col = c("deepskyblue","dodgerblue3"), cex.main = 2,each = 1, pch = 20, cex = 0.1 , main='Combined Generator',xlab='',ylab='')

################################################################################################################


### Histograms

# Fibonacci Generator

intr.20_FBG <- seq(min(FBG[,1]), max(FBG[,1]), (max(FBG[,1])-min(FBG[,1]))/20)

histogram_FBG <- hist((FBG[,1]), breaks=c(intr.20_FBG), xlab='Random Number Intervals', main='20,000 Random Numbers: Fibonacci Generator',col = c("dodgerblue3"))


# Inverse Congruential Generator

intr.20_ICG <- seq(min(ICG[,1]), max(ICG[,1]), (max(ICG[,1])-min(ICG[,1]))/20)

histogram_ICG <- hist((ICG[,1]), breaks=c(intr.20_ICG), xlab='Random Number Intervals', main='20,000 Random Numbers with: Inverse Congruential Generator',col = c("dodgerblue3"))


# Multiply-with-carry Generator

intr.20_MwcG <- seq(min(MwcG[,1]), max(MwcG[,1]), (max(MwcG[,1])-min(MwcG[,1]))/20)

histogram_MwcG <- hist((MwcG[,1]), breaks=c(intr.20_MwcG), xlab='Random Number Intervals', main='20,000 Random Numbers: Multiply-with-carry Generator',col = c("dodgerblue3"))

# Combinded Generator

intr.20_k_MCG <- seq(min(k_MCG[,1]), max(k_MCG[,1]), (max(k_MCG[,1])-min(k_MCG[,1]))/20)

histogram_k_MCG <- hist((k_MCG[,1]), breaks=c(intr.20_k_MCG), xlab='Random Number Intervals', main='20,000 Random Numbers: k-multiplicative congruential generator',col = c("dodgerblue3"))


################################################################################################################

### Time

system.time(matrix(normalized(fibonacci_Method(n=20000000,m=2^32,num1=12,num2=6)),ncol=2000,nrow=10000))

system.time(matrix(normalized(Inverse_Congruential_Generator(20000000,3,14254,65231,421367)),ncol=2000,nrow=10000))

system.time(matrix(normalized(Multiply_with_carry_Generator(20000000, 124785, 14556,1,2^32)),ncol=2000,nrow=10000))

system.time(matrix(k_multiplicative_congruential_Generator(n=20000000,x=12345,y=67890),ncol=2000,nrow=10000))

################################################################################################################


### Kolmogorov Smirnov Test

ks.test((FBG[,1]),"punif",0,1) 

ks.test((ICG[,1]),"punif",0,1) 

ks.test((MwcG[,1]),"punif",0,1) 

ks.test((k_MCG[,1]),"punif",0,1)

################################################################################################################

### Correlation

# Fibonacci Generator

cor_FBG <- function(n){
  vec <- vector(length = n)
  
  for(i in 1:n){
    
    
    cor <- cor(FBG[,i],FBG[,i+1])
    
    if(i==n-1){
      break
    }
    
    vec[i] <- cor
  }
  return(vec)
}

correlation_FBG <- cor_FBG(n=2000)
mean(correlation_FBG)


# Inversive COngruential Generator

cor_ICG <- function(n){
  vec <- vector(length = n)
  
  for(i in 1:n){
    
    
    cor <- cor(ICG[,i],ICG[,i+1])
    
    if(i==n-1){
      break
    }
    
    vec[i] <- cor
  }
  return(vec)
}

correlation_ICG <- cor_ICG(n=2000)
mean(correlation_ICG)


# Multiply with carry Generator

cor_MwcG <- function(n){
  vec <- vector(length = n)
  
  for(i in 1:n){
    
    
    cor <- cor(MwcG[,i],MwcG[,i+1])
    
    if(i == n-1 ){
      break
    }
    
    vec[i] <- cor
  }
  return(vec)
}

correlation_MwcG <- cor_MwcG(n=2000)
mean(correlation_MwcG)


# Combined Generator

cor_k_MCG <- function(n){
  vec <- vector(length = n)
  
  for(i in 1:n){
    
    
    cor <- cor(k_MCG[,i],k_MCG[,i+1])
    
    if(i == n-1 ){
      break
    }
    
    vec[i] <- cor
  }
  return(vec)
}

correlation_k_MCG <- cor_k_MCG(n=2000)
mean(correlation_k_MCG)



