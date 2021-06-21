# https://www.rdocumentation.org/packages/DirichletReg/versions/0.6-3
library(DirichletReg) 
library(scales)
rm(list=ls())

# calculate P(max(x)=x1 | {x1,x2,x3,s})
pmax.3opt <- function(x, s, mu1, mu2, mu3=F, id) {
  if(mu3){
    if (id == 1){
      out <- pnorm(x, mu2, s) * pnorm(x, mu3, s) * dnorm(x, mu1, s)}
    if (id == 2){
      out <- pnorm(x, mu1, s) * pnorm(x, mu3, s) * dnorm(x, mu2, s)}
  }
  else{
    if (id == 1){
      out <- pnorm(x, mu2, s) * dnorm(x, mu1, s)}
    if (id == 2){
      out <- pnorm(x, mu1, s) * dnorm(x, mu2, s)}
  }
  return(out)
}

# 1 trial simulation
simulate_trial <- function(s,A1,A2,A3=F,a){
  
  ## generate internal responses
  x1 <- rnorm(1, A1, s)
  x2 <- rnorm(1, A2, s)
  if(A3){
    x3 <- rnorm(1, A3, s)
    x  <- cbind(x1,x2,x3)}
  else{
    x  <- cbind(x1,x2)}
  
  ## type 1 decision
  
  if(A3){
    post      <- rep(NA, 3)
    post[1]   <- integrate(pmax.3opt, -10, 10, s+0.15, x1, x2, x3, 1)$value
    post[2]   <- integrate(pmax.3opt, -10, 10, s+0.15, x1, x2, x3, 2)$value
    post[3]   <- 1 - post[1] - post[2]
  }
  else{
    post      <- rep(NA, 2)
    post[1]   <- integrate(pmax.3opt, -10, 10, s+0.15, x1, x2, id=1)$value
    post[2]   <- integrate(pmax.3opt, -10, 10, s+0.15, x1, x2, id=2)$value
  }
  
  # decision noise (p ---> q)
  q <- rdirichlet(n = 1, alpha = a*post)
  d <- which.max(q) # type 1 decision
  
  
  ## decision tipo 2
  o <- order(q, decreasing = T)
  if(A3){
    confidence <- (q[o[1]] - q[o[2]]) + (q[o[1]] - q[o[3]]) # pairwase model
  }
  else{
    confidence <- q[o[1]] - q[o[2]]} # equal to diff model
  
  return(c(d,confidence))
  
}

##### parameters ####
a1 <- 1
a2 <- c(0.7, 0.8, 0.9, 0.93, 0.95)
a3 <- 0.3
s  <- 0.1    # sensory noise
alpha <- 15  # decision noise

##### model simulation #####
n <- 10000 # number of trials to simulate 
two_alt_c <- matrix(NA, nrow = n, ncol = 5) # confidence matrices
three_alt_c <- matrix(NA, nrow = n, ncol = 5)
two_alt_d <- matrix(NA, nrow = n, ncol = 5) # decision matrices
three_alt_d <- matrix(NA, nrow = n, ncol = 5)

for(i in 1:n){ # n simulations for two alternatives trials 
  for(j in seq_along(a2)){
    temp <- simulate_trial(s, a1, a2[j], a = alpha) 
    two_alt_d[i,j] <- temp[1] # decision
    two_alt_c[i,j] <- temp[2] # confidence
  }
}

for(i in 1:n){ # 10000 simulations for three alternatives trials 
  for(j in seq_along(a2)){
    temp <- simulate_trial(s, a1, a2[j], a3, a = alpha) 
    three_alt_d[i,j] <- temp[1] # decision
    three_alt_c[i,j] <- temp[2] # confidence
  }
}
two_alt_c <- rescale(two_alt_c, to = 0:1) # rescale simulations to 0:1
three_alt_c <- rescale(three_alt_c, to = 0:1)

#### plot modelling results #####
df <- data.frame(mean_conf2 = c(mean(two_alt_c[,1]), mean(two_alt_c[,2]), mean(two_alt_c[,3]), 
                                mean(two_alt_c[,4]), mean(two_alt_c[,5])),
                 mean_conf3 = c(mean(three_alt_c[,1]), mean(three_alt_c[,2]), mean(three_alt_c[,3]), 
                                mean(three_alt_c[,4]), mean(three_alt_c[,5])))

par(bty='l', pty='s')
plot(a2, df$mean_conf2, type = 'l', col = rgb(0,0.6,0), ylim = c(0,1), lwd = 3, 
     xlab = 'Task difficulty (2 alt proportion size)', ylab = 'Confidence', main = 'contrast model')
lines(a2, df$mean_conf3, type = 'l', col = 'orange', lwd = 3)
#legend(x=0.87, y= 0.95, legend = c('2 alternatives', '3 alternatives'), col = c('green', 'orange'),lty = 1, lwd = 3)


##### new parameters #####
a1 <- 1
a2 <- 0.93
a3 <- c(0.03, 0.3, 0.6, 0.8, 0.9)

# model simulation
two_alt_c <- matrix(NA, nrow = n, ncol = 5) # confidence matrices
three_alt_c <- matrix(NA, nrow = n, ncol = 5)
two_alt_d <- matrix(NA, nrow = n, ncol = 5) # decision matrices
three_alt_d <- matrix(NA, nrow = n, ncol = 5)

for(i in 1:n){ # n simulations for two alternatives trials 
  for(j in seq_along(a3)){
    temp <- simulate_trial(s, a1, a2, a = alpha) 
    two_alt_d[i,j] <- temp[1] # decision
    two_alt_c[i,j] <- temp[2] # confidence
  }
}

for(i in 1:n){ # 10000 simulations for three alternatives trials 
  for(j in seq_along(a3)){
    temp <- simulate_trial(s, a1, a2, a3[j], a = alpha) 
    three_alt_d[i,j] <- temp[1] # decision
    three_alt_c[i,j] <- temp[2] # confidence
  }
}
two_alt_c <- rescale(two_alt_c, to = 0:1) # rescale simulations to 0:1
three_alt_c <- rescale(three_alt_c, to = 0:1)

# plot
df <- data.frame(mean_conf2 = c(mean(two_alt_c[,1]), mean(two_alt_c[,2]), mean(two_alt_c[,3]), 
                                mean(two_alt_c[,4]), mean(two_alt_c[,5])),
                 mean_conf3 = c(mean(three_alt_c[,1]), mean(three_alt_c[,2]), mean(three_alt_c[,3]), 
                                mean(three_alt_c[,4]), mean(three_alt_c[,5])))

plot(a3, df$mean_conf3, type = 'l', col = 'orange', ylim = c(0,1), lwd = 3, 
     xlab = 'A3 proportion size', ylab = 'Confidence', main = 'contrast model')
points(0.02, df$mean_conf2[1], pch=12, col = rgb(0,0.6,0), lwd = 7)