#1. b
#Bull Call Spread:
  S_T <- seq(0, 200, by = 1)
  S_0 <- 100
  K1 <- 100
  K2 <- 120
  PayOff <- numeric(length(S_T))
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K2-K1
    } else if (S_T[i] > K1 && S_T[i] < K2) {
      PayOff[i] <- K2 - S_T[i]
    }
    else {
      PayOff[i] <- 0
    }
  }
  
  do_plot <- function(main, color) {
    plot(S_T, PayOff, type = "l", lwd = 2, col = color,
        xlim = c(0,200),
        ylim = c(-60,60),
        xlab = expression(S[T]), 
        ylab = "Profit / Loss",
        main = main)
    abline(h = 0, col = "gray", lty = 2)
  }
  do_plot("Bull Call Spread","darkgreen")
  
#Bear Put Spread:
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- 0
    } else if (S_T[i] > K1 && S_T[i] < K2) {
      PayOff[i] <- K1 - S_T[i]
    }
    else {
      PayOff[i] <- K1-K2
    }
  }
  do_plot("Bear Put Spread","red")

#Covered Call: 
  K <- 110
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K) {
      PayOff[i] <- S_T[i]-S_0
    } else 
      PayOff[i] <- K - S_0
  }
  do_plot("Covered Call","blue")
  
#Covered Put:
  K <- 90
  for (i in 1:length(S_T)) {
    if (S_T[i] < K) {
      PayOff[i] <- K - S_0
    } else 
      PayOff[i] <- -(S_T[i]-S_0)
  }
    do_plot("Covered Put","black")
#Collar
  K1 <- 90
  K2 <- 110
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K1 - S_0
    } else if(S_T[i] > K1 && S_T[i] < K2){
      PayOff[i] <- S_T[i]-S_0
    } else if (S_T[i] >= K2)
      PayOff[i]  <- K2 - S_0
  }
    do_plot("Collar","darkgreen")

#Butterfly
    K1 <- 90
    K2 <- 100
    K3 <- 110
    for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K1 - S_0
    } else if(S_T[i] > K1 && S_T[i] <= K2){
      PayOff[i] <- S_T[i]-K1
    } else if (S_T[i] > K2 && S_T[i] <= K3){
      PayOff[i]<-K3-S_T[i]
    } else
      PayOff[i]  <- 0
    }
    do_plot("Butterfly","darkred")
    
# Condor
    K1 <- 90
    K2 <- 100
    K3 <- 110
    K4 <- 120
    
    for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K1 - S_0
    } else if(S_T[i] > K1 && S_T[i] <= K2){
      PayOff[i] <- S_T[i]-K1
    } else if (S_T[i] > K2 && S_T[i] <= K3){
      PayOff[i]<-K2-K1
    } else if (K3 < S_T[i] && S_T[i] <= K4){
      K4 - S_T[i]
    }
      else
      PayOff[i]  <- 0
    }
    do_plot("Condor","darkblue")

#1. d
#Bull Call Spread:
  premium <- 5
  S_T <- seq(0, 200, by = 1)
  S_0 <- 100
  K1 <- 100
  K2 <- 120
  PayOff <- numeric(length(S_T))
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K2-K1
    } else if (S_T[i] > K1 && S_T[i] < K2) {
      PayOff[i] <- K2 - S_T[i]
    }
    else {
      PayOff[i] <- 0
    }
  }
  
  do_plot <- function(main, color) {
    profit <- PayOff-premium
    plot(S_T, profit, type = "l", lwd = 2, col = color,
        xlim = c(0,200),
        ylim = c(-60,60),
        xlab = expression(S[T]), 
        ylab = "Profit / Loss",
        main = main)
    abline(h = 0, col = "gray", lty = 2)
  }
  do_plot("Bull Call Spread","darkgreen")
  
#Bear Put Spread:
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- 0
    } else if (S_T[i] > K1 && S_T[i] < K2) {
      PayOff[i] <- K1 - S_T[i]
    }
    else {
      PayOff[i] <- K1-K2
    }
  }
  do_plot("Bear Put Spread","red")

#Covered Call: 
  K <- 110
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K) {
      PayOff[i] <- S_T[i]-S_0
    } else 
      PayOff[i] <- K - S_0
  }
  do_plot("Covered Call","blue")
  
#Covered Put:
  K <- 90
  for (i in 1:length(S_T)) {
    if (S_T[i] < K) {
      PayOff[i] <- K - S_0
    } else 
      PayOff[i] <- -(S_T[i]-S_0)
  }
    do_plot("Covered Put","black")
#Collar
  K1 <- 90
  K2 <- 110
  for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K1 - S_0
    } else if(S_T[i] > K1 && S_T[i] < K2){
      PayOff[i] <- S_T[i]-S_0
    } else if (S_T[i] >= K2)
      PayOff[i]  <- K2 - S_0
  }
    do_plot("Collar","darkgreen")

#Butterfly
    K1 <- 90
    K2 <- 100
    K3 <- 110
    for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K1 - S_0
    } else if(S_T[i] > K1 && S_T[i] <= K2){
      PayOff[i] <- S_T[i]-K1
    } else if (S_T[i] > K2 && S_T[i] <= K3){
      PayOff[i]<-K3-S_T[i]
    } else
      PayOff[i]  <- 0
    }
    do_plot("Butterfly","darkred")
    
# Condor
    K1 <- 90
    K2 <- 100
    K3 <- 110
    K4 <- 120
    
    for (i in 1:length(S_T)) {
    if (S_T[i] <= K1) {
      PayOff[i] <- K1 - S_0
    } else if(S_T[i] > K1 && S_T[i] <= K2){
      PayOff[i] <- S_T[i]-K1
    } else if (S_T[i] > K2 && S_T[i] <= K3){
      PayOff[i]<-K2-K1
    } else if (K3 < S_T[i] && S_T[i] <= K4){
      K4 - S_T[i]
    }
      else
      PayOff[i]  <- 0
    }
    do_plot("Condor","darkblue")

#2
  C <- 3
  P <- 2
  K <- 58
  S0 <- 60
  t <- 0
  r <- 0.10
  T <- 1
  
  K_disc <- K * exp(-r * (T-t))
  right_side <- C + K_disc
  left_side <- P + S0
  diff<-abs(right_side-left_side)
  diff<-round(diff,2)
  print(list(left_side = left_side, right_side = right_side, diff=diff))

# NA
