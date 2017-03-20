#van bord voor 6.2
source("lepton.txt")
Bmu <- lepton$Bmu
B1 <- lepton$B1
Be <- lepton$Be
Bpi <- lepton$Bpi
Brho <- lepton$Brho

#opdracht d

alpha=0.25
B=1000
y1 <- numeric(B)
y2 <- numeric(B)
y3 <- numeric(B)
y4 <- numeric(B)
y5 <- numeric(B)
for(j in 1.:B){
  y1[j] <- mean(sample(Bmu, replace = TRUE),trim=alpha)
  y2[j] <- mean(sample(B1, replace = TRUE),trim=alpha)
  y3[j] <- mean(sample(Be, replace = TRUE),trim=alpha)
  y4[j] <- mean(sample(Bpi, replace = TRUE),trim=alpha)
  y5[j] <- mean(sample(Brho, replace = TRUE),trim=alpha)
}
D <- y2 - (y1+y3+y3+y5)
mean(D,trim=alpha)
#compute interval (copied from assignment 5.1), 16,90 is trimmed mean D
2 * 8.83 - quantile(D,c(0.975,0.025))


#opdracht e

alpha=0.0
B=1000
y1 <- numeric(B)
y2 <- numeric(B)
y3 <- numeric(B)
y4 <- numeric(B)
y5 <- numeric(B)
for(j in 1.:B){
  y1[j] <- mean(sample(Bmu, replace = TRUE),trim=alpha)
  y2[j] <- mean(sample(B1, replace = TRUE),trim=alpha)
  y3[j] <- mean(sample(Be, replace = TRUE),trim=alpha)
  y4[j] <- mean(sample(Bpi, replace = TRUE),trim=alpha)
  y5[j] <- mean(sample(Brho, replace = TRUE),trim=alpha)
}
D <- y2 - (y1+y3+y3+y5)
mean(D,trim=alpha)
#compute interval (copied from assignment 5.1), 16,90 is trimmed mean D
2 * 8.83 - quantile(D,c(0.975,0.025))

#klad

bootstrap <- function(x1,x2,x3,x4,x5, statistic, B)
{
  # returns a vector of B bootstrap values of real-valued statistic.
  # statistic(x) should be R-function ; arguments of 
  # statistic kan be inserted on ...
  # resampling is done from empirical distribution of x
  y1 <- numeric(B)
  y2 <- numeric(B)
  y3 <- numeric(B)
  y4 <- numeric(B)
  y5 <- numeric(B)
  for(j in 1.:B){
    y1[j] <- statistic(sample(x1, replace = TRUE))
    y2[j] <- statistic(sample(x2, replace = TRUE))
    y3[j] <- statistic(sample(x3, replace = TRUE))
    y4[j] <- statistic(sample(x4, replace = TRUE))
    y5[j] <- statistic(sample(x5, replace = TRUE))
  }
D <- D(y1,y2,y3,y4,y5)
}

D <- function(Bmu, B1, Be, Bpi, Brho){
  D = mean(B1) - (mean(Brho) + mean(Bpi) + mean(Be) + mean(Bmu))
}




#klad
D <- function(Bmu, B1, Be, Bpi, Brho){
  mean(B1) - (mean(Brho) + mean(Bpi) + mean(Be) + mean(Bmu))
}


#klad
D = B1 - (Brho + Bpi + Be + Bmu)
D = mean(B1) - (mean(Brho) + mean(Bpi) + mean(Be) + mean(Bmu))
D = mean(B1,trim=0.25) - (mean(Brho,trim=0.25) + mean(Bpi,trim=0.25) + mean(Be,trim=0.25) + mean(Bmu,trim=0.25))
D
D - 15.41
21.09 - D
bootstrap_D <- bootstrap
2 * T_n - quantile(bootstrap_D,c(0.975,0.025))
length(Bmu)
length(B1)
length(Be)
