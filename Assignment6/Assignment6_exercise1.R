#van bord
source("light.txt")
x1 <- light[[1]]
x2 <- light[[2]]
#
par(mfrow=c(2,2))
hist(x1,main="Histogram of first measurements",xlab="First measurements")
qqnorm(x1)
hist(x2,main="Histogram of second measurements",xlab="Second measurements")
qqnorm(x2)
mean(x1)
var(x1)
mean(x2)
var(x2)

#voor x1
t=ks.test(x1,pnorm,mean(x1),sd(x1))$statistic

B<-1000
Tstar<-numeric(B)
for (i in 1:B){
  xstar=rnorm(length(x1),mean(x1),sd(x1))
  Tstar[i]=ks.test(xstar,pnorm,mean(xstar),sd(xstar))$statistic
}
T_n <- 0
for (j in 1:length(Tstar)){
  if (Tstar[j] >= t)
    T_n = T_n + 1
  fraction <- T_n / length(Tstar)
}
fraction
# fraction is P_H_0(T_n > t) =< alpha
#
#voor x2
t=ks.test(x2,pnorm,mean(x2),sd(x2))$statistic
B<-1000
Tstar<-numeric(B)
for (i in 1:B){
  xstar=rnorm(length(x2),mean(x2),sd(x2))
  Tstar[i]=ks.test(xstar,pnorm,mean(xstar),sd(xstar))$statistic
}
T_n <- 0
for (j in 1:length(Tstar)){
  if (Tstar[j] >= t)
    T_n = T_n + 1
  fraction <- T_n / length(Tstar)
}
fraction
#van bord voor 6.2
source("lepton.txt")
lepton$B1
lepton$mu
