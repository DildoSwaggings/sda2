setwd("H:/Documents/StatisticalDataAnalysis/github_sda/Assignment5")
data1 <- source("thromboglobulin.txt")
attach(thromboglobulin)
#van bord:
#T_n = (n+1)/n X_(n)
#var(T_n) = var(..) = ((N=1)/(n))^2 var(x_(n))
#P(X_(n)<x)=P(max(x_1,..,x_n)<x)=product_1_tot_n P(x_i<x)=x^n
#f(x)=nx^(n-1) => Ex, Ex^2 => var(X_(n))=Ex^2-(Ex)^2 (pijlte naar regel 6)

par(mfrow=c(2,2))
hist(SDRP)
symplot(SDRP)
boxplot(SDRP)
qqnorm(SDRP)
qqexp(SDRP)
n <- length(SDRP)
T_n = mean(SDRP)
bootstrap_SDRP <- bootstrap(SDRP,mean,1000)

#MET HULP WERKCOLLEGE DOCENT:
2 * T_n - quantile(bootstrap_SDRP,c(0.975,0.025))
#upper_boundary <- 2 * T_n - quantile(bootstrap_SDRP,0.025)
#
#B
T_n = median(SDRP)
bootstrap_SDRP <- bootstrap(SDRP,median,1000)
2 * T_n - quantile(bootstrap_SDRP,c(0.975,0.025))
#
#D
T_n <- median(SDRP) - median(CTRP)
bootstrap_SDPR <- bootstrap(SDRP,median,1000)
bootstrap_CTRP <- bootstrap(CTRP,median,1000)
z_star <- median(bootstrap_SDPR) - median(CTRP) - T_n
z_star

