#exercise2, asssignment5

sample_uniform <- runif(50)
sample_uniform
hist(sample_uniform)
#define estimator T_n using function
T_n <- function(sample)
{
      n = length(sample)
      T_n = ((n+1)/n)*max(sample)
      T_n
}
#use function for generated sample
T_n(sample_uniform)
#using emperical bootstrap to estimate variance of T_n, take B=1000
emperical_bootstrap_uniform_sample <- bootstrap(sample_uniform,T_n,1000)
#plot bootstrap in histogram and compute variance, sd
hist(emperical_bootstrap_uniform_sample,main=expression(paste("Hist. of estimates for ", T[n]," from emp. bootstrap")),xlab=expression(paste(T[n])))
var(emperical_bootstrap_uniform_sample)
sd(emperical_bootstrap_uniform_sample)
#
#B
par(mfrow=c(4,2))
var_different_emp_bootstraps = numeric(10)
for (i in 1:10)
{
  sample_uniform <- runif(50)
  emperical_bootstrap_uniform_sample <- bootstrap(sample_uniform,T_n,1000)
  hist(emperical_bootstrap_uniform_sample, main="Hist. of emp. bootstrap from uni(50,0,1)",xlab=expression(paste("Estimate for ",T[n])))
  var_different_emp_bootstraps[i] = var(emperical_bootstrap_uniform_sample)
}
hist(var_different_emp_bootstraps,main=expression(paste("Hist. of variance of ", T[n], " from 10 emp. bootstraps")),xlab=expression(paste(T[n])))
#
#
#C
#using parametrical bootstrap
B=1000
repetitions = 8
var_different_par_bootstraps = numeric(repetitions)
par(mfrow=c(4,2))
for (j in 1:repetitions)
{
  Tstar = numeric(B)
  for(i in 1:B){
      xstar=runif(50)
      Tstar[i]=T_n(xstar)
  }
  hist(Tstar, main="Hist. of par. bootstrap from uni(50,0,1)",xlab=expression(paste("Estimate for ",T[n])))
  var_different_par_bootstraps[j] = var(Tstar)
}
par(mfrow=c(1,1))
hist(var_different_par_bootstraps,main=expression(paste("Hist. of variance of ", T[n], " from 8 par. bootstraps")),xlab=expression(paste(T[n])))
