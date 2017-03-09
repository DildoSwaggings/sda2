#Assignment 5
sample <- rexp(100,1)
v <- seq(0.01,1,0.01)
qqplot(qexp(v),sample)

