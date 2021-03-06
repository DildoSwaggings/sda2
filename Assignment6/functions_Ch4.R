

bootstrap <- function(x, statistic, B = 100., ...)
{
  # returns a vector of B bootstrap values of real-valued statistic.
  # statistic(x) should be R-function ; arguments of 
  # statistic kan be inserted on ...
  # resampling is done from empirical distribution of x
  y <- numeric(B)
  for(j in 1.:B)
    y[j] <- statistic(sample(x, replace = TRUE), ...)
  y
}
