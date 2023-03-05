rm(list=ls())

set.seed(1)

nsim = 1000
err_MLE = rep(0,nsim) # SEL for MLE
err_JSE = rep(0,nsim) # SEL for James-Stein


# -------------------------------------------------------------------------


# p = N : dimension is equal to the sample size
N = 100
mu = runif(N) # generating mu_i (i=1(1)100) from U(0,1)
for (i in 1:nsim)
{
  z = rnorm(N,mu,1)
  mu_MLE = z
  mu_JSE = (1-(N-2)/sum(z^2))*z
  # Squared-Error Loss (SEL)
  err_MLE[i] = sum((mu_MLE-mu)^2)/N
  err_JSE[i] = sum((mu_JSE-mu)^2)/N
}
err1 = as.data.frame(cbind(err_MLE,err_JSE))
names(err1) = c("err_MLE","err_JSE")


# -------------------------------------------------------------------------


N = 100
mu = rnorm(N) # generating mu_i (i=1(1)100) from N(0,1)
for (i in 1:nsim)
{
  z = rnorm(N,mu,1)
  mu_MLE = z
  mu_JSE = (1-(N-2)/sum(z^2))*z
  # Squared-Error Loss (SEL)
  err_MLE[i] = sum((mu_MLE-mu)^2)/N
  err_JSE[i] = sum((mu_JSE-mu)^2)/N
}
err2 = as.data.frame(cbind(err_MLE,err_JSE))
names(err2) = c("err_MLE","err_JSE")


# -------------------------------------------------------------------------

N = 100
mu = rexp(N) # generating mu_i (i=1(1)100) from Exp(1)
for (i in 1:nsim)
{
  z = rnorm(N,mu,1)
  mu_MLE = z
  mu_JSE = (1-(N-2)/sum(z^2))*z
  # Squared-Error Loss (SEL)
  err_MLE[i] = sum((mu_MLE-mu)^2)/N
  err_JSE[i] = sum((mu_JSE-mu)^2)/N
}
err3 = as.data.frame(cbind(err_MLE,err_JSE))
names(err3) = c("err_MLE","err_JSE")


# -------------------------------------------------------------------------

N = 100
mu = 1 # generating mu_i = 1 (for all; i=1(1)100)
for (i in 1:nsim)
{
  z = rnorm(N,mu,1)
  mu_MLE = z
  mu_JSE = (1-(N-2)/sum(z^2))*z
  # Squared-Error Loss (SEL)
  err_MLE[i] = sum((mu_MLE-mu)^2)/N
  err_JSE[i] = sum((mu_JSE-mu)^2)/N
}
err4 = as.data.frame(cbind(err_MLE,err_JSE))
names(err4) = c("err_MLE","err_JSE")

par(mfrow=c(2,2))
boxplot(err1,ylab="SEL",main=bquote(mu[i]~" ~ U(0,1)"))
boxplot(err2,ylab="SEL",main=bquote(mu[i]~" ~ N(0,1)"))
boxplot(err3,ylab="SEL",main=bquote(mu[i]~" ~ Exp(1)"))
boxplot(err4,ylab="SEL",main=bquote(mu[i]~" = 1"))
mtext(bquote("SEL for MLE/JS, estimating"~mu),side = 3,line = - 2,outer = TRUE)