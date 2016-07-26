nstores <- 20
nprods <- 50
nweeks <- 10
weeks <- 1:10

set.seed(105)

storetheta <- rep(0,nstores)
for (i in 1:length(storetheta)){
  while (storetheta[i] <= 0) storetheta[i] <- rcauchy(1, 0, 5)
}

prodtheta <- rep(0,nprods) 
for (i in 1:length(prodtheta)){
  while (prodtheta[i] <=0) prodtheta[i] <- rnorm(1,0,10)
}

hist(prodtheta)
hist(storetheta)

rawdat <- expand.grid(1:nweeks, 1:nstores, 1:nprods)
colnames(rawdat) <- c('week', 'store', 'prod')

rawdat$storetheta <- sapply(rawdat$store, function(x) storetheta[x])
rawdat$prodtheta <- sapply(rawdat$prod, function(x) prodtheta[x])

set.seed(105)
rawdat$shipped <- floor(rawdat$storetheta * rawdat$prodtheta* runif(nrow(rawdat), min=0.5, max=.9))
rawdat$rawdemand <- mapply(function(a,b) rpois(1,a * b), rawdat$storetheta, rawdat$prodtheta)
#rawdat$demand <- pmin(rawdat$shipped, rawdat$rawdemand)
rawdat$demand <- rawdat$rawdemand
table(rawdat$shipped==rawdat$demand)

library(rstan)
options(mc.cores = parallel::detectCores())

dat <- list(nstores = nstores,
            nprods = nprods,
            N = nrow(rawdat),
#            shipped = rawdat$shipped,
            ss = rawdat$store,
            pp = rawdat$prod,
            y = rawdat$demand)

try1 <- stan(file='synthetic0.stan', data=dat)


library(shinystan)
library(bayesplot)
library(ggplot2)
thetastore_est <- as.matrix(try1, pars='thetastore')
p <- mcmc_intervals(thetastore_est)
p + geom_point(aes(y=y, x=storetheta), data=data.frame(y=20:1, storetheta=storetheta), colour='blue', size=4)

thetaprod_est <- as.matrix(try1, pars='thetaprod')
p2 <- mcmc_intervals(thetaprod_est)
p2 + geom_point(aes(y=y, x=prodtheta), data=data.frame(y=50:1, storetheta=prodtheta), colour='blue', size=4) +
  scale_x_continuous(limits = c(0,5))
