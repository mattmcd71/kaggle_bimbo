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
rawdat$demand <- pmin(rawdat$shipped, rawdat$rawdemand)
table(rawdat$shipped==rawdat$demand)

library(rstan)
options(mc.cores = parallel::detectCores())

dat <- list(nstores = nstores,
            nprods = nprods,
            N = nrow(rawdat),
            shipped = rawdat$shipped,
            ss = rawdat$store,
            pp = rawdat$prod,
            y = rawdat$demand)

try1 <- stan(file='synthetic.stan', data=dat, chains=1)

#this doesn't work.  I think it's because there are some large counts, and when the compiler tries
#to calculate the log probability with the starting point of the estimation
#, it's getting numbers that are close to negative infinity