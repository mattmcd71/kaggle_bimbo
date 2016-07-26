data{
  int<lower = 0> nstores; //number of stores
  int<lower = 0> nprods; //number of products
  int<lower = 0> N; //number of observations
  
  int<lower = 0> shipped[N]; //Number shipped for obs. n
  int<lower = 1, upper = nstores> ss[N]; //storenum for obs. n
  int<lower = 1, upper = nprods> pp[N]; //prodnum for obs. n
  
  int<lower = 0> y[N]; //demand for obs. n (max = number shipped)
}

parameters{
  real<lower=0> thetastore[nstores]; 
  real<lower=0> thetaprod[nprods];
}

model{
      
  thetastore ~ cauchy(0,5);
  thetaprod ~ normal(0,10);

  for (n in 1:N) {
    if (y[n] < shipped[n])
      y[n] ~ poisson(thetastore[ss[n]] * thetaprod[pp[n]]);
    else
      //print(shipped[n]);
      //print(poisson_lccdf((shipped[n] - 1) | thetastore[ss[n]] * thetaprod[pp[n]]));
      target += poisson_lccdf((shipped[n] - 1) | thetastore[ss[n]] * thetaprod[pp[n]]);
  }
}

