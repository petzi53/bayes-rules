
data {
  int<lower=1> n;             // number of observations
  array[n] int<lower=0> Y;    // observed successes
}

parameters {
  real<lower=0> lambda;       // success probability
}

model {
  // Prior
  lambda ~ gamma(4, 2);
  
  // Likelihood
  Y ~ poisson(lambda);
}

