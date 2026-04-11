
data {
  int<lower=0> N;
  array[N] int<lower=0> Y;
}

parameters {
  real<lower=0> lambda;
}

model {
  // Prior: Gamma(4, 2) with shape=4, rate=2
  lambda ~ gamma(4, 2);

  // Likelihood: Poisson for each observation
  Y ~ poisson(lambda);
}

