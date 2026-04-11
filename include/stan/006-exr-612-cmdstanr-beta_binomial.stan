
data {
  int<lower=1> n;                              // number of observations
  int<lower=0> n_trials;                       // number of trials per obs
  array[n] int<lower=0, upper=n_trials> Y;     // observed successes
}

parameters {
  real<lower=0, upper=1> pi;  // success probability
}

model {
  // Prior
  pi ~ beta(1, 1);
  
  // Likelihood
  Y ~ binomial(n_trials, pi);
}

