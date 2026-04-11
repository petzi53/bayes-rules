//
// This Stan program defines a Beta-Binomial model,
// as explained in chapter 6 of "Bayes Rules!"
// but with several improvements


data {
  int<lower=0> n;             // number of observations
  int<lower=0> n_trials;      // number of trials
  array[n] int<lower=0, upper=n_trials> Y;  // observed successes
}

parameters {
  real<lower=0, upper=1> pi;  // probability parameter
}

model {
  // Prior
  pi ~ beta(2, 2);

  // Likelihood
  Y ~ binomial(n_trials, pi);
}



