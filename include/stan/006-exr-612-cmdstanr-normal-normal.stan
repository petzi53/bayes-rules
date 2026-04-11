
data {
  real Y;                    // observed data point
  real mu_0;                 // prior mean
  real<lower=0> sigma_0;     // prior SD
  real<lower=0> sigma;       // likelihood SD (known)
  int<lower=0> n;            // number of observations
}

parameters {
  real mu;
}

model {
  // Prior
  mu ~ normal(mu_0, sigma_0);
  
  // Likelihood
  Y ~ normal(mu, sigma);
}

generated quantities {
  // Posterior predictive distribution for new observation
  real y_pred = normal_rng(mu, sigma);
}

