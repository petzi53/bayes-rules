//
// This Stan program defines a Beta-Binomial model,
// as explained in chapter 6 of "Bayes Rules!"


data {
  int<lower = 0, upper = 10> Y;
}
parameters {
  real<lower = 0, upper = 1> pi;
}
model {
  Y ~ binomial(10, pi);
  pi ~ beta(2, 2);
}


