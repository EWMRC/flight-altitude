data {
  int<lower=0> n_obs;
  int flips[n_obs];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper=1> p;
}

// transformed parameters {
//   real lq; 
//   lq = binomial_lpmf(flips| 20, p);
// }

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += binomial_lpmf(flips| 20, p);
  p ~ uniform(0, 1);
}

