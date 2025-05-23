// 2PL IRT MODEL
data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of MEPs
  int<lower=1> I;                     // Number of indicators (votes)
  array[N] int<lower=1, upper=J> jj;  // MEP ID for observation n
  array[N] int<lower=1, upper=I> ii;  // Vote ID for observation n
  array[N] int<lower=0, upper=1> y;   // Value (=position) of mep for vote n
}

parameters {
  vector[I] alpha;            // difficulty parameter
  vector<lower=0>[I] beta;    // discrimination parameter
  vector[J] eta;              // latent trait/ability parameter
  real mu_alpha;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
}

model {
  // priors
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta ~ lognormal(0, sigma_beta);
  eta ~ normal(0, 1);
  sigma_alpha ~ student_t(3, 0, 1);
  sigma_beta ~ student_t(3, 0, 1);
  mu_alpha ~ student_t(3, 0, 1);
  
  // likelihood
  y ~ bernoulli_logit(beta[ii] .* (eta[jj] - alpha[ii]));
}

