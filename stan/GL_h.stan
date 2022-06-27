data{
  int<lower=0> N;
  vector<lower=0>[N] LMA;
  vector<lower=0>[N] A;
  vector<lower=0>[N] R;
  vector<lower=0>[N] LL;
  array[N] int<lower=0> DE;
}
transformed data{
  vector[N] log_A;
  vector[N] log_LL;
  vector[N] log_R;
  matrix[N,3] obs;
  vector[N] intercept;
  for (n in 1:N)
    intercept[n] = 1;
  log_A = log(A);
  log_LL = log(LL);
  log_R = log(R);
  // use net photosynthesis (A) instead of gross (A + R)
  obs = append_col(append_col(log_A, log_LL), log_R);
}

parameters{
  real a0;
  real ap;
  // real as;
  real b0;
  real bs;
  real g0;
  real gp;
  real gs;
  // real mu;
  // real<lower=0> tau;
  // vector[3] theta_tilde;
  vector[3] mu;
  vector<lower=0>[3] sig;
  vector<lower=0>[3] L_sigma;
  cholesky_factor_corr[3] L_Omega;
  vector<lower=0, upper=1>[N] p;
}

transformed parameters{
  // vector[3] theta;
  // for (j in 1:3)
  //   theta[j] = mu + tau * theta_tilde[j];

  matrix[N,3] Mu;
  matrix[3,3] Z;
  matrix[N,3] X;
  Z[1,1] = a0;
  Z[1,2] = b0;
  Z[1,3] = g0;
  Z[2,1] = ap;
  Z[2,2] = 0;
  Z[2,3] = gp;
  Z[3,1] = 0;
  Z[3,2] = bs;
  Z[3,3] = gs;

  //log_LMAp = log(LMA) + log(p);
  //log_LMAs = log(LMA) + log(1 - p);
  //X = append_col(append_col(append_col(intercept, log_LMAp), log_LMAs), leaf);
  X = append_col(append_col(intercept, log(LMA) + log(p)), log(LMA) + log(1 - p));
  Mu = X * Z;
}
model{
  vector[N] log_LMAp;
  log_LMAp = log(LMA) + log(p);
  // priors
  a0 ~ normal(0, 5);
  b0 ~ normal(0, 5);
  g0 ~ normal(0, 5);
  ap ~ normal(0, 5);
  bs ~ normal(0, 5);
  gp ~ normal(0, 5);
  gs ~ normal(0, 5);
  // as ~ normal(0, 5);
  L_Omega ~ lkj_corr_cholesky(2); //uniform of L_Omega * L_Omega'
  L_sigma ~ cauchy(0, 2.5);
  mu ~ normal(0, 5);
  // tau ~ cauchy(0, 5);
  // theta_tilde ~ normal(0, 1);
  sig ~ cauchy(0, 5);
  // model
  // for (i in 1:N) p[i] ~ beta_proportion(mu[DE[i]], kappa);
  for (i in 1:N) {
     target += normal_lpdf(log_LMAp[i] | mu[DE[i]], sig[DE[i]]);
     target += multi_normal_cholesky_lpdf(obs[i,] | Mu[i,], diag_pre_multiply(L_sigma, L_Omega));
  }
}
generated quantities {
  vector[N] log_lik;
  // eve vs dec
  real mu12;
  real<lower=-1, upper=1> rho12;
  real<lower=-1, upper=1> rho23;
  real<lower=-1, upper=1> rho13;
  cov_matrix[3] Sigma;
  mu12 = mu[2] - mu[1];
  Sigma = diag_pre_multiply(L_sigma, L_Omega)
     * diag_post_multiply(L_Omega', L_sigma);
  rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
  rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
  rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
  for (i in 1:N)
   log_lik[i] = multi_normal_cholesky_lpdf(obs[i,] | Mu[i,], diag_pre_multiply(L_sigma, L_Omega));
 }
