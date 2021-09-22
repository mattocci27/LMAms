data{
  int<lower=0> N;
  vector<lower=0>[N] LMA;
  vector<lower=0>[N] A;
  vector<lower=0>[N] R;
  vector<lower=0>[N] LL;
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
  obs = append_col(append_col(log_A, log_LL), log_R);
}
parameters{
  real alpha_0;
  real alpha_p;
  real beta_0;
  real beta_s;
  real gamma_0;
  real gamma_p;
  real gamma_s;
  vector<lower=0, upper=1>[N] p;
  vector<lower=0>[3] L_sigma;
  cholesky_factor_corr[3] L_Omega;
}
transformed parameters{
  matrix[3,3] Z;
  matrix[N,3] Mu;
  matrix[N,3] X;
  matrix[N,3] L_Sigma;
  vector[N] log_LMAp;
  vector[N] log_LMAs;
  Z[1,1] = alpha_0;
  Z[1,2] = beta_0;
  Z[1,3] = gamma_0;
  Z[2,1] = alpha_p;
  Z[2,2] = 0;
  Z[2,3] = gamma_p;
  Z[3,1] = 0;
  Z[3,2] = beta_s;
  Z[3,3] = gamma_s;
  L_Sigma = rep_matrix(to_row_vector(0.5 * L_sigma .* L_sigma), N);
  log_LMAp = log(LMA) + log(p);
  log_LMAs = log(LMA) + log(1 - p);
  X = append_col(append_col(intercept, log_LMAp), log_LMAs);
  Mu = X * Z - L_Sigma;
}
 odel{
  // priors
  alpha_0 ~ normal(0, 10);
  alpha_p ~ normal(0, 10);
  beta_0 ~ normal(0, 10);
  beta_s ~ normal(0, 10);
  gamma_0 ~ normal(0, 10);
  gamma_p ~ normal(0, 10);
  gamma_s ~ normal(0, 10);
  //p ~ normal(0.5, 0.5);
  p ~ beta(1, 1);
  L_Omega ~ lkj_corr_cholesky(2); //uniform of L_Omega * L_Omega'
  L_sigma ~ cauchy(0, 5);

  // model
  for (i in 1:N)
     target += multi_normal_cholesky_lpdf(obs[i,] | Mu[i,], diag_pre_multiply(L_sigma, L_Omega));
}
generated quantities {
  vector[N] log_lik;
  real<lower=-1, upper=1> rho12;
  real<lower=-1, upper=1> rho23;
  real<lower=-1, upper=1> rho13;
  cov_matrix[3] Sigma;
  Sigma = diag_pre_multiply(L_sigma, L_Omega)
     * diag_post_multiply(L_Omega', L_sigma);
  rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
  rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
  rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
  for (i in 1:N)
   log_lik[i] = multi_normal_cholesky_lpdf(obs[i,] | Mu[i,], diag_pre_multiply(L_sigma, L_Omega));
 }
