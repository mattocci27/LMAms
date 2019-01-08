data{
  int<lower=0> N;
  int<lower=0> J;
  vector<lower=0>[N] LMA;
  vector<lower=0>[N] A;
  vector<lower=0>[N] R;
  vector<lower=0>[N] LL;
  int<lower=0> gr[N];
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
  matrix[3,3] Z;
  vector<lower=0>[3] L_sigma;
  cholesky_factor_corr[3] L_Omega;
  vector[N] z;
  vector<lower=0>[3] theta_tilde;
  real<lower=0> tau;
  real<lower=0> tau2;
  real theta;
}
transformed parameters{
  matrix[N,3] Mu;
  matrix[N,3] X;
  matrix[N,3] L_Sigma;
  vector[N] log_LMAp;
  vector[N] log_LMAs;
  vector[3] mu_z;
  vector<lower=0, upper=1>[N] p;
  p <- 1 ./ (1 + exp(-z));
  L_Sigma = rep_matrix(to_row_vector(0.5 * L_sigma .* L_sigma), N);
  log_LMAp = log(LMA) + log(p);
  log_LMAs = log(LMA) + log(1 - p);
  X = append_col(append_col(intercept, log_LMAp), log_LMAs);
  Mu = X * Z - L_Sigma;
  mu_z = theta + tau * theta_tilde;
}
model{
  // priors
  to_vector(Z) ~ normal(0, 10);
  theta_tilde ~ normal(0, 1);
  theta ~ normal(0, 1);
  tau ~ cauchy(0, 5);
  z ~ normal(mu_z[gr], tau2);
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
