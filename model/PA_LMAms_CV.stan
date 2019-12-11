data{
  int<lower=0> N;
  int<lower=0> J;
  vector<lower=0>[N] LMA;
  vector<lower=0>[N] A;
  vector<lower=0>[N] R;
  vector<lower=0>[N] LL;
  vector<lower=0>[N] leaf;
  int<lower=0,upper=1> holdout[N]; //CV
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
  vector[7] z;
  ordered[2] amas;
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
  Z[1,1] = z[1];
  Z[1,2] = z[2];
  Z[1,3] = z[3];
  Z[2,1] = amas[2];
  Z[2,2] = z[4];
  Z[2,3] = z[5];
  Z[3,1] = amas[1];
  Z[3,2] = z[6];
  Z[3,3] = z[7];
  L_Sigma = rep_matrix(to_row_vector(0.5 * L_sigma .* L_sigma), N);
  log_LMAp = log(LMA) + log(p);
  log_LMAs = log(LMA) + log(1 - p);
  X = append_col(append_col(intercept, log_LMAp), log_LMAs);
  Mu = X * Z - L_Sigma;
}
model{
  // priors
  z ~ normal(0, 10);
  amas ~ normal(0, 10);
  L_Omega ~ lkj_corr_cholesky(2); //uniform of L_Omega * L_Omega'
  L_sigma ~ cauchy(0, 5);

  // model
  for (i in 1:N) {
    if(holdout[i] == 0) {
      p[i] ~ beta(1, 1);
      target += multi_normal_cholesky_lpdf(obs[i,] | Mu[i,], diag_pre_multiply(L_sigma, L_Omega));
    }
  }
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
