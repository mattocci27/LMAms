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
  matrix[N,2] X;
  vector[N] intercept;
  for (n in 1:N)
    intercept[n] = 1;
  log_A = log(A);
  log_LL = log(LL);
  log_R = log(R);
  obs = append_col(append_col(log_A, log_LL), log_R);
  X = append_col(intercept, log(LMA));
}

parameters{
  real log_alpha;
  real log_beta;
  real rp;
  real rs;
  vector<lower=0>[3] L_sigma;
  cholesky_factor_corr[3] L_Omega;
  simplex[3] p[N];
}
transformed parameters{
  vector[N] mu1;
  vector[N] mu2;
  vector<lower=0>[N] mu0;
  vector[N] mu3;
  matrix[N,3] mu;

  for (n in 1:N){
    mu1[n] = log_alpha + log(LMA[n]) + log(p[n][1])
           - 0.5 * square(L_sigma[1]);
    mu2[n] = log_beta + log(LMA[n]) + log(p[n][2])
           - 0.5 * square(L_sigma[2]);
    mu0[n] = rp * LMA[n] * p[n][1] + rs * LMA[n] * p[n][2];
    mu3[n]  = log(mu0[n]) - 0.5 * square(L_sigma[3]);
  }

  mu = append_col(append_col(mu1, mu2), mu3);
}
model{
// priors
  vector[3] alpha=[1,1,1]';
  log_alpha ~ normal(0, 10);
  log_beta ~ normal(0, 10);
  rp ~ normal(0, 10);
  rs ~ normal(0, 10);
  L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
  L_sigma ~ cauchy(0, 5);
  for (n in 1:N)
    p[n] ~ dirichlet(alpha);

//model
  //obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
  // model
  for (i in 1:N)
     target += multi_normal_cholesky_lpdf(obs[i,] | mu[i,], diag_pre_multiply(L_sigma, L_Omega));
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
   log_lik[i] = multi_normal_cholesky_lpdf(obs[i,] | mu[i,], diag_pre_multiply(L_sigma, L_Omega));
 }
