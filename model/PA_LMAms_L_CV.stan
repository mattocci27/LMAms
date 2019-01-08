functions{
  real repulsive_lpdf(real[] gamma, real tau, real nu) {
    real lik;
    real tmp;
    lik = normal_lpdf(gamma[1]| 0, 1) +
          normal_lpdf(gamma[2]| 0, 1);
    tmp = -tau * pow(fabs(gamma[2]-gamma[1]), -nu);
    return lik + tmp;
  }
}
data{
  int<lower=0> N;
  int<lower=0> J;
  int jj[N];
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
  vector[8] z;
  real am;
  real<upper=am> as;
  real<lower=0> tau;
  vector<lower=0, upper=1>[N] p;
  vector<lower=0>[3] L_sigma;
  cholesky_factor_corr[3] L_Omega;
}
transformed parameters{
  matrix[4,3] Z;
  matrix[N,3] Mu;
  matrix[N,4] X;
  matrix[N,3] L_Sigma;
  vector[N] log_LMAp;
  vector[N] log_LMAs;
  vector<lower=0>[4] var_p0;
  vector<lower=0>[4] var_p;
  vector<lower=0>[4] lambda;
  vector<lower=0>[4] phi0;
  vector<lower=0>[4] phi;
  vector<lower=0>[4] alpha;
  vector<lower=0>[4] beta;
  vector<lower=0>[4] J2;
  phi0 = [0,0,0,0]';
  var_p0 = [0,0,0,0]';
  J2 = [0,0,0,0]';
  for (n in 1:N) {
    if(holdout[n] == 0) {
      phi0[jj[n]] += p[n];
      J2[jj[n]] += 1;
    }
  }
  phi = phi0 ./ J2;
  for (n in 1:N) {
    if(holdout[n] == 0) {
      var_p0[jj[n]] += (phi[jj[n]] - p[n])^2;
    }
  }
  var_p = var_p0 ./ J2;
  lambda = phi .* (1 - phi) ./ var_p - 1;
  alpha = lambda .* phi;
  beta = lambda .* (1 - phi);

  Z[1,1] = z[1];
  Z[1,2] = z[2];
  Z[1,3] = z[3];
  Z[2,1] = am;
  Z[2,2] = z[4];
  Z[2,3] = z[5];
  Z[3,1] = as;
  Z[3,2] = z[6];
  Z[3,3] = z[7];
  Z[4,1] = 0;
  Z[4,2] = z[8];
  Z[4,3] = 0;
  L_Sigma = rep_matrix(to_row_vector(0.5 * L_sigma .* L_sigma), N);
  log_LMAp = log(LMA) + log(p);
  log_LMAs = log(LMA) + log(1 - p);
  X = append_col(append_col(append_col(intercept, log_LMAp), log_LMAs), leaf);
  Mu = X * Z - L_Sigma;
}
model{
  // priors
  real a_sm[2];
  a_sm[1] = am;
  a_sm[2] = as;
  a_sm ~ repulsive(tau, 2);
  z ~ normal(0, 10);
  tau ~ cauchy(0, 2.5);
  L_Omega ~ lkj_corr_cholesky(2); //uniform of L_Omega * L_Omega'
  L_sigma ~ cauchy(0, 5);

  // model
  for (i in 1:N) {
    if(holdout[i] == 0) {
      p[i] ~ beta(2, 2);
      target += multi_normal_cholesky_lpdf(obs[i,] | Mu[i,], diag_pre_multiply(L_sigma, L_Omega));
    }
  // need to estimate p for holdout[n] == 1
    else {
     p[i] ~ beta(alpha[jj[i]], beta[jj[i]]); // alpha and beta will be transformed data
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
