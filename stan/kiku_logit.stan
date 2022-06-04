data{
  int<lower=0> N;
  vector<lower=0>[N] LMA;
  vector<lower=0>[N] A;
  vector<lower=0>[N] R;
  vector<lower=0>[N] LL;
  vector<lower=0>[N] leaf;
  real<lower=0> theta_lim;
}
transformed data{
  vector[N] log_LL;
  log_LL = log(LL);
}
parameters{
  vector[2] beta;
  real<lower=theta_lim, upper=0.999> theta;
  vector<lower=0, upper=1>[N] p;
  real<lower=0> sigma;
  real<lower=0> tau;
}
transformed parameters{
  vector[N] mu;
  vector[N] z;
  z = logit(p);
  for (i in 1:N){
    if (leaf[i]==1) // sun
      mu[i] = beta[1] 
        + 0.5 * beta[2] * log(1 - p[i])
        + 0.5 * (beta[2] + 1) * log(LMA[i])
        - 0.5 * log(A[i] - R[i])
        - 0.5 * square(sigma);
    else
      mu[i] = beta[1] 
        + 0.5 * beta[2] * log(1 - p[i])
        + 0.5 * (beta[2] + 1) * log(LMA[i])
        - 0.5 * log(theta * A[i] - R[i])
        - 0.5 * square(sigma);
    }
}
model{
  // priors
  beta ~ normal(0, 2.5);
  theta ~ beta(1, 1);
  tau ~ cauchy(0, 2.5);
  //z ~ normal(0, tau);
  target += normal_lpdf(z | 0, tau);
  // model
  for (i in 1:N)
     target += normal_lpdf(log_LL[i] | mu[i], sigma);
}
generated quantities {
  vector[N] log_lik;
  for (i in 1:N)
   log_lik[i] = normal_lpdf(log_LL[i] | mu[i], sigma);
 }
