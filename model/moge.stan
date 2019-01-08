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
  vector<lower=0, upper=1>[N] p;
}
transformed parameters{
  vector<lower=0>[4] phi;
  vector<lower=0>[4] J2;
  vector<lower=0>[4] phi2;
  phi = [0,0,0,0]';
  J2 = [0,0,0,0]';
  for (n in 1:N) {
    if(holdout[n] == 0) {
      phi[jj[n]] += p[n];
      J2[jj[n]] += 1;
    }
  }
  phi2 = phi ./ J2;
}
model{
  // priors
  p ~ beta(2,2);
  print(phi);
  print(J2);
  print(phi2);
}

