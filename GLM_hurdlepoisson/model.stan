functions {
 vector meansX (matrix X, int D) {
    vector[D] means_X;
    for (i in 1:D) {
        means_X[i] = mean(X[, i]);
    }
    return means_X;
 }
 matrix centerX (matrix X, int D, int N, vector means_X) { 
    matrix[N, D] Xc;
    for (i in 1:D) {
        Xc[, i] = X[, i] - means_X[i];
    }
    return Xc; 
 }
 real hurdle_poisson_log_lpmf(int y, real eta, real hu) {
  if (y == 0) {
   return bernoulli_lpmf(1 | hu);
  } else {
   return bernoulli_lpmf(0 | hu) +
             poisson_log_lpmf(y | eta) -
             log1m_exp(-exp(eta));
  }
 }
}

data {
 int<lower = 1> N1, D1;
 array[N1] int y;
 matrix[N1, D1] x1;
}

transformed data {
 vector[D1] meanS = meansX(x1, D1);
 matrix[N1, D1] X = centerX(x1, D1, N1, meanS);
}

parameters {
 real alpha;
 real<lower=0,upper=1> hu;
 vector[D1] beta;
}

transformed parameters {
 vector[N1] mu = alpha + X*beta;
}

model {
 target += student_t_lpdf(alpha | 3, -2.3, 2.5);
 target += beta_lpdf(hu | 1, 1);

 for (n in 1:N1) {
  target += hurdle_poisson_log_lpmf(y[n] | mu[n], hu);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
}
