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
 real zero_inflated_binomial_blogit_lpmf(int y, int trials,
                                          real eta, real zi) {
  if (y == 0) {
   return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         binomial_logit_lpmf(0 | trials, eta));
  } else {
   return bernoulli_lpmf(0 | zi) +
             binomial_logit_lpmf(y | trials, eta);
  }
 }
}

data {
 int<lower = 1> N1, D1;
 array[N1] int trials;
 array[N1] int y;
 matrix[N1, D1] x1;
}

transformed data {
 vector[D1] meanS = meansX(x1, D1);
 matrix[N1, D1] X = centerX(x1, D1, N1, meanS);
}

parameters {
 real alpha;
 vector[D1] beta;

 real<lower=0,upper=1> zi;
}

transformed parameters {
 vector[N1] mu;
 mu = alpha + X * beta;
}

model {
 target += student_t_lpdf(alpha | 3, 0, 2.5);
 target += beta_lpdf(zi | 1, 1);

 for (n in 1:N1) {
  target += zero_inflated_binomial_blogit_lpmf(y[n] | trials[n], mu[n], zi);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
}
