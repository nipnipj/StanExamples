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
 real zero_inflated_neg_binomial_log_logit_lpmf(int y, real eta,
                                                 real phi, real zi) {
  if (y == 0) {
   return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         neg_binomial_2_log_lpmf(0 | eta, phi));
  } else {
   return bernoulli_logit_lpmf(0 | zi) +
             neg_binomial_2_log_lpmf(y | eta, phi);
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
 real alpha, alpha_sh, alpha_zi;
 vector[D1] beta, beta_sh, beta_zi;
}

transformed parameters {
 vector[N1] mu = alpha + X*beta;
 vector[N1] zi = alpha_zi + X*beta_zi;
 vector[N1] shape = exp(alpha_sh + X*beta_sh);
}

model {
 target += student_t_lpdf(alpha | 3, -2.3, 2.5);
 target += student_t_lpdf(alpha_sh | 3, 0, 2.5);
 target += logistic_lpdf(alpha_zi | 0, 1);

 for (n in 1:N1) {
  target += zero_inflated_neg_binomial_log_logit_lpmf(y[n] | mu[n], shape[n], zi[n]);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
 real Intercept_zi = alpha_zi - dot_product(meanS, beta_zi);
 real Intercept_sh = alpha_sh - dot_product(meanS, beta_sh);
}
