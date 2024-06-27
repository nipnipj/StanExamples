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
 real rho_quantile(real y, real quantile) {
  if (y < 0) {
   return y * (quantile - 1);
  } else {
   return y * quantile;
  }
 }
 real asym_laplace_lpdf(real y, real mu, real sigma, real quantile) {
  return log(quantile * (1 - quantile)) - log(sigma) -
            rho_quantile((y - mu) / sigma, quantile);
 }
}

data {
 int<lower = 1> N1, D1;
 vector[N1] y;
 real quantile;
 matrix[N1, D1] x1;
}

transformed data {
 vector[D1] meanS = meansX(x1, D1);
 matrix[N1, D1] X = centerX(x1, D1, N1, meanS);
}

parameters {
 real alpha;
 real<lower=0> sigma;
 vector[D1] beta;
}

transformed parameters {
 vector[N1] mu = alpha + X*beta;
}

model {
 target += student_t_lpdf(alpha | 3, 0, 2.5);
 target += student_t_lpdf(sigma | 3, 0, 2.5)
   - 1 * student_t_lccdf(0 | 3, 0, 2.5);

 for (n in 1:N1) {
  target += asym_laplace_lpdf(y[n] | mu[n], sigma, quantile);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
}
