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
 real scale_xi(real xi, vector y, vector mu, real sigma) {
  vector[rows(y)] x = (y - mu) / sigma;
  vector[2] bounds = [-inv(min(x)), -inv(max(x))]';
  real lb = min(bounds);
  real ub = max(bounds);
  return inv_logit(xi) * (ub - lb) + lb;
 }
 real gen_extreme_value_lpdf(real y, real mu, real sigma, real xi) {
  real x = (y - mu) / sigma;
  if (xi == 0) {
   return - log(sigma) - x - exp(-x);
  } else {
   real t = 1 + xi * x;
   real inv_xi = 1 / xi;
   return - log(sigma) - (1 + inv_xi) * log(t) - pow(t, -inv_xi);
  }
 }
}

data {
 int<lower = 1> N1, D1;
 vector[N1] y;
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

 real tmp_xi;
}

transformed parameters {
 vector[N1] mu = alpha + X*beta;

 real xi = scale_xi(tmp_xi, y, mu, sigma);
}

model {
 target += student_t_lpdf(alpha | 3, 0, 2.5);
 target += student_t_lpdf(sigma | 3, 0, 2.5)
   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
 target += normal_lpdf(tmp_xi | 0, 2.5);

 for (n in 1:N1) {
  target += gen_extreme_value_lpdf(y[n] | mu[n], sigma, xi);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
}
