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
 real zero_one_inflated_beta_lpdf(real y, real mu, real phi, real zoi, real coi) {
  row_vector[2] shape = [mu * phi, (1 - mu) * phi];
  if (y == 0) {
   return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(0 | coi);
  } else if (y == 1) {
   return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(1 | coi);
  } else {
   return bernoulli_lpmf(0 | zoi) + beta_lpdf(y | shape[1], shape[2]);
  }
 }
}

data {
 int<lower = 1> N1, D1;
 vector<lower=0, upper=1>[N1] y;
 matrix[N1, D1] x1;
}

transformed data {
 vector[D1] meanS = meansX(x1, D1);
 matrix[N1, D1] X = centerX(x1, D1, N1, meanS);
}

parameters {
 real alpha, alpha_phi, alpha_zoi, alpha_coi;
 vector[D1] beta, beta_phi, beta_zoi, beta_coi;
}

transformed parameters {
 vector[N1] mu = inv_logit(alpha + X*beta);
 vector[N1] phi = exp(alpha_phi + X*beta_phi);
 vector[N1] zoi = inv_logit(alpha_zoi + X*beta_zoi);
 vector[N1] coi = inv_logit(alpha_coi + X*beta_coi);
}

model {
 target += student_t_lpdf(alpha | 3, 0, 2.5);
 target += student_t_lpdf(alpha_phi | 3, 0, 2.5);
 target += logistic_lpdf(alpha_zoi | 0, 1);
 target += logistic_lpdf(alpha_coi | 0, 1);

 for (n in 1:N1) {
  target += zero_one_inflated_beta_lpdf(y[n] | mu[n], phi[n], zoi[n], coi[n]);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
 real Intercept_phi = alpha_phi - dot_product(meanS, beta_phi);
 real Intercept_zoi = alpha_zoi - dot_product(meanS, beta_zoi);
 real Intercept_coi = alpha_coi - dot_product(meanS, beta_coi);
}
