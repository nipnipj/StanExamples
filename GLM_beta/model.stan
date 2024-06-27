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
 real alpha, alpha_phi;
 vector[D1] beta, beta_phi;
}

transformed parameters {
 vector[N1] mu = inv_logit(alpha + X*beta);
 vector[N1] phi = exp(alpha_phi + X*beta_phi);
}

model {
 target += student_t_lpdf(alpha | 3, 0, 2.5);
 target += student_t_lpdf(alpha_phi | 3, 0, 2.5);

 for (n in 1:N1) {
  target += beta_lpdf(y[n] | mu[n] * phi[n], (1 - mu[n]) * phi[n]);
 }
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
 real Intercept_phi = alpha_phi - dot_product(meanS, beta_phi);
}
