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
 int<lower = 2> ncat;
 array[N1] int trials;
 array[N1] vector[ncat] y;
 matrix[N1, D1] x1;
}

transformed data {
 vector[D1] meanS = meansX(x1, D1);
 matrix[N1, D1] X = centerX(x1, D1, N1, meanS);
}

parameters {
 real alpha2;
 vector[D1] beta2;

 real alpha3;
 vector[D1] beta3;
 
 real<lower=0> phi;
}

transformed parameters {
 array[N1] vector[ncat] mu;
 vector[N1] mu2, mu3;
 mu2 = alpha2 + X * beta2;
 mu3 = alpha3 + X * beta3;

 for (n in 1:N1) {
  mu[n] = transpose([0, mu2[n], mu3[n]]);
 }
}

model {
 target += student_t_lpdf(alpha2 | 3, 0, 2.5);
 target += student_t_lpdf(alpha3 | 3, 0, 2.5);
 target += gamma_lpdf(phi | 0.01, 0.01);

 for (n in 1:N1) {
  target += dirichlet_lpdf(y[n] | softmax(mu[n]) * phi);
 }
}

generated quantities{
 real Intercept2 = alpha2 - dot_product(meanS, beta2);
 real Intercept3 = alpha3 - dot_product(meanS, beta3);
}
