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
}

transformed parameters {
 vector[N1] mu = alpha + X*beta;
}

model {
 target += student_t_lpdf(alpha | 3, 0, 2.5);
 target += student_t_lpdf(sigma | 3, 0, 2.5)
   - 1 * student_t_lccdf(0 | 3, 0, 2.5);

 target += normal_lpdf(y | mu, sigma);
}

generated quantities{
 real Intercept = alpha - dot_product(meanS, beta);
}
