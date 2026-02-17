
  data {
    int<lower=2> K;                      // number of categories
    int<lower=1> I;                      // items
    int<lower=1> J;                      // raters
    int<lower=1> N;                      // number of observed ratings
    array[N] int<lower=1, upper=I> ii;   // item index per observation
    array[N] int<lower=1, upper=J> jj;   // rater index per observation
    array[N] int<lower=1, upper=K> y;    // observed category per observation
  
    vector<lower=0>[K] alpha;            // prior for pi
    array[K] vector<lower=0>[K] beta;    // Dirichlet priors for theta rows, vary by true class
  }

parameters {
  simplex[K] pi;                       // prevalence over true classes
  array[J, K] simplex[K] theta;        // confusion rows: for rater j and true class k
}

transformed parameters {
  // log_q_z[i, k] = log p(z_i = k | pi, theta) up to a constant
  array[I] vector[K] log_q_z;

  // Initialize with log pi
  for (i in 1:I) {
    log_q_z[i] = log(pi);
  }

  // Accumulate per observation
  for (n in 1:N) {
    int i = ii[n];
    int j = jj[n];
    int y_n = y[n];
    for (k in 1:K) {
      // Add log categorical pmf of y_n under theta[j, k, :]
      log_q_z[i, k] += categorical_lpmf(y_n | theta[j, k]);
    }
  }
}

model {
  // Priors
  pi ~ dirichlet(alpha);
  for (j in 1:J)
    for (k in 1:K)
      theta[j, k] ~ dirichlet(beta[k]);

  // Marginalized likelihood
  for (i in 1:I) {
    target += log_sum_exp(log_q_z[i]);
  }
}

generated quantities {
  array[I] simplex[K] q_z;
  for (i in 1:I) {
    vector[K] lq = log_q_z[i];
    q_z[i] = softmax(lq);
  }
}

