data {
  int J;
  int C[J];
  int D[J];
}
parameters {
  vector [61] a;
  real abar;
  real<lower=0> sigma;
}
model {
  vector[J] p;
  sigma ~ exponential( 1);
  abar ~ normal(0, 1);
  a ~ normal(abar, sigma);
  for (i in 1:J) {
    p[i] = a[D[i]];
    p[i] = inv_logit(p[i]);
  }
  C ~ bernoulli(p);
}
