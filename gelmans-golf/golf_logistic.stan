data {
  int J;
  int n[J];
  int y[J];
  vector[J] x;
}
parameters {
  real a;
  real b;
}
model {
  y ~ binomial_logit(n, a + b * x);
}
