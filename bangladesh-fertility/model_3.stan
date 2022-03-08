data {
  int J;
  int C[J];
  int D[J];
  int U[J];
  int K[J];
  vector[3] Kprior;
  vector[J] A; 
}
parameters {
  real bU;
  real bA;
  real bK;
  simplex[3] delta;
  vector [61] a;
  real abar;
  real<lower=0> sigma;
}
model {
  vector[J] p;
  vector[4] delta_j;
  sigma ~ exponential( 1);
  abar ~ normal(0, 1);
  a ~ normal(abar, sigma);
  delta ~ dirichlet(Kprior);
  delta_j = append_row(0, delta);
  bU ~ normal(0 , 0.5);
  bA ~ normal(0 , 0.5);
  bK ~ normal(0 , 0.5);
  for (i in 1:J) {
    p[i] = a[D[i]]+ bU*U[i] + bA*A[i] + bK*sum(delta_j[1:K[i]]);
    p[i] = inv_logit(p[i]);
  }
  C ~ bernoulli(p);
}
