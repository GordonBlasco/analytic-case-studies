//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int J;
  vector[J] A;
  vector[J] B;
  vector[J] I;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real aA;
  real aB;
  real bIA;
  real bAB;
  corr_matrix[2] Rho;
  vector<lower=0>[2] Sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  vector[500] muB;
  vector[500] muA;
  Sigma ~ exponential( 1 );
  Rho ~ lkj_corr( 2 );
  bAB ~ normal( 0 , 0.5 );
  bIA ~ normal( 0 , 0.5 );
  aB ~ normal( 0 , 0.2 );
  aA ~ normal( 0 , 0.2 );
  
  
  
  for ( i in 1:500 ) muA[i] = aA + bIA * I[i];
  for (i in 1:J)  muB[i] = aB + bAB * A[i];
  
  {
  vector[2] YY[J];
  vector[2] MU[J];
  for (j in 1:J) MU[j] = [ muB[j], muA[j] ]';
  for (j in 1:J) YY[j] = [ B[j], A[j] ]';
  YY ~ multi_normal(MU, quad_form_diag(Rho, Sigma));
  }
}

