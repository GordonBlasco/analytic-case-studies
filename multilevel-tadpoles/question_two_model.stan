data{
    int D[48];
    int S[48];
    int G[48];
    int P[48];
    int T[48];
}
parameters{
    vector[48] a;
    matrix[2,2] b;
    real<lower=0> sigma;
}
model{
    vector[48] p;
    sigma ~ exponential( 1 );
    to_vector( b ) ~ normal( 0 , 1 );
    a ~ normal( 0 , sigma );
    for ( i in 1:48 ) {
        p[i] = a[T[i]] + b[P[i], G[i]];
        p[i] = inv_logit(p[i]);
    }
    S ~ binomial( D , p );
}
generated quantities{
    vector[48] log_lik;
    vector[48] p;
    for ( i in 1:48 ) {
        p[i] = a[T[i]] + b[P[i], G[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:48 ) log_lik[i] = binomial_lpmf( S[i] | D[i] , p[i] );
}
