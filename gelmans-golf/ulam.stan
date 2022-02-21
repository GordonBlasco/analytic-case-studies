data{
    int J;
    int n[J];
    int y[J];
    int x[J];
}
parameters{
    real a;
    real b;
}
model{
    vector[J] p;
    b ~ normal( 0 , 1.5 );
    a ~ normal( 0 , 0.5 );
    for ( i in 1:J ) {
        p[i] = a + b * x[i];
        p[i] = inv_logit(p[i]);
    }
    y ~ binomial( n , p );
}
generated quantities{
    vector[J] log_lik;
    vector[J] p;
    for ( i in 1:J ) {
        p[i] = a + b * x[i];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:J ) log_lik[i] = binomial_lpmf( y[i] | n[i] , p[i] );
}