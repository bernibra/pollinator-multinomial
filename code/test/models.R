model_code_i <- "
data{
    int N;
    int y[N];
    real p2[N];
    real sleep[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bd[K-1];				// fixed effect for sleep
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bd ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bd[k] * sleep[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bd[k] * sleep[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"


model_code_ii <- "
data{
    int N;
    int y[N];
    real p1[N];
    real p2[N];
    real sleep[N];
    real year2014[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bd[K-1];				// fixed effect for sleep
    real y1[K-1];				// fixed effect for p2
    real y2[K-1];				// fixed effect for sleep
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bd ~ normal(0,10);
    y1 ~ normal(0,10);
    y2 ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = y1[k] + y2[k] * year2014[i] + ba[k] * p1[i] + bb[k] * p2[i] + bd[k] * sleep[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = y1[k] + y2[k] * year2014[i] + ba[k] * p1[i] + bb[k] * p2[i] + bd[k] * sleep[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

model_code_ii_ii <- "
data{
    int N;
    int y[N];
    real p2[N];
    real sleep[N];
    real year2014[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bd[K-1];				// fixed effect for sleep
    real y2[K-1];				// fixed effect for sleep
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bd ~ normal(0,10);
    y2 ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bd[k] * sleep[i] + y2[k] * year2014[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bd[k] * sleep[i] + y2[k] * year2014[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

model_code_iii <- "
data{
    int N;
    int y[N];
    real p1[N];
    real p2[N];
    real sleep[N];
    real year2013[N];
    real year2014[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bd[K-1];				// fixed effect for sleep
    real y1[K-1];				// fixed effect for p2
    real y2[K-1];				// fixed effect for sleep
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bd ~ normal(0,10);
    y1 ~ normal(0,10);
    y2 ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = y1[k] * year2013[i] + y2[k] * year2014[i] + ba[k] * p1[i] + bb[k] * p2[i] + bd[k] * sleep[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = y1[k] * year2013[i] + y2[k] * year2014[i] + ba[k] * p1[i] + bb[k] * p2[i] + bd[k] * sleep[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"