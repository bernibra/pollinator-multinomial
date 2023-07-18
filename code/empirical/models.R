model_code_i <- "
data{
    int N;
    int y[N];
    real p2[N];
    real p3[N];
    real sleep[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bc[K-1];				// fixed effect for p3
    real bd[K-1];				// fixed effect for sleep
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bc ~ normal(0,10);
    bd ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bc[k] * p3[i] + bd[k] * sleep[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bc[k] * p3[i] + bd[k] * sleep[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

model_code_ii <- "
data{
    int N;
    int y[N];
    real p2[N];
    real p3[N];
    real sleep[N];
    real year2014[N];
    real year2015[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bc[K-1];				// fixed effect for p3
    real bd[K-1];				// fixed effect for sleep
    real by2[K-1];				// fixed effect for year2014
    real by3[K-1];				// fixed effect for year2015
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bc ~ normal(0,10);
    bd ~ normal(0,10);
    by2 ~ normal(0,10);
    by3 ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bc[k] * p3[i] + bd[k] * sleep[i] + by2[k] * year2014[i] + by3[k] * year2015[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bb[k] * p2[i] + bc[k] * p3[i] + bd[k] * sleep[i] + by2[k] * year2014[i] + by3[k] * year2015[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

model_code_iii <- "
data{
    int N;
    int y[N];
    real p2[N];
    real p3[N];
    real sleep[N];
    real p2T[N];
    real p3T[N];
    real sleepT[N];
    real year2014[N];
    real year2015[N];
    real time[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bc[K-1];				// fixed effect for p3
    real bd[K-1];				// fixed effect for sleep
    real bTa[K-1];				// fixed effect for week p1
    real bTb[K-1];				// fixed effect for week p2
    real bTc[K-1];				// fixed effect for week p3
    real bTd[K-1];				// fixed effect for week sleep
    real by2[K-1];				// fixed effect for year2014
    real by3[K-1];				// fixed effect for year2015
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bc ~ normal(0,10);
    bd ~ normal(0,10);
    bTa ~ normal(0,10);
    bTb ~ normal(0,10);
    bTc ~ normal(0,10);
    bTd ~ normal(0,10);
    by2 ~ normal(0,10);
    by3 ~ normal(0,10);
    
    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bTa[k] * time[i] + bb[k] * p2[i] + bTb[k] * p2T[i] + bc[k] * p3[i] + bTc[k] * p3T[i] + bd[k] * sleep[i] + bTd[k] * sleepT[i] + by2[k] * year2014[i] + by3[k] * year2015[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bTa[k] * time[i] + bb[k] * p2[i] + bTb[k] * p2T[i] + bc[k] * p3[i] + bTc[k] * p3T[i] + bd[k] * sleep[i] + bTd[k] * sleepT[i] + by2[k] * year2014[i] + by3[k] * year2015[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

model_code_v <- "
data{
    int N;
    int y[N];
    real p2[N];
    real p3[N];
    real sleep[N];
    real p2T[N];
    real p3T[N];
    real sleepT[N];
    real year2014[N];
    real year2015[N];
    real year2014T[N];
    real year2015T[N];
    real time[N];
    int K;
}
parameters{
    real ba[K-1];				// fixed effect for p1
    real bb[K-1];				// fixed effect for p2
    real bc[K-1];				// fixed effect for p3
    real bd[K-1];				// fixed effect for sleep
    real bTa[K-1];				// fixed effect for week p1
    real bTb[K-1];				// fixed effect for week p2
    real bTc[K-1];				// fixed effect for week p3
    real bTd[K-1];				// fixed effect for week sleep
    real by2[K-1];				// fixed effect for year2014
    real by3[K-1];				// fixed effect for year2015
    real bTy2[K-1];				// fixed effect for year2014
    real bTy3[K-1];				// fixed effect for year2015
}
model{
    
    // priors
    ba ~ normal(0,10);
    bb ~ normal(0,10);
    bc ~ normal(0,10);
    bd ~ normal(0,10);
    bTa ~ normal(0,10);
    bTb ~ normal(0,10);
    bTc ~ normal(0,10);
    bTd ~ normal(0,10);
    by2 ~ normal(0,10);
    by3 ~ normal(0,10);
    bTy2 ~ normal(0,10);
    bTy3 ~ normal(0,10);
    
    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bTa[k] * time[i] + bb[k] * p2[i] + bTb[k] * p2T[i] + bc[k] * p3[i] + bTc[k] * p3T[i] + bd[k] * sleep[i] + bTd[k] * sleepT[i] + by2[k] * year2014[i] + bTy2[k] * year2014T[i] + by3[k] * year2015[i] + bTy3[k] * year2015T[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    vector[N] log_lik;

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = ba[k] + bTa[k] * time[i] + bb[k] * p2[i] + bTb[k] * p2T[i] + bc[k] * p3[i] + bTc[k] * p3T[i] + bd[k] * sleep[i] + bTd[k] * sleepT[i] + by2[k] * year2014[i] + bTy2[k] * year2014T[i] + by3[k] * year2015[i] + bTy3[k] * year2015T[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"
