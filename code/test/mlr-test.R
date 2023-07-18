library(rethinking)
library(matrixStats)
library(Hmisc)

########################
# GENERATE DATA
########################

#year 2013
p1_done <- 0.2
p1_p1 <- 0.5
p1_p2 <- 0.3
p1_sleep <- 0.0

p2_done <- 0.6
p2_p1 <- 0.15
p2_p2 <- 0.25
p2_sleep <- 0.0

psleep_done <- 0.0
psleep_p1 <- 0.27
psleep_p2 <- 0.03
psleep_sleep <- 0.7
pmat <- rbind(c(1,1,1,1),
              c(p1_done, p1_p1, p1_p2, p1_sleep),
              c(p2_done, p2_p1, p2_p2, p2_sleep),
              c(psleep_done, psleep_p1, psleep_p2, psleep_sleep))

#year 2014
p1_done <- 0.2
p1_p1 <- 0.5
p1_p2 <- 0.3
p1_sleep <- 0.0

p2_done <- 0.33
p2_p1 <- 0.33
p2_p2 <- 0.34
p2_sleep <- 0.0

psleep_done <- 0.0
psleep_p1 <- 0.27
psleep_p2 <- 0.03
psleep_sleep <- 0.7

pmat2 <- rbind(c(1,1,1,1),
              c(p1_done, p1_p1, p1_p2, p1_sleep),
              c(p2_done, p2_p1, p2_p2, p2_sleep),
              c(psleep_done, psleep_p1, psleep_p2, psleep_sleep))

N <- 1000
previous1 <- sample(c(2,3,4), N, replace = T)
state1 <- sapply(previous1, function(idx) sample(c(1,2,3,4), 1, prob = pmat[idx,]))

previous2 <- sample(c(2,3,4), N, replace = T)
state2 <- sapply(previous2, function(idx) sample(c(1,2,3,4), 1, prob = pmat2[idx,]))

markchain <- data.frame(state=c(state1, state2), previous=c(previous1, previous2), year=c(rep(2013, length(previous1)), rep(2014, length(previous2))))

########################
# Reformat data
########################

markchain$state[markchain$state==4] <- "sleep"
markchain$sleep <- ifelse( markchain$previous==4 , 1 , 0 )
markchain$p1 <- ifelse( markchain$previous==2 , 1 , 0 )
markchain$p2 <- ifelse( markchain$previous==3 , 1 , 0 )
markchain$year2013 <- ifelse( markchain$year==2013 , 1 , 0 )
markchain$year2014 <- ifelse( markchain$year==2014 , 1 , 0 )
markchain$year[markchain$year==2013] <- "a"
markchain$year[markchain$year==2014] <- "b"

N <- nrow(markchain) ## Number of observations
markchain$y <- coerce_index (markchain$state) ## Renaming response variable
K <- max(markchain$y) ## Number of response categories
markchain$year_id <- coerce_index (markchain$year) ## Index of months in which observations occurred
N_year <- max(markchain$year_id)

########################
# RSTAN LISTS
# (i) constant p model without year as treatment
# (ii) constant p model with year as treatment but n dummy variables for n origin group categories
# (ii_ii) constant p model with year as treatment and n-1 dummy variables for n origin group categories
# (iii) same as (ii) with intercept but with additional year dummy variable
########################

dat_list_i <- list(
  K = K,
  N = N,
  y = markchain$y,
  sleep = markchain$sleep,
  # p1 = markchain$p1,
  p2 = markchain$p2
  )

dat_list_ii <- list(
  K = K,
  N = N,
  y = markchain$y,
  p1 = markchain$p1,
  p2 = markchain$p2,
  sleep = markchain$sleep,
  # year2013 = markchain$year2013,
  year2014 = markchain$year2014
)

dat_list_ii_ii <- list(
  K = K,
  N = N,
  y = markchain$y,
  # p1 = markchain$p1,
  p2 = markchain$p2,
  sleep = markchain$sleep,
  # year2013 = markchain$year2013,
  year2014 = markchain$year2014
)

dat_list_iii <- list(
  K = K,
  N = N,
  y = markchain$y,
  p1 = markchain$p1,
  p2 = markchain$p2,
  sleep = markchain$sleep,
  year2013 = markchain$year2013,
  year2014 = markchain$year2014
)

########################
# GENERATE STAN CODE
########################

source("./models.R")

########################
# SET STARTING VALUES AND RUN MODELS
########################

# Model i
start_i <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bd = rep(0,K-1)
)

n_chains_i <- 3
init_i <- list()
for ( i in 1:n_chains_i ) init_i[[i]] <- start_i

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_i <- stan ( model_code=model_code_i , data=dat_list_i , chains=n_chains_i , cores= n_chains_i , warmup=1000, iter=2000, init=init_i , control = list(adapt_delta = 0.95))
precis(mfit_i, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_i_out <- precis(mfit_i, depth = 2, prob = .96)
write.csv(mfit_i_out, file = "./mfit.i.csv")

# Model ii
start_ii <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bd = rep(0,K-1),
  y1 = rep(0,K-1),
  y2 = rep(0,K-1)
)

n_chains_ii <- 3
init_ii <- list()
for ( i in 1:n_chains_ii ) init_ii[[i]] <- start_ii

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_ii <- stan ( model_code=model_code_ii , data=dat_list_ii , chains=n_chains_ii , cores= n_chains_ii , warmup=1000, iter=2000, init=init_ii , control = list(adapt_delta = 0.95))
precis(mfit_ii, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_ii_out <- precis(mfit_ii, depth = 2, prob = .96)
write.csv(mfit_ii_out, file = "./mfit.ii.csv")

# Model ii
start_ii_ii <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bd = rep(0,K-1),
  y2 = rep(0,K-1)
)

n_chains_ii_ii <- 3
init_ii_ii <- list()
for ( i in 1:n_chains_ii_ii ) init_ii_ii[[i]] <- start_ii_ii

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_ii_ii <- stan ( model_code=model_code_ii_ii , data=dat_list_ii_ii , chains=n_chains_ii_ii , cores= n_chains_ii_ii , warmup=1000, iter=2000, init=init_ii_ii , control = list(adapt_delta = 0.95))
precis(mfit_ii_ii, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_ii_ii_out <- precis(mfit_ii_ii, depth = 2, prob = .96)
write.csv(mfit_ii_ii_out, file = "./mfit.ii_ii.csv")

# Model iii
start_iii <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bd = rep(0,K-1),
  y1 = rep(0,K-1),
  y2 = rep(0,K-1)
)

n_chains_iii <- 3
init_iii <- list()
for ( i in 1:n_chains_iii ) init_iii[[i]] <- start_iii

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_iii <- stan ( model_code=model_code_iii , data=dat_list_iii , chains=n_chains_iii , cores= n_chains_iii , warmup=1000, iter=2000, init=init_iii , control = list(adapt_delta = 0.95))
precis(mfit_iii, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_iii_out <- precis(mfit_iii, depth = 2, prob = .96)
write.csv(mfit_iii_out, file = "./mfit.iii.csv")


#######################################################################################################
## MODEL COMPARISON USING WAIC
#######################################################################################################
models <- compare (mfit_i, mfit_ii, mfit_ii_ii, mfit_iii)
plot(models)

############################
# LINK FUNCTION FOR DIFFERENT MODELS
############################
link.i <- function( data , post, K=K) {
  ns <- dim(post$ba)[1]
  n <- dim(data)[1]
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post$ba[,k] + 
          post$bb[,k] * data$p2[i] + 
          post$bd[,k] * data$sleep[i]
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    ## The values are converted to probabilities using the softmax function
    ## which ensures that the predicted values across categories sum to
    ## 100% probabilities.
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}

link.ii <- function( data , post, K=K) {
  ns <- dim(post$ba)[1]
  n <- dim(data)[1]
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post$ba[,k] * data$p1[i] + 
          post$bb[,k] * data$p2[i] + 
          post$bd[,k] * data$sleep[i] +
          post$y1[,k] +
          post$y2[,k] * as.numeric(data$year[i]==2)
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    ## The values are converted to probabilities using the softmax function
    ## which ensures that the predicted values across categories sum to
    ## 100% probabilities.
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}

link.ii_ii <- function( data , post, K=K) {
  ns <- dim(post$ba)[1]
  n <- dim(data)[1]
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post$ba[,k] + 
          post$bb[,k] * data$p2[i] + 
          post$bd[,k] * data$sleep[i] +
          post$y2[,k] * as.numeric(data$year[i]==2)
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    ## The values are converted to probabilities using the softmax function
    ## which ensures that the predicted values across categories sum to
    ## 100% probabilities.
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}

link.iii <- function( data , post, K=K) {
  ns <- dim(post$ba)[1]
  n <- dim(data)[1]
  
  softmax2 <- function(x) {
    x <- max(x) - x
    exp(-x)/sum(exp(-x))
  }
  
  p <- list()
  
  for ( i in 1:n ) {
    p[[i]] <- sapply( 1:K , function(k) {
      if ( k < K ) {
        ptemp <- post$ba[,k] * data$p2[i]+ 
          post$bb[,k] * data$p2[i] + 
          post$bd[,k] * data$sleep[i] +
          post$y1[,k] * as.numeric(data$year[i]==1) +
          post$y2[,k] * as.numeric(data$year[i]==2)
      } else {
        ptemp <- rep(0,ns)
      }
      return(ptemp)
    })
    ## The values are converted to probabilities using the softmax function
    ## which ensures that the predicted values across categories sum to
    ## 100% probabilities.
    for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
  }
  return(p)
}


me <- c()
mi <- c()
ma <- c()
for (m in 1:3){
  v <- rep(0,3)
  v[m] <- 1
  # for(n in rle(dat$time)$values){
  i <- link.i(data = data.frame(sleep=v[3],p1=v[1],p2=v[2]), post=extract.samples(mfit_i), K=K)
  me <- rbind(me,colQuantiles(i[[1]], probs = 0.5))
  mi <- rbind(mi,colQuantiles(i[[1]], probs = 0.01))
  ma <- rbind(ma,colQuantiles(i[[1]], probs = 0.99))
}

me <- c()
mi <- c()
ma <- c()
for (m in 1:3){
  v <- rep(0,3)
  v[m] <- 1
  # for(n in rle(dat$time)$values){
  i <- link.ii(data = data.frame(sleep=v[3],p1=v[1],p2=v[2], year=2), post=extract.samples(mfit_ii), K=K)
  me <- rbind(me,colQuantiles(i[[1]], probs = 0.5))
  mi <- rbind(mi,colQuantiles(i[[1]], probs = 0.01))
  ma <- rbind(ma,colQuantiles(i[[1]], probs = 0.99))
}

me <- c()
mi <- c()
ma <- c()
for (m in 1:3){
  v <- rep(0,3)
  v[m] <- 1
  # for(n in rle(dat$time)$values){
  i <- link.ii_ii(data = data.frame(sleep=v[3],p1=v[1],p2=v[2], year=2), post=extract.samples(mfit_ii_ii), K=K)
  me <- rbind(me,colQuantiles(i[[1]], probs = 0.5))
  mi <- rbind(mi,colQuantiles(i[[1]], probs = 0.01))
  ma <- rbind(ma,colQuantiles(i[[1]], probs = 0.99))
}



me <- c()
mi <- c()
ma <- c()
for (m in 1:3){
  v <- rep(0,3)
  v[m] <- 1
  # for(n in rle(dat$time)$values){
  i <- link.iii(data = data.frame(sleep=v[3],p1=v[1],p2=v[2], year=2), post=extract.samples(mfit_iii), K=K)
  me <- rbind(me,colQuantiles(i[[1]], probs = 0.5))
  mi <- rbind(mi,colQuantiles(i[[1]], probs = 0.01))
  ma <- rbind(ma,colQuantiles(i[[1]], probs = 0.99))
}
