library(rethinking)
library(matrixStats)
library(Hmisc)

whos <- "pollinators"

########################
# LOAD DATA
########################

markchain <- readRDS("../data/pollinator-data.rds")

########################
# Reformat data
########################

N <- nrow(markchain) ## Number of observations
markchain$y <- coerce_index (markchain$state) ## Renaming response variable
K <- max(markchain$y) ## Number of response categories
markchain$id <- coerce_index (markchain$sp) ## Index of observed individuals
N_id <- max(markchain$id) ## Number of observed individuals
markchain$year_id <- coerce_index (markchain$year) ## Index of months in which observations occurred
N_year <- 3
markchain$p1T <- markchain$time * markchain$p1
markchain$p2T <- markchain$time * markchain$p2
markchain$p3T <- markchain$time * markchain$p3
markchain$sleepT <- markchain$time * markchain$sleep
markchain$year2013T <- markchain$time * markchain$year2013
markchain$year2014T <- markchain$time * markchain$year2014
markchain$year2015T <- markchain$time * markchain$year2015
markchain$p1A <- markchain$abundance * markchain$p1
markchain$p2A <- markchain$abundance * markchain$p2
markchain$p3A <- markchain$abundance * markchain$p3
markchain$sleepA <- markchain$abundance * markchain$sleep
markchain$year2013A <- markchain$abundance * markchain$year2013
markchain$year2014A <- markchain$abundance * markchain$year2014
markchain$year2015A <- markchain$abundance * markchain$year2015

########################
# RSTAN LISTS
# (i) constant p model without year as treatments
# (ii) constant p model with year as treatments on the intercept
# (iii) time-dependent p model with year as treatments on the intercept
# (iv)[deprecated] abundance-dependent p model with year as treatment on the intercept
# (v) time-dependent p model with year as treatments in both the intercept and slope
# (vi)[deprecated] abundance-dependent p model with year as treatments in both the intercept and slope

########################

dat_list_i <- list(
  K = K,
  N = N,
  y = markchain$y,
  sleep = markchain$sleep,
  # p1 = markchain$p1,
  p2 = markchain$p2,
  p3 = markchain$p3
)

dat_list_ii <- list(
  K = K,
  N = N,
  y = markchain$y,
  # p1 = markchain$p1,
  p2 = markchain$p2,
  p3 = markchain$p3,
  sleep = markchain$sleep,
  # year2013 = markchain$year2013,
  year2014 = markchain$year2014,
  year2015 = markchain$year2015
)

dat_list_iii <- list(
  K = K,
  N = N,
  y = markchain$y,
  # p1 = markchain$p1,
  p2 = markchain$p2,
  p3 = markchain$p3,
  sleep = markchain$sleep,
  # p1T = markchain$p1T,
  p2T = markchain$p2T,
  p3T = markchain$p3T,
  sleepT = markchain$sleepT,
  # year2013 = markchain$year2013,
  year2014 = markchain$year2014,
  year2015 = markchain$year2015,
  time = markchain$time
)

dat_list_v <- list(
  K = K,
  N = N,
  y = markchain$y,
  # p1 = markchain$p1,
  p2 = markchain$p2,
  p3 = markchain$p3,
  sleep = markchain$sleep,
  # p1T = markchain$p1T,
  p2T = markchain$p2T,
  p3T = markchain$p3T,
  sleepT = markchain$sleepT,
  # year2013 = markchain$year2013,
  year2014 = markchain$year2014,
  year2015 = markchain$year2015,
  year2014T = markchain$year2014T,
  year2015T = markchain$year2015T,
  time = markchain$time
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
  bc = rep(0,K-1),
  bd = rep(0,K-1)
)

n_chains_i <- 3
init_i <- list()
for ( i in 1:n_chains_i ) init_i[[i]] <- start_i

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_i <- stan ( model_code=model_code_i , data=dat_list_i , chains=n_chains_i , cores= n_chains_i , warmup=1000, iter=4000, init=init_i , control = list(adapt_delta = 0.95))
saveRDS(object = mfit_i, file = paste("./mfit_i_",whos,".rds", sep=""))
precis(mfit_i, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_i_out <- precis(mfit_i, depth = 2, prob = .96)
# write.csv(mfit_i_out, file = "./mfit.i.csv")

# Model ii
start_ii <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bc = rep(0,K-1),
  bd = rep(0,K-1),
  by2 = rep(0,K-1),
  by3 = rep(0,K-1)
)

n_chains_ii <- 3
init_ii <- list()
for ( i in 1:n_chains_ii ) init_ii[[i]] <- start_ii

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_ii <- stan ( model_code=model_code_ii , data=dat_list_ii , chains=n_chains_ii , cores= n_chains_ii , warmup=1000, iter=4000, init=init_ii , control = list(adapt_delta = 0.95))
saveRDS(object = mfit_ii, file = paste("./mfit_ii_",whos,".rds", sep=""))
precis(mfit_ii, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_ii_out <- precis(mfit_ii, depth = 2, prob = .96)
# write.csv(mfit_ii_out, file = "./mfit.ii.csv")


# Model iii
start_iii <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bc = rep(0,K-1),
  bd = rep(0,K-1),
  bTa = rep(0,K-1),
  bTb = rep(0,K-1),
  bTc = rep(0,K-1),
  bTd = rep(0,K-1),
  by2 = rep(0,K-1),
  by3 = rep(0,K-1)
)

n_chains_iii <- 3
init_iii <- list()
for ( i in 1:n_chains_iii ) init_iii[[i]] <- start_iii

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_iii <- stan ( model_code=model_code_iii , data=dat_list_iii , chains=n_chains_iii , cores= n_chains_iii , warmup=1000, iter=4000, init=init_iii , control = list(adapt_delta = 0.95))
saveRDS(object = mfit_iii, file = paste("./mfit_iii_",whos,".rds", sep=""))
precis(mfit_iii, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_iii_out <- precis(mfit_iii, depth = 2, prob = .96)
# write.csv(mfit_iii_out, file = "./mfit.iii.csv")

# Model v
start_v <- list (
  ba = rep(0,K-1),
  bb = rep(0,K-1),
  bc = rep(0,K-1),
  bd = rep(0,K-1),
  bTa = rep(0,K-1),
  bTb = rep(0,K-1),
  bTc = rep(0,K-1),
  bTd = rep(0,K-1),
  by2 = rep(0,K-1),
  by3 = rep(0,K-1),
  bTy2 = rep(0,K-1),
  bTy3 = rep(0,K-1)
)

n_chains_v <- 3
init_v <- list()
for ( i in 1:n_chains_v ) init_v[[i]] <- start_v

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_v <- stan ( model_code=model_code_v , data=dat_list_v , chains=n_chains_v , cores= n_chains_v , warmup=1000, iter=4000, init=init_v , control = list(adapt_delta = 0.95))
saveRDS(object = mfit_v, file = paste("./mfit_v_",whos,".rds", sep=""))
precis(mfit_v, depth = 2, prob = .96)
# plot(precis(mfit_v, depth = 2, prob = .96))

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_v_out <- precis(mfit_v, depth = 2, prob = .96)
# write.csv(mfit_v_out, file = "./mfit.v.csv")


#######################################################################################################
## MODEL COMPARISON USING WAIC
#######################################################################################################
models <- compare (mfit_i, mfit_ii, mfit_iii, mfit_v)
plot(models)

############################
# LINK FUNCTION FOR DIFFERENT MODELS
############################

source("./link_functions.R")
ordre <- c(5, 1, 2, 3, 4)

idx <- 0
for (yy in c(2013, 2014, 2015)){
  me <- c()
  mi <- c()
  ma <- c()
  for (m in 1:(K-1)){
    me_r <- c()
    mi_r <- c()
    ma_r <- c()
    v <- rep(0,(K-1))
    v[m] <- 1
    i <- link.ii(data = data.frame(sleep=v[1],p1=v[2],p2=v[3],p3=v[4], year=idx, time=rle(markchain$time)$values), post=extract.samples(mfit_ii), K=K)
    for(n in 1:length(rle(markchain$time)$values)){
      me_r <- rbind(me_r,colQuantiles(i[[n]], probs = 0.5)[ordre])
      mi_r <- rbind(mi_r,colQuantiles(i[[n]], probs = 0.25)[ordre])
      ma_r <- rbind(ma_r,colQuantiles(i[[n]], probs = 0.75)[ordre])
    }
    ma <- cbind(ma, ma_r)
    me <- cbind(me, me_r)
    mi <- cbind(mi, mi_r)
  }
  
  saveRDS(me, file=paste("./",whos,"_modularity-3_indepe_mean_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  saveRDS(mi, file=paste("./",whos,"_modularity-3_indepe_lower_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  saveRDS(ma, file=paste("./",whos,"_modularity-3_indepe_upper_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  idx <- idx+1
}

idx <- 0
for (yy in c(2013, 2014, 2015)){
  me <- c()
  mi <- c()
  ma <- c()
  for (m in 1:(K-1)){
    me_r <- c()
    mi_r <- c()
    ma_r <- c()
    v <- rep(0,(K-1))
    v[m] <- 1
    i <- link.iii(data = data.frame(sleep=v[1],p1=v[2],p2=v[3],p3=v[4], year=idx, time=rle(markchain$time)$values), post=extract.samples(mfit_iii), K=K)
    for(n in 1:length(rle(markchain$time)$values)){
      me_r <- rbind(me_r,colQuantiles(i[[n]], probs = 0.5)[ordre])
      mi_r <- rbind(mi_r,colQuantiles(i[[n]], probs = 0.25)[ordre])
      ma_r <- rbind(ma_r,colQuantiles(i[[n]], probs = 0.75)[ordre])
    }
    ma <- cbind(ma, ma_r)
    me <- cbind(me, me_r)
    mi <- cbind(mi, mi_r)
  }
  
  saveRDS(me, file=paste("./",whos,"_modularity-3_time_mean_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  saveRDS(mi, file=paste("./",whos,"_modularity-3_time_lower_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  saveRDS(ma, file=paste("./",whos,"_modularity-3_time_upper_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  idx <- idx+1
}


idx <- 0
for (yy in c(2013, 2014, 2015)){
  me <- c()
  mi <- c()
  ma <- c()
  for (m in 1:(K-1)){
    me_r <- c()
    mi_r <- c()
    ma_r <- c()
    v <- rep(0,(K-1))
    v[m] <- 1
    i <- link.v(data = data.frame(sleep=v[1],p1=v[2],p2=v[3],p3=v[4], year=idx, time=rle(markchain$time)$values), post=extract.samples(mfit_v), K=K)
    for(n in 1:length(rle(markchain$time)$values)){
      me_r <- rbind(me_r,colQuantiles(i[[n]], probs = 0.5)[ordre])
      mi_r <- rbind(mi_r,colQuantiles(i[[n]], probs = 0.25)[ordre])
      ma_r <- rbind(ma_r,colQuantiles(i[[n]], probs = 0.75)[ordre])
    }
    ma <- cbind(ma, ma_r)
    me <- cbind(me, me_r)
    mi <- cbind(mi, mi_r)
  }
  
  saveRDS(me, file=paste("./",whos,"_modularity-3_time2_mean_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  saveRDS(mi, file=paste("./",whos,"_modularity-3_time2_lower_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  saveRDS(ma, file=paste("./",whos,"_modularity-3_time2_upper_", as.character(yy),"_",ncol(ma),".rds",sep=""))
  idx <- idx+1
}


