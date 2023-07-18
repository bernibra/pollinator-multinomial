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
          post$bc[,k] * data$p3[i] + 
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
        ptemp <- post$ba[,k] + 
          post$bb[,k] * data$p2[i] + 
          post$bc[,k] * data$p3[i] + 
          post$bd[,k] * data$sleep[i] +
          post$by2[,k] * as.numeric(data$year[i]==2) +
          post$by3[,k] * as.numeric(data$year[i]==3)
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
        ptemp <- post$ba[,k] + post$bTa[,k] * data$time[i] + 
          (post$bb[,k] + post$bTb[,k] * data$time[i]) * data$p2[i] + 
          (post$bc[,k] + post$bTc[,k] * data$time[i]) * data$p3[i] + 
          (post$bd[,k] + post$bTd[,k] * data$time[i]) * data$sleep[i] +
          post$by2[,k] * as.numeric(data$year[i]==2) +
          post$by3[,k] * as.numeric(data$year[i]==3)
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


link.v <- function( data , post, K=K) {
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
        ptemp <- post$ba[,k] + post$bTa[,k] * data$time[i] + 
          (post$bb[,k] + post$bTb[,k] * data$time[i]) * data$p2[i] + 
          (post$bc[,k] + post$bTc[,k] * data$time[i]) * data$p3[i] + 
          (post$bd[,k] + post$bTd[,k] * data$time[i]) * data$sleep[i] +
          (post$by2[,k] + post$bTy2[,k] * data$time[i]) * as.numeric(data$year[i]==2) +
          (post$by3[,k] + post$bTy3[,k] * data$time[i]) * as.numeric(data$year[i]==3)
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