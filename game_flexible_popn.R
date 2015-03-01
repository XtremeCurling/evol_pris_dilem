game_flexible_popn <- function(T) {
  alpha <- 1
  beta <- 5e-3
  gamma <- 1e4
  e_D <- .5
  e_C <- 1
  extra_CC <- .25
  a_C <- .1
  a_D <- 1-(1-a_C)^(e_D/e_C)
  p_r <- .35
  p_m <- 20/(1*16000)
  R <- .5
  N_min <- 0
  N_max <- 100000
  N_guess <- 1000
  
  e_DD <- e_D
  e_CD <- (e_D+e_C)/2
  e_DC <- e_CD
  e_CC <- (2*e_C+extra_CC)/2
  
  while (N_max-N_min>1) {
    LHS <- 1-(1/alpha)*(1-exp(-beta*N_guess*e_CC))
    RHS <- -(N_guess/(gamma*(1-exp(-beta*N_guess*e_CC))))*log((p_r-a_c*(1+p_r))/((1-a_c)*(1+p_r)))
    if (RHS > LHS) {
      N_max <- N_guess
    }
    else {
      N_min <- N_guess
    }
    N_guess <- (N_max+N_min)/2
  }
  N <- ceiling(N_guess)
  print(N)
  #N <- 1000
  
  surv_prob <- rep(1,N)
  hist      <- matrix((runif(2*N) > .5),N,2)
  strat     <- matrix((runif(16*N) > .5),N,16)
  game_mat  <- cbind(surv_prob,hist,strat)
  
  powers_vec <- c(8,4,2,1)
  
  stat_interval <- 5
  stats <- matrix(0,floor(T/stat_interval),21)
  
  for (t in 1:T) {
    game_mat  <- game_mat[sample(N),]
    Neven <- 2*floor(N/2)
    play <- play_flexible_popn(game_mat[1:Neven,],Neven,powers_vec,alpha,beta,gamma,R,e_DD,e_CD,e_CC,a_C,a_D)
    game_mat[1:Neven,] <- play$game_mat
    E <- play$E
    
    if (t/stat_interval == floor(t/stat_interval)) {
      stats[t/stat_interval,1] <- t
      stats[t/stat_interval,2] <- sum((1-game_mat[,2]))/(Neven)
    }
    
    if (t/250 == floor(t/250)) {
      print(c(t,sum((1-game_mat[,2]))/Neven))
    }
    
    is_alive  <- runif(N) <= game_mat[,1]
    num_alive <- sum(is_alive)
    game_mat  <- game_mat[is_alive,]
    
    is_repr  <- runif(num_alive) <= p_r
    num_repr <- sum(is_repr)
    
    children  <- game_mat[is_repr,]
    is_mutate <- matrix(runif(16*num_repr),nrow=num_repr,ncol=16) <= p_m
    children[,4:19] <- (children[,4:19]-is_mutate)*(children[,4:19]-is_mutate)
    
    game_mat <- rbind(game_mat,children)
    
    N <- nrow(game_mat)
    
    R <- R*(1+alpha*(1-R)-(1-exp(-beta*E)))
    
    #print(c(t,N,R,mean(game_mat[,1])))
    
    if (t/stat_interval == floor(t/stat_interval)) {
      stats[t/stat_interval,3] <- N
      stats[t/stat_interval,4] <- R
      stats[t/stat_interval,5] <- mean(game_mat[,1])
      stats[t/stat_interval,6:21] <- colSums(game_mat[,4:19])/N
    }
  }
  
  return(stats)
  
}