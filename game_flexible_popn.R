# Run the simulation for T time periods
game_flexible_popn <- function(T) {
  ## Set the parameters
  # Function parameters
  alpha <- 1
  beta <- 5e-3
  gamma <- 4e4
  # Individual effort levels
  e_D <- .5
  e_C <- 1
  # Bonus effort from successful cooperation
  extra_CC <- .25
  # Collective effort levels
  e_DD <- e_D
  e_CD <- (e_D+e_C)/2
  e_DC <- e_CD
  e_CC <- (2*e_C+extra_CC)/2
  # Accident probabilities
  a_C <- .3
  a_D <- 1 - (1-a_C)^(e_D/e_C)
  # Reproduction probability
  #   Conditional on surviving to the next time period
  p_r <- .8
  # Mutation probability
  p_m <- 20/(1*16000)
  # Initial resource amount
  R <- .5
  # Initial intervals and guess for the steady state population size
  N_min <- 0
  N_max <- 100000
  N_guess <- 1000
  # Determines the intervals by which game statistics are recorded
  #   I.e. stats_interval = 5 means stats will be saved for every 5th time period
  stat_interval <- 5
  
  # Numerically solve for the steady state population size under full cooperation.
  # The solution will be used as the initial population size.
  # The LHS and RHS come from a system of equations that was worked out on paper.
  while (N_max - N_min > 1) {
    LHS <- 1 - (1/alpha)*(1 - exp(-beta*N_guess*e_CC))
    RHS <- -(N_guess/(gamma*(1 - exp(-beta*N_guess*e_CC))))*log((p_r - a_C*(1+p_r))/((1-a_C)*(1+p_r)))
    if (RHS > LHS) {
      N_max <- N_guess
    }
    else {
      N_min <- N_guess
    }
    N_guess <- (N_max + N_min)/2
  }
  N <- ceiling(N_guess)
  print(N)
  
  # Initial survival probabilites (all 1)
  surv_prob <- rep(1, N)
  # Initial player histories (random binomial, length 2)
  hist <- matrix((runif(2*N) > .5), nrow = N, ncol = 2)
  # Initial player strategies i.e. genes (random binomial, length 16)
  #   There are 16 different possible 2-period histories between 2 players
  strat <- matrix((runif(16*N) > .5), nrow = N, ncol = 16)
  # Combine into the "game matrix"
  game_mat <- cbind(surv_prob, hist, strat)
  
  # Initiate the matrix in which simulation statistics will be saved
  stats <- matrix(0, nrow = floor(T/stat_interval), ncol = 21)
  
  for (t in 1:T) {
    # Randomly sort the players into pairs
    game_mat <- game_mat[sample(N), ]
    # Neven is either N if N even, or N - 1 if N odd.
    # This ensures that all players are paired in play_flexible_popn().
    Neven <- 2*floor(N/2)
    # "Play" the round; i.e. allow all people to engage in resource collection
    #   with their partners, either cooperating (putting in high effort) or
    #   defecting (putting in low effort)
    play <- play_flexible_popn(game_mat[1:Neven, ], Neven, powers_vec, alpha,
                               beta, gamma, R, e_DD, e_CD, e_CC, a_C, a_D)
    game_mat[1:Neven, ] <- play$game_mat
    E <- play$E
    
    # Every `stat_interval` periods, record the population's statistics
    if (t/stat_interval == floor(t/stat_interval)) {
      stats[t/stat_interval, 1] <- t
      # Percent of population that cooperated in time period `t`
      stats[t/stat_interval, 2] <- sum((1 - game_mat[,2]))/Neven
    }
    
    # Every 250 periods, print the time period and the percent of the population
    # that cooperated.
    if (t/250 == floor(t/250)) {
      print(c(t, sum((1 - game_mat[,2]))/Neven))
    }
    
    # Determine which players survive to time period `t + 1`
    is_alive <- runif(N) <= game_mat[, 1]
    num_alive <- sum(is_alive)
    game_mat  <- game_mat[is_alive, ]
    
    # Determine which players reproduce
    is_repr <- runif(num_alive) <= p_r
    num_repr <- sum(is_repr)
    
    # Children of reproducing players, possibly with mutated strategy genes
    children <- game_mat[is_repr, ]
    is_mutate <- matrix(runif(16*num_repr), nrow = num_repr, ncol = 16) <= p_m
    children[, 4:19] <- (children[, 4:19] - is_mutate)*(children[, 4:19] - is_mutate)
    game_mat <- rbind(game_mat, children)
    
    # New population size
    N <- nrow(game_mat)
    
    # New resource amount, based on total time period `t` effort
    R <- R*(1 + alpha*(1-R) - (1 - exp(-beta*E)))
    
    # Every `stat_interval` periods, record the population's statistics
    if (t/stat_interval == floor(t/stat_interval)) {
      # Population size after period `t` deaths and births
      stats[t/stat_interval, 3] <- N
      # Resource amount of period `t` consumption and resource growth 
      stats[t/stat_interval, 4] <- R
      # Mean survival probability (including children, whose values will
      #   be equal to those of their parents in period `t`)
      stats[t/stat_interval, 5] <- mean(game_mat[, 1])
      # Mean of defections in each of the 16 strategy genes
      stats[t/stat_interval, 6:21] <- colSums(game_mat[, 4:19])/N
    }
  }
  
  print(game_mat[1:10, ])
  # Return the game statistics
  return(stats)
}