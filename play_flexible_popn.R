play_flexible_popn <- function(game_matrix, Neven, powers_vec, alpha, beta,
                               gamma, R, e_DD, e_CD, e_CC, a_C, a_D) {
  # 2-period player histories
  hist <- game_matrix[, 2:3]
  # Odd indices
  odds  <- seq(1, Neven - 1, 2)
  # Odd- and even-indexed player histories
  hist_odd <- hist[odds, ]
  hist_even <- hist[(odds + 1), ]
  
  # Matrix with each player's information about her own and partner's histories
  info <- matrix(NA, nrow = Neven, ncol = 4)
  # Odd- and even-indexed player information
  info[odds, ] <- cbind(hist_even[, 1], hist_odd[, 1], hist_even[, 2], hist_odd[, 2])
  info[(odds + 1), ] <- cbind(hist_odd[, 1], hist_even[, 1], hist_odd[, 2], hist_even[, 2])
  
  # Player's information translated to their strategy for the time period
  #   The strategy is a number 0-15 that determines which strategy to use
  # Powers of 2
  powers_vec <- c(8, 4, 2, 1)
  # powers_vec <- 2^((ncol(info):1) - 1)
  info_to_strat <- colSums(t(info)*powers_vec)
  # Determine the strategy's index within the `game_matrix`
  strat_index <- cbind(1:Neven, info_to_strat + 4)
  
  # Determine each player's "play", i.e. whether they cooperate or defect 
  play <- game_matrix[strat_index]
  # Update the players' histories with their current plays
  game_matrix[, 3] <- game_matrix[, 2]
  game_matrix[, 2] <- play
  
  # Calculate effort
  # Odd- and even-indexed player plays
  play_odd <- play[odds]
  play_even <- play[odds + 1]
  # Total number of people in each of the 4 types of pairs
  n_CC <- 2*sum((1 - play_odd)*(1 - play_even))
  n_CD <- sum((1 - play_odd)*play_even) + sum(play_odd*(1 - play_even))
  n_DC <- n_CD
  n_DD <- 2*sum(play_odd*play_even)
  # Total population effort
  e_DC <- e_CD
  E <- n_CC*e_CC + n_CD*e_CD + n_DC*e_DC + n_DD*e_DD
  # Cumulative efforts exerted by DD, CD/DC, and CC pairs
  E_DD <- Neven*(e_DD/2)
  E_CD <- E_DD + (Neven - n_DD)*((e_CD - e_DD)/2)
  E_CC <- E
  
  # Calculate resource consumption
  # Individual resource consumption for each of the 4 types of pairs
  y_DD <- (1/Neven)*R*(1 - exp(-beta*E_DD))
  y_CD <- y_DD + (R/(Neven - n_DD))*(exp(-beta*E_DD) - exp(-beta*E_CD))
  y_DC <- y_CD
  y_CC <- y_CD + (R/n_CC)*(exp(-beta*E_CD) - exp(-beta*E_CC))
  # Figure out each specific player's consumption
  consumption <- rep(NA, Neven)
  consumption[odds] <- 
    y_DD*play_odd*play_even + 
    y_DC*play_odd*(1-play_even) + 
    y_CD*(1-play_odd)*play_even + 
    y_CC*(1-play_odd)*(1-play_even)
  # Partners consume the same amount
  consumption[odds + 1] <- consumption[odds]
  
  # Calculate each player's survival probability, based on consumption and
  # accident probability (as determined by effort)
  surv_prob <- (1 - exp(-gamma*consumption))*(1 - play*a_D - (1-play)*a_C)
  game_matrix[, 1] <- surv_prob
  
  # Return the game matrix and total effort level
  return(list(game_mat = game_matrix, E = E))
}