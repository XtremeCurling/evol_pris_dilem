play_own_history <- function(game_matrix,popn,powers_vec,correct_prob_mat) {

	rnd <- matrix(runif(4*popn),nrow=2*popn,ncol=2)

	is_false <- rnd > correct_prob_mat

	hist <- game_matrix[,2:3]

	observed_hist <- (hist-is_false)^2

	odds <- seq(1,(2*popn)-1,2)

	hist1 <- hist[odds,]
	hist2 <- hist[odds+1,]

	observed_hist1 <- observed_hist[odds,]
	observed_hist2 <- observed_hist[odds+1,]

	info1 <- cbind(observed_hist2,hist1)
	info2 <- cbind(observed_hist1,hist2)

	info <- matrix(0,nrow=2*popn,ncol=4)

	info[odds,] <- info1
	info[odds+1,] <- info2

	#print(info)
	#print(powers_vec)
	#print(t(info)*powers_vec)
	#print(colSums(t(info)*powers_vec))

	info_to_strat <- colSums(t(info)*powers_vec)

	#print(info_to_strat)

	strat_index <- cbind(1:(2*popn),info_to_strat+4)

	#print(strat_index)

	play <- game_matrix[strat_index]

	game_matrix[,2] <- play
	game_matrix[,3] <- hist[,1]

	play1 <- play[odds]
	play2 <- play[odds+1]

	death1 <- (play1*play2)*.4 + ((1-play1)*(1-play2))*.1 + (play1*(1-play2))*.05 + (play2*(1-play1))*.5
	death2  <- (play2*play1)*.4 + ((1-play2)*(1-play1))*.1 + (play2*(1-play1))*.05 + (play1*(1-play2))*.5

	death <- matrix(0,nrow=2*popn,ncol=1)
	death[odds,] <- death1
	death[odds+1,] <- death2

	game_matrix[,1] <- death

	return(game_matrix)

}