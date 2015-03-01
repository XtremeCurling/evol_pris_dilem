play <- function(game_matrix) {
	
	popn <- nrow(game_matrix)/2
	
	powers_vec <- c(8,4,2,1)
	
	for (i in 1:popn) {
		
		hist1   <- game_matrix[2*i-1,2:5]
		hist2   <- game_matrix[2*i,2:5]
		play1   <- game_matrix[2*i-1,6+sum(hist2*powers_vec)]
		play2   <- game_matrix[2*i,6+sum(hist1*powers_vec)]
		
		#game_matrix[(2*i-1):2*i,2:5] <- append(t(c(play1,hist1[1:3])), t(c(play2,hist2[1:3])))
		
		game_matrix[2*i-1,2:5] <- t(c(play1,hist1[1:3]))
		game_matrix[2*i,2:5]   <- t(c(play2,hist2[1:3]))
		
		death1  <- (play1*play2)*.3 + ((1-play1)*(1-play2))*.1 + (play1*(1-play2))*.05 + (play2*(1-play1))*.5
		death2  <- (play2*play1)*.3 + ((1-play2)*(1-play1))*.1 + (play2*(1-play1))*.05 + (play1*(1-play2))*.5
		
		game_matrix[2*i-1,1] <- death1
		game_matrix[2*i,1]   <- death2
	}
	
	return(game_matrix)
}