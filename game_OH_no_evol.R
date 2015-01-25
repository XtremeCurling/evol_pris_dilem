game_OH_no_evol <- function(N,T) {
	death_prob  <- rep(.5,2*N)
	hist        <- matrix((runif(2*N) > .5),2*N,2)
	strat       <- matrix((runif(32*N) > .5),nrow=2*N,ncol=16)
	game_mat	<- cbind(death_prob,hist,strat)
	
	powers_vec <- c(32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1)
	
	powers_vec1 <- c(8,4,2,1)
	
	misinfo_prob <- .05
	correct_prob <- c((.5 + ((1-misinfo_prob)/2)),(.5 + (((1-misinfo_prob)^2)/2)))
	correct_prob_mat <- matrix(correct_prob,nrow=2*N,ncol=2,byrow=TRUE)

	stats       <- matrix(0,T/50,19)

	mutate_prob <- .01

	player_types <- matrix(0,1,1)
	
	for (t in 1:T) {
		game_mat  <- game_mat[sample(2*N),]
		game_mat <- play_own_history(game_mat,N,powers_vec1,correct_prob_mat)
		
		if (t/50 == floor(t/50)) {
			stats[t/50,1] <- t
			stats[t/50,2] <- sum((1-game_mat[,2]))/(2*N)
			stats[t/50,3] <- num_alive/(2*N)
			stats[t/50,4:19] <- colSums(game_mat[,4:19])/(2*N)
			
			player_types_now <- matrix(c(as.numeric(names(table(colSums(powers_vec*t(game_mat[,4:19]))))[table(colSums(powers_vec*t(game_mat[,4:19])))>9]),as.numeric(table(colSums(powers_vec*t(game_mat[,4:19])))[table(colSums(powers_vec*t(game_mat[,4:19])))>9])),ncol=2)

			player_types_temp <- player_types
			player_types <- matrix(0,nrow=length(unique(c(player_types_temp[,1],player_types_now[,1]))),ncol=1+(t/50))
			player_types[1:nrow(player_types_temp),1:ncol(player_types_temp)] <- player_types_temp
			player_types[,1] <- unique(c(player_types_temp[,1],player_types_now[,1]))
			player_types[match(player_types_now[,1],player_types[,1]),1+(t/50)] <- player_types_now[,2]
		}
		
		if (t/1000 == floor(t/1000)) {
			print(t)
			print(sum((1-game_mat[,2]))/(2*N))
			print(table(colSums(powers_vec*t(game_mat[,4:19]))))
		}
		
		probs <- runif(2*N)
		
		is_alive  <- probs > game_mat[,1]
		is_mutate <- probs > 1 - mutate_prob
		
		num_alive  <- sum(is_alive)
		num_mutate <- sum(is_mutate)

		if (num_mutate > 0) {
			mutations <- mutate_OH_no_evol(matrix(game_mat[is_mutate,],ncol=19))
			game_mat[1:num_alive,] <- game_mat[is_alive,]
			game_mat[(num_alive+1):(num_alive+num_mutate),] <- mutations
		}
		else {
			game_mat[1:num_alive,] <- game_mat[is_alive,]
		}

		births <- sample(1:num_alive, 2*N - num_alive - num_mutate, replace=TRUE)
		game_mat[(num_alive+num_mutate+1):(2*N),] <- game_mat[births,]
	}
	
	print(game_mat)
	
	#print(stats)
	write.table(stats,file="C:\\Users\\E1RWS01\\Documents\\R\\Data OH No Evol\\dataA3",sep=",")
	write.table(player_types,file="C:\\Users\\E1RWS01\\Documents\\R\\Data OH No Evol\\dataB3",sep=",")

}