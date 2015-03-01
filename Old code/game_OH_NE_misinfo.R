game_OH_NE_misinfo <- function(N,T) {
	death_prob  <- rep(.5,2*N)
	hist        <- matrix((runif(2*N) > .5),2*N,2)
	strat       <- matrix((runif(32*N) > .5),nrow=2*N,ncol=16)
	misinfo     <- rep(.025,2*N)
	game_mat	<- cbind(death_prob,hist,strat,misinfo)
	
	powers_vec <- c(32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1)
	
	powers_vec1 <- c(8,4,2,1)

	stats       <- matrix(0,T/25,20)

	mutate_prob <- .1

	#player_types <- matrix(0,1,1)
	
	for (t in 1:T) {
		game_mat  <- game_mat[sample(2*N),]
		game_mat <- play_OH_NE_misinfo(game_mat,N,powers_vec1)
		
		if (t/25 == floor(t/25)) {
			stats[t/25,1] <- t
			stats[t/25,2] <- sum((1-game_mat[,2]))/(2*N)
			stats[t/25,3] <- num_alive/(2*N)
			stats[t/25,4] <- sum(.5*(2-game_mat[,20]))/(2*N)
			stats[t/25,5:20] <- colSums(game_mat[,4:19])/(2*N)
		}

		#if (t/100 == floor(t/100)) {	
			#player_types_now <- matrix(c(as.numeric(names(table(colSums(powers_vec*t(game_mat[,4:19]))))[table(colSums(powers_vec*t(game_mat[,4:19])))>99]),as.numeric(table(colSums(powers_vec*t(game_mat[,4:19])))[table(colSums(powers_vec*t(game_mat[,4:19])))>99])),ncol=2)

			#player_types_temp <- player_types
			#player_types <- matrix(0,nrow=length(unique(c(player_types_temp[,1],player_types_now[,1]))),ncol=1+(t/100))
			#player_types[1:nrow(player_types_temp),1:ncol(player_types_temp)] <- player_types_temp
			#player_types[,1] <- unique(c(player_types_temp[,1],player_types_now[,1]))
			#player_types[match(player_types_now[,1],player_types[,1]),1+(t/100)] <- player_types_now[,2]
		#}
		
		if (t/1000 == floor(t/1000)) {
			print(t)
			print(sum((1-game_mat[,2]))/(2*N))
			print(sum(.5*(2-game_mat[,20]))/(2*N))
			#print(table(colSums(powers_vec*t(game_mat[,4:19]))))
		}
		
		probs <- runif(2*N)
		
		is_alive  <- probs > game_mat[,1]
		is_mutate <- probs > 1 - mutate_prob
		
		num_alive  <- sum(is_alive)
		num_mutate <- sum(is_mutate)

		if (num_mutate > 0) {
			mutations <- mutate_OH_NE_misinfo(matrix(game_mat[is_mutate,],ncol=20))
			game_mat[1:num_alive,] <- game_mat[is_alive,]
			game_mat[(num_alive+1):(num_alive+num_mutate),] <- mutations
		}
		else {
			game_mat[1:num_alive,] <- game_mat[is_alive,]
		}

		births <- sample(1:num_alive, 2*N - num_alive - num_mutate, replace=TRUE)
		game_mat[(num_alive+num_mutate+1):(2*N),] <- game_mat[births,]

		rnd_info <- runif(2*N - num_alive)
		game_mat[(num_alive+1):(2*N),20] <- pmax(rep(0,2*N-num_alive),pmin(rep(1,2*N-num_alive),game_mat[(num_alive+1):(2*N),20] + (rnd_info > .75)*.0025 - (rnd_info < .25)*.0025))
	}
	
	print(game_mat)
	
	#print(stats)
	write.table(stats,file="C:\\Users\\E1RWS01\\Documents\\R\\Data OH NE Misinfo\\dataA1",sep=",")
	#write.table(player_types,file="C:\\Users\\E1RWS01\\Documents\\R\\Data OH NE Misinfo\\dataB1",sep=",")

}