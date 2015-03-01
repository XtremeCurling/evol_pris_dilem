game_own_history <- function(N,T) {
	death_prob  <- rep(.5,2*N)
	#hist        <- matrix((runif(4*N) > 1),nrow=2*N,ncol=2)
	#strats1     <- runif(2*N) > 1
	#strats2     <- runif(2*N) > 1
	#strats3     <- runif(2*N) > 0
	#strats4     <- runif(2*N) > 0
	#strat1      <- matrix(strats1,nrow=2*N,ncol=4)
	#strat2      <- matrix(strats2,nrow=2*N,ncol=4)
	#strat3      <- matrix(strats3,nrow=2*N,ncol=4)
	#strat4      <- matrix(strats4,nrow=2*N,ncol=4)
	#periods     <- rep(2,2*N)
	#game_mat    <- cbind(death_prob,hist,cbind(strat1,strat2,strat3,strat4),periods)
	

	hist        <- matrix((runif(2*N) > .5),2*N,2)
	strat       <- matrix((runif(32*N) > .5),nrow=2*N,ncol=16)
	periods	<- rep(4,2*N)
	game_mat	<- cbind(death_prob,hist,strat,periods)

	#hist0       <- matrix(0,N,2)
	#hist1 	<- matrix(1,N,2)
	#strat0	<- matrix(0,N,16)
	#strat1	<- matrix(1,N,16)
	#periods	<- rep(0,2*N)
	#game_mat	<- cbind(death_prob,rbind(hist0,hist1),rbind(strat0,strat1),periods)
	
	powers_vec <- c(32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1)
	powers_vec1 <- c(8,4,2,1)
	
	misinfo_prob <- .05
	correct_prob <- c((.5 + ((1-misinfo_prob)/2)),(.5 + (((1-misinfo_prob)^2)/2)))
	correct_prob_mat <- matrix(correct_prob,nrow=2*N,ncol=2,byrow=TRUE)

	stats       <- matrix(0,T/10,20)

	mutate_prob <- .01

	#player_types <- matrix(0,1,1)
	
	for (t in 1:T) {
		#mutate_prob <- 1/(100*(t^(1/3)))
		game_mat  <- game_mat[sample(2*N),]
		game_mat <- play_own_history(game_mat,N,powers_vec1,correct_prob_mat)
		
		if (t/10 == floor(t/10)) {
			stats[t/10,1] <- t
			stats[t/10,2] <- sum((1-game_mat[,2]))/(2*N)
			stats[t/10,3] <- num_alive/(2*N)
			stats[t/10,4] <- sum(game_mat[,20]/(8*N))
			stats[t/10,5:20] <- colSums(game_mat[,4:19])/(2*N)

			#player_types_now <- matrix(c(as.numeric(names(table(colSums(powers_vec*t(game_mat[,4:19]))))[table(colSums(powers_vec*t(game_mat[,4:19])))>24]),as.numeric(table(colSums(powers_vec*t(game_mat[,4:19])))[table(colSums(powers_vec*t(game_mat[,4:19])))>24])),ncol=2)

			#player_types_temp <- player_types
			#player_types <- matrix(0,nrow=length(unique(c(player_types_temp[,1],player_types_now[,1]))),ncol=1+(t/50))
			#player_types[1:nrow(player_types_temp),1:ncol(player_types_temp)] <- player_types_temp
			#player_types[,1] <- unique(c(player_types_temp[,1],player_types_now[,1]))
			#player_types[match(player_types_now[,1],player_types[,1]),1+(t/50)] <- player_types_now[,2]

			# stats[t/4,3:19] <- colSums(game_mat[,4:19])/(2*N)
			# print(sum(game_mat[,2])/(2*N))
			# print(table(colSums(powers_vec*t(game_mat[,4:19]))))	
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
			mutations <- mutate_own_history(matrix(game_mat[is_mutate,],ncol=20))
			game_mat[1:num_alive,] <- game_mat[is_alive,]
			game_mat[(num_alive+1):(num_alive+num_mutate),] <- mutations
		}
		else {
			game_mat[1:num_alive,] <- game_mat[is_alive,]
		}

		#game_mat <- rbind(game_mat[is_alive,],mutations)

		#game_mat <- rbind(game_mat,mutate(game_mat[is_mutate,]))

		births <- sample(1:num_alive, 2*N - num_alive - num_mutate, replace=TRUE)
		game_mat[(num_alive+num_mutate+1):(2*N),] <- game_mat[births,]
		
		#game_mat <- rbind(game_mat,game_mat[births,])
		
		# stats[t,1] <- t
		# stats[t,2] <- sum(game_mat[,2])/(2*N)
		
		# if (t/100 == floor(t/100)) {
			# print(t)
			# print(table(game_mat[,1]))
		# }
	}
	
	#print(2*N-num_alive)
	#print(game_mat)
	
	#print(stats)
	#write.table(stats,file="C:\\Users\\E1RWS01\\Documents\\R\\Data Own History\\data2",sep=",")
	#write.table(player_types,file="C:\\Users\\E1RWS01\\Documents\\R\\Data Own History\\dataB1",sep=",")
}