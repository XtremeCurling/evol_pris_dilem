mutate_OH_NE_misinfo <- function(players) {

	rnd <- runif(nrow(players))

	gene <- floor(16*rnd)

	player_index <- 1:(nrow(players))

	players[cbind(player_index,4+gene)] <- 1 - players[cbind(player_index,4+gene)]

	#for (i in 1:nrow(players)) {

	#player <- players[i,]

	#rnd <- runif(1)

	#gene <- floor(16*rnd[1])

	#player[4 + gene] <- 1 - player[4 + gene]

	#players[i,] <- player

	#}

	return(players)
}
