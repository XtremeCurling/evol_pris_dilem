mutate_OH_no_evol <- function(players) {

	for (i in 1:nrow(players)) {

	player <- players[i,]

	rnd <- runif(1)

	gene <- floor(16*rnd[1])

	player[4 + gene] <- 1 - player[4 + gene]

	players[i,] <- player

	}

	return(players)
}
