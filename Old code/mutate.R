mutate <- function(players) {

	for (i in 1:nrow(players)) {

	player <- players[i,]

	rnd <- runif(2)

	if (player[22] == 0) {
		gene <- floor(2*rnd[1])
		is_evolve <- floor(2*rnd[2])
		player[(6 + 8*(2-is_evolve)*floor(gene/(2-is_evolve))):(13 + 8*(1-is_evolve) + 8*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + 8*(2-is_evolve)*floor(gene/(2-is_evolve))):(13 + 8*(1-is_evolve) + 8*(2-is_evolve)*floor(gene/(2-is_evolve)))]
		player[22] <- is_evolve
	}
	else if (player[22] == 1) {
		gene <- floor(4*rnd[1])
		is_evolve <- floor(2*rnd[2])
		player[(6 + 4*(2-is_evolve)*floor(gene/(2-is_evolve))):(9 + 4*(1-is_evolve) + 4*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + 4*(2-is_evolve)*floor(gene/(2-is_evolve))):(9 + 4*(1-is_evolve) + 4*(2-is_evolve)*floor(gene/(2-is_evolve)))]
		player[22] <- 1 + is_evolve
	}
	else if (player[22] == 2) {
		gene      <- floor(8*rnd[1])
		is_evolve <- floor(2*rnd[2])
		player[(6 + 2*(2-is_evolve)*floor(gene/(2-is_evolve))):(7 + 2*(1-is_evolve) + 2*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + 2*(2-is_evolve)*floor(gene/(2-is_evolve))):(7 + 2*(1-is_evolve) + 2*(2-is_evolve)*floor(gene/(2-is_evolve)))]
		player[22] <- 2 + is_evolve	
	}
	else if (player[22] == 3) {
		gene      <- floor(16*rnd[1])
		is_evolve <- floor(2*rnd[2])
		player[(6 + (2-is_evolve)*floor(gene/(2-is_evolve))):(7-is_evolve + (2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + (2-is_evolve)*floor(gene/(2-is_evolve))):(7-is_evolve + (2-is_evolve)*floor(gene/(2-is_evolve)))]
		player[22] <- 3 + is_evolve
	}
	else {
		gene      <- floor(16*rnd[1])
		player[6 + gene] <- 1 - player[6 + gene]
	}

	players[i,] <- player

	}

	return(players)
}