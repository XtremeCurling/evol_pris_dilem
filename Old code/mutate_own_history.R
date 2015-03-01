mutate_own_history <- function(players) {

	for (i in 1:nrow(players)) {

	player <- players[i,]

	rnd <- runif(2)

	first_evol <- .25

	evol_prob <- c(first_evol,(first_evol/2),(first_evol/4),(first_evol/8),(first_evol/16))

	is_evolve <- rnd[2] > 1 - evol_prob[player[20]+1]

	is_devolve <- rnd[2] < first_evol/16

	if (player[20] == 0) {		
		gene <- floor(2*rnd[1])
		player[(4 + 8*(2-is_evolve)*floor(gene/(2-is_evolve))):(11 + 8*(1-is_evolve) + 8*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(4 + 8*(2-is_evolve)*floor(gene/(2-is_evolve))):(11 + 8*(1-is_evolve) + 8*(2-is_evolve)*floor(gene/(2-is_evolve)))]
		player[20] <- is_evolve
	}
	else if (player[20] == 1) {
		gene <- floor(4*rnd[1])
		if (is_devolve == 0) {
			player[(4 + 4*(2-is_evolve)*floor(gene/(2-is_evolve))):(7 + 4*(1-is_evolve) + 4*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(4 + 4*(2-is_evolve)*floor(gene/(2-is_evolve))):(7 + 4*(1-is_evolve) + 4*(2-is_evolve)*floor(gene/(2-is_evolve)))]
			player[20] <- 1 + is_evolve
		}
		else {
			player[4:19] <- matrix(player[4 + 4*gene],nrow=1,ncol=16)
			player[20] <- 0
		}
	}
	else if (player[20] == 2) {
		gene <- floor(8*rnd[1])
		if (is_devolve == 0) {
			player[(4 + 2*(2-is_evolve)*floor(gene/(2-is_evolve))):(5 + 2*(1-is_evolve) + 2*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(4 + 2*(2-is_evolve)*floor(gene/(2-is_evolve))):(5 + 2*(1-is_evolve) + 2*(2-is_evolve)*floor(gene/(2-is_evolve)))]
			player[20] <- 2 + is_evolve
		}
		else {
			player[4:11] <- matrix(player[4 + gene],nrow=1,ncol=8)
			player[12:19] <- matrix(player[12 + floor(8*runif(1))],nrow=1,ncol=8)
			player[20] <- 1
		}
	}
	else if (player[20] == 3) {
		if (is_devolve == 0) {
			gene <- floor(16*rnd[1])
			player[(4 + (2-is_evolve)*floor(gene/(2-is_evolve))):(5-is_evolve + (2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(4 + (2-is_evolve)*floor(gene/(2-is_evolve))):(5-is_evolve + (2-is_evolve)*floor(gene/(2-is_evolve)))]
			player[20] <- 3 + is_evolve
		}
		else {
			rnd2 <- runif(4)
			player[4:7] <- matrix(player[4 + floor(4*rnd2[1])],nrow=1,ncol=4)
			player[8:11] <- matrix(player[8 + floor(4*rnd2[2])],nrow=1,ncol=4)
			player[12:15] <- matrix(player[12 + floor(4*rnd2[3])],nrow=1,ncol=4)
			player[16:19] <- matrix(player[16 + floor(4*rnd2[4])],nrow=1,ncol=4)
			player[20] <- 2
		}
	}
	else {
		if (is_devolve == 0) {
			gene <- floor(16*rnd[1])
			player[4 + gene] <- 1 - player[4 + gene]
		}
		else {
			rnd2 <- runif(8)
			player[4:5] <- matrix(player[4 + floor(2*rnd2[1])],nrow=1,ncol=2)
			player[6:7] <- matrix(player[4 + floor(2*rnd2[2])],nrow=1,ncol=2)
			player[8:9] <- matrix(player[4 + floor(2*rnd2[3])],nrow=1,ncol=2)
			player[10:11] <- matrix(player[4 + floor(2*rnd2[4])],nrow=1,ncol=2)
			player[12:13] <- matrix(player[4 + floor(2*rnd2[5])],nrow=1,ncol=2)
			player[14:15] <- matrix(player[4 + floor(2*rnd2[6])],nrow=1,ncol=2)
			player[16:17] <- matrix(player[4 + floor(2*rnd2[7])],nrow=1,ncol=2)
			player[18:19] <- matrix(player[4 + floor(2*rnd2[8])],nrow=1,ncol=2)
			player[20] <- 3
		}
	}

	players[i,] <- player

	}

	return(players)
}