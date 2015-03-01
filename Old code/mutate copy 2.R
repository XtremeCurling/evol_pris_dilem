mutate <- function(players) {

	for (i in 1:nrow(players)) {

	player <- players[i,]

	rnd <- runif(2)

	first_evol <- .25

	evol_prob <- c(first_evol,(first_evol/2),(first_evol/4),(first_evol/8),(first_evol/16))

	is_evolve <- rnd[2] > 1 - evol_prob[player[22]+1]

	if (player[22] == 0) {		
		gene <- floor(2*rnd[1])
		player[(6 + 8*(2-is_evolve)*floor(gene/(2-is_evolve))):(13 + 8*(1-is_evolve) + 8*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + 8*(2-is_evolve)*floor(gene/(2-is_evolve))):(13 + 8*(1-is_evolve) + 8*(2-is_evolve)*floor(gene/(2-is_evolve)))]
		player[22] <- is_evolve
	}
	else if (player[22] == 1) {
		anomalies <- abs(player[14]-player[13])
		is_devolve <- rnd[2] < (first_evol/(2^(2*anomalies)))
		gene <- floor(4*rnd[1])
		if (is_devolve == 0) {
			player[(6 + 4*(2-is_evolve)*floor(gene/(2-is_evolve))):(9 + 4*(1-is_evolve) + 4*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + 4*(2-is_evolve)*floor(gene/(2-is_evolve))):(9 + 4*(1-is_evolve) + 4*(2-is_evolve)*floor(gene/(2-is_evolve)))]
			player[22] <- 1 + is_evolve
		}
		else {
			player[6:21] <- matrix(player[6 + 4*gene],nrow=1,ncol=16)
			player[22] <- 0
		}
	}
	else if (player[22] == 2) {
		anomalies <- abs(player[10]-player[9]) + abs(player[18]-player[17])
		is_devolve <- rnd[2] < (first_evol/(2^(2*anomalies)))
		gene <- floor(8*rnd[1])
		if (is_devolve == 0) {
			player[(6 + 2*(2-is_evolve)*floor(gene/(2-is_evolve))):(7 + 2*(1-is_evolve) + 2*(2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + 2*(2-is_evolve)*floor(gene/(2-is_evolve))):(7 + 2*(1-is_evolve) + 2*(2-is_evolve)*floor(gene/(2-is_evolve)))]
			player[22] <- 2 + is_evolve
		}
		else {
			player[6:13] <- matrix(player[6 + gene],nrow=1,ncol=8)
			player[14:21] <- matrix(player[14 + floor(8*runif(1))],nrow=1,ncol=8)
			player[22] <- 1
		}
	}
	else if (player[22] == 3) {
		anomalies <- abs(player[8]-player[7]) + abs(player[12]-player[11]) + abs(player[16]-player[15]) + abs(player[20]-player[19])
		is_devolve <- rnd[2] < (first_evol/(2^(2*anomalies)))
		if (is_devolve == 0) {
			gene <- floor(16*rnd[1])
			player[(6 + (2-is_evolve)*floor(gene/(2-is_evolve))):(7-is_evolve + (2-is_evolve)*floor(gene/(2-is_evolve)))] <- 1 - player[(6 + (2-is_evolve)*floor(gene/(2-is_evolve))):(7-is_evolve + (2-is_evolve)*floor(gene/(2-is_evolve)))]
			player[22] <- 3 + is_evolve
		}
		else {
			rnd2 <- runif(4)
			player[6:9] <- matrix(player[6 + floor(4*rnd2[1])],nrow=1,ncol=4)
			player[10:13] <- matrix(player[10 + floor(4*rnd2[2])],nrow=1,ncol=4)
			player[14:17] <- matrix(player[14 + floor(4*rnd2[3])],nrow=1,ncol=4)
			player[18:21] <- matrix(player[18 + floor(4*rnd2[4])],nrow=1,ncol=4)
			player[22] <- 2
		}
	}
	else {
		anomalies <- abs(player[7]-player[6]) + abs(player[9]-player[8]) + abs(player[11]-player[10]) + abs(player[13]-player[12]) + abs(player[15]-player[14]) + abs(player[17]-player[16]) + abs(player[19]-player[18]) + abs(player[21]-player[20])
		is_devolve <- rnd[2] < (first_evol/(2^(2*anomalies)))
		if (is_devolve == 0) {
			gene <- floor(16*rnd[1])
			player[6 + gene] <- 1 - player[6 + gene]
		}
		else {
			rnd2 <- runif(8)
			player[6:7] <- matrix(player[6 + floor(2*rnd2[1])],nrow=1,ncol=2)
			player[8:9] <- matrix(player[6 + floor(2*rnd2[2])],nrow=1,ncol=2)
			player[10:11] <- matrix(player[6 + floor(2*rnd2[3])],nrow=1,ncol=2)
			player[12:13] <- matrix(player[6 + floor(2*rnd2[4])],nrow=1,ncol=2)
			player[14:15] <- matrix(player[6 + floor(2*rnd2[5])],nrow=1,ncol=2)
			player[16:17] <- matrix(player[6 + floor(2*rnd2[6])],nrow=1,ncol=2)
			player[18:19] <- matrix(player[6 + floor(2*rnd2[7])],nrow=1,ncol=2)
			player[20:21] <- matrix(player[6 + floor(2*rnd2[8])],nrow=1,ncol=2)
			player[22] <- 3
		}
	}

	players[i,] <- player

	}

	return(players)
}