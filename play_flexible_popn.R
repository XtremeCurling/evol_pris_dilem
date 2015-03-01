play_flexible_popn <- function(game_matrix,Neven,powers_vec,alpha,beta,gamma,R,e_DD,e_CD,e_CC,a_C,a_D) {
  
  hist <- game_matrix[,2:3]
  
  odds  <- seq(1,Neven-1,2)
  
  hist1 <- hist[odds,]
  hist2 <- hist[(odds+1),]
 
  info1 <- cbind(hist2[,1],hist1[,1],hist2[,2],hist1[,2])
  info2 <- cbind(hist1[,1],hist2[,1],hist1[,2],hist2[,2])
  
  info <- matrix(NA,nrow=Neven,ncol=4)
  
  info[odds,]     <- info1
  info[(odds+1),] <- info2
  
  info_to_strat <- colSums(t(info)*powers_vec)
  
  strat_index <- cbind(1:Neven,info_to_strat+4)
  
  play <- game_matrix[strat_index]
  
  game_matrix[,3] <- game_matrix[,2]
  game_matrix[,2] <- play
  
  play1 <- play[odds]
  play2 <- play[odds+1]
  
  n_CC <- 2*sum((1-play1)*(1-play2))
  n_CD <- sum((1-play1)*play2)+sum(play1*(1-play2))
  n_DC <- n_CD
  n_DD <- 2*sum(play1*play2)
  
  e_DC <- e_CD
  
  E <- n_CC*e_CC + n_CD*e_CD + n_DC*e_DC + n_DD*e_DD
  
  y_DD <- (1/Neven)*R*(1-exp(-beta*Neven*e_DD))
  y_CD <- (1/(Neven-n_DD))*R*((1-exp(-beta*(Neven*n_DD+(Neven-n_DD)*(e_CD-e_DD))))-(n_DD/N)*(1-exp(-beta*Neven*e_DD)))
  y_DC <- y_CD
  y_CC <- (1/n_CC)*R*((1-exp(-beta*Neven*E))-((n_CD+n_DC)/(Neven-n_DD))*(1-exp(-beta*(Neven*e_DD+(N-n_DD)*(e_CD-e_DD))))-(n_CC/(Neven-n_DD))*(n_DD/Neven)*(1-exp(-beta*Neven*e_DD)))
  
  consumption <- rep(NA,Neven)
  consumption[odds]   <- y_DD*play1*play2 + y_DC*play1*(1-play2) + y_CD*(1-play1)*play2 + y_CC*(1-play1)*(1-play2)
  consumption[odds+1] <- consumption[odds]
  
  surv_prob <- (1-exp(-gamma*consumption))*(1-play*a_D-(1-play)*a_C)
  
  game_matrix[,1] <- surv_prob
  
  return(list(game_mat=game_matrix,E=E))
  
}