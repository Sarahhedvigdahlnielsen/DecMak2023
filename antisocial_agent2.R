antisocial_agent <- function(rec_soc_puns, ntrials, groupSize, alpha, rho, omega) {
  
  
  # arrays to populate for simulation
  Ga <- rec_soc_puns
  Gb <- array(NA, c(groupSize, ntrials))
  p <- array(NA, c(groupSize, ntrials))
  c <- array(NA, c(groupSize, ntrials))
  
  for (s in 1:groupSize) {
    
    #--------------- Initial values ------------------------------------------------------
    
    
    #beliefs about others on first trial - gamma-poisson distribution
    temp_Gb <- rpois(1,alpha[s])
    Gb[s,1] <- ifelse(temp_Gb <= 30, temp_Gb, 30)
    
    # modelled preference and first contribution - see below
    p[s,1] <- (rho[s]*Gb[s,1])
    temp_c <- rpois(1, p[s,1])
    c[s,1] <- ifelse(temp_c <= 30, temp_c, 30)
    
  }
  
  #---------------    Generating antisocial punishments   -----------------------
  
  for (t in 2:ntrials) {
    
    for (s in 1:groupSize) {
      
      #- Belief about group contribution
      temp_Gb <- ((1-omega[s])*(Gb[s,t-1]))+(omega[s]*(Ga[s,t-1]))
      Gb[s,t] <- ifelse(temp_Gb <= 30, temp_Gb, 30)
      
      #- Contribution preference, given belief and matching preference rho  
      p[s,t] <- rho[s]*Gb[s,t]
      
      #- Contribution as discrete sample from preferences
      temp_c <- rpois(1,p[s,t])
      c[s,t] <- ifelse(temp_c <= 30, temp_c, 30)
      
      # Add the anti soc pun to the other subjects
      all_subs <- c(1,2,3,4)
      others <- all_subs[-s]
      
      for (o in others) {
        Ga[o,t] <- Ga[o,t] + round(c[s,t]/3)
      }
      
    }
    
  }
  
  result <- list(c=c,
                 Ga=Ga)
  
  return(result)
}