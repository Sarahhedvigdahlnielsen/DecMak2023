install.packages("pacman")
pacman::p_load(extraDistr, R2jags, parallel, ggpubr, ggplot2, tidyverse)

set.seed(1998)
setwd("/work/SarahHedvigDahlNielsen#8292/Exam")

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

# Load data

fullDat <- read.csv('data/preprocessedDataNotRed.csv')
df <- fullDat %>% filter(groupid == 4802) %>% select(-c(sessionid, mgroupid, p, secseq, city, civic, ruleoflaw, female, age, numknown, singlechild, ageu21, urbanbackground, middleclass, membership, nation))


## Build environment

nsubs <- 4
groupSize <- nsubs
ntrials <- 10

rec_soc_puns <- array(NA, dim = c(nsubs, ntrials))

subIDs <- unique(df$subjectid)

# Create array of received social punishment [s,t]

for (t in 1:ntrials) {
  for (s in 1:nsubs) {
    rec_pun <- mean(df$recpun[df$subjectid == subIDs[s] & df$period == t])
    
    others <- subIDs[-s]
    others_anti_soc_puns <- array(NA, dim = 3)
    for (o in 1:3) {
      anti_soc_pun <- sum(df$antisocial[df$subjectid == others[o] & df$period == t])
      others_anti_soc_puns[o] <- anti_soc_pun
    }
    others_mean_anti <- mean(others_anti_soc_puns)
    rec_soc_pun <- ifelse(rec_pun >= others_mean_anti, round(rec_pun - others_mean_anti), 0)
    
    rec_soc_puns[s,t] <- rec_soc_pun
  }
}

# Import the agent
setwd("/work/SarahHedvigDahlNielsen#8292/Exam/Final exam code")
source("antisocial_agent2.R")

niterations <- 100
true_alpha <- array(0, dim = c(niterations, nsubs))
true_rho <- array(0, dim = c(niterations, nsubs))
true_omega <- array(0, dim = c(niterations, nsubs))

infer_alpha <- array(0, dim = c(niterations, nsubs))
infer_rho <- array(0, dim = c(niterations, nsubs))
infer_omega <- array(0, dim = c(niterations, nsubs))


# Run parameter recovery

for (i in 1:niterations) {
  
  alpha <- runif(4, 0.001, 20)
  rho <- runif(4, 0.001, 1)
  omega <- runif(4, 0, 1)
  
  antisocial_sims <- antisocial_agent(rec_soc_puns, ntrials, nsubs, alpha, rho, omega)
  
  Ga <- antisocial_sims$Ga
  c <- antisocial_sims$c
  
  data <- list("groupSize", "ntrials","c","Ga") 
  params <- c("alpha", "rho", "omega") 
  
  samples <- jags.parallel(data, inits=NULL, params,
                           model.file ="antisocial_pun.txt",
                           n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=3) # run group level jags on the data
  
  for(s in 1:4){
  
    true_alpha[i, s] <- alpha[s]
    true_rho[i, s] <- rho[s]
    true_omega[i, s] <- omega[s]
    
    Q <- samples$BUGSoutput$sims.list$alpha[,s]
    infer_alpha[i,s] <- MPD(Q)
    
    Q <- samples$BUGSoutput$sims.list$rho[,s]
    infer_rho[i,s] <- MPD(Q)
    
    Q <- samples$BUGSoutput$sims.list$omega[,s]
    infer_omega[i,s] <- MPD(Q)
  
  }
  
  print(i)
}

#flatten på true og infer arrays så de bliver 1 dimension
true_alpha_v <- as.vector(array(true_alpha, dim = niterations*nsubs))
true_rho_v <- as.vector(array(true_rho, dim = niterations*nsubs))
true_omega_v <- as.vector(array(true_omega, dim = niterations*nsubs))

infer_alpha_v <- as.vector(array(infer_alpha, dim = niterations*nsubs))
infer_rho_v <- as.vector(array(infer_rho, dim = niterations*nsubs))
infer_omega_v <- as.vector(array(infer_omega, dim = niterations*nsubs))

source("recov_plot.R")
pl1 <- recov_plot(true_alpha_v, infer_alpha_v, c("true alpha", "infer alpha"), "smoothed linear fit")
pl2 <- recov_plot(true_rho_v, infer_rho_v, c("true rho", "infer rho"), "smoothed linear fit")
pl3 <- recov_plot(true_omega_v, infer_omega_v, c("true omega", "infer omega"), "smoothed linear fit")

save(samples, file = "param_recov3.RData")

traceplot(samples)
samples