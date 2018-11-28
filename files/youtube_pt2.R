'''
####################################



Simulating infection over the Dynamic Network Object in R

(Analyzing Gephi Dynamic network  Part 2)


Hospital_Contact


Young Joon Oh
https://youngjoon5.github.io
2018.11.20
####################################
'''



'''
ERGM ? 
STERGM ? 
EPIModel ?

'''




'''
######################################################################

From part 1
'''
library(EpiModel)

## loading data  or Load RData


load("net_data_epi.rda")

net
class(net)

list.vertex.attributes(net)
get.vertex.attribute(net,"label")



'''
##############################################################################################################

Updating Modules

http://statnet.github.io/gal/empnet.html

############################################################################################################

'''



'''
# update initialization
'''
net.init.mod <- function(x, param, init, control, s) {
  
  # Master Data List
  dat <- list()
  dat$param <- param
  dat$init <- init
  dat$control <- control
  
  dat$attr <- list()
  dat$stats <- list()
  dat$temp <- list()
  
  # Network Parameters
  dat$nw <- x
  dat$param$modes <- 1
  
  
  # Initialization
  
  ## Infection Status and Time Modules
  n <- network.size(dat$nw)
  dat$attr$status <- rep("s", n)
  dat$attr$status[sample(1:n, init$i.num)] <- "i"
  
  dat$attr$active <- rep(1, n)
  dat$attr$entrTime <- rep(1, n)
  dat$attr$exitTime <- rep(NA, n)
  
  dat$attr$infTime <- rep(NA, n)
  dat$attr$infTime[dat$attr$status == "i"] <- 1
  
  #--------------- --------------------------------Create dynamic attribute // for the infected at the initial step
  idsInf <- which(dat$attr$active == 1 & dat$attr$status == "i")
  dat$nw <- activate.vertex.attribute(dat$nw, "testatus", value = "i", onset = 1, terminus = Inf, v = idsInf) 
  
  
  ## Get initial prevalence
  dat <- get_prev.net(dat, at = 1)
  return(dat)
}






'''
# update infection
'''
my.inf.mod <- function(dat, at) {
  
  ## Variables ##
  active <- dat$attr$active
  status <- dat$attr$status
  
  inf.prob <- dat$param$inf.prob
  act.rate <- dat$param$act.rate
  
  nw <- dat$nw
  
  # Vector of infected and susceptible IDs
  idsSus <- which(active == 1 & status == "s")
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  nElig <- length(idsInf)
  
  
  # Initialize vectors
  nInf <- totInf <- 0
  
  ## Processes ##
  # If some infected AND some susceptible, then proceed
  if (nElig > 0 && nElig < nActive) {
    
    # Get discordant edgelist
    del <- discord_edgelist(dat, at)
    
    # If some discordant edges, then proceed
    if (!(is.null(del))) {
      
      # Infection probabilities
      del$transProb <- inf.prob
      
      # Act rates
      del$actRate <- act.rate
      
      # Calculate final transmission probability per timestep
      del$finalProb <- 1 - (1 - del$transProb) ^ del$actRate
      
      # Randomize transmissions and subset df
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      
      # Set new infections vector
      idsNewInf <- unique(del$sus)
      totInf <- length(idsNewInf) 
      
      
      
      # Update attributes
      if (totInf > 0) {
        
        dat$attr$status[idsNewInf] <- "i"
        dat$attr$infTime[idsNewInf] <- at 
        
        
        #---------------------------------------------------------update dynamic attribute // for the new infected
        activate.vertex.attribute(nw, "testatus", value = "i", onset = at, terminus = Inf, v = idsNewInf) 
      }
      
    } 
  } 
  
  ## Summary statistics ##
  if (at == 2) {
    dat$epi$si.flow <- c(0, totInf)
  } else {
    dat$epi$si.flow[at] <- totInf
  }
  
  dat$nw <- nw
  return(dat)
}





## Parameterization

inf_prob <- 0.1
ini.num <- 1


param <- param.net(inf.prob = inf_prob)  
init <- init.net(i.num = ini.num) 

control <- control.net(type = "SI", nsteps = 17377, nsims = 5,    # nsteps = 17377
                       initialize.FUN = net.init.mod, infection.FUN = my.inf.mod,
                       module.order = c("infection.FUN" , "get_prev.FUN"),  # 
                       skip.check = TRUE, save.nwstats = F, save.network = T)




## Simulation and Analysis

sim <- netsim(net, param, init, control) # DO not run

load("sim1_epi.rda")

par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, main = "Prevalence")
plot(sim, ylim=c(0, 0.03), y = "si.flow", main = "Incidence")




# 1min = 3 steps. 1 hour : 180 steps
#  TimE rearrange . from 1:pm, Monday to 2pm, Friday
par(mar = c(0,0,0,0), mfrow = c(2, 4))
plot(sim, type = "network", col.status = TRUE, at = 1, sims = 1, main="1pm. Mon") #  1pm. Mon
plot(sim, type = "network", col.status = TRUE, at = 2160*1, sims = 1, main="1 am. Tue") # 2160 = 12 hours, 1am. Tue
plot(sim, type = "network", col.status = TRUE, at = 2160*2, sims = 1, main="1 pm. Tue") # 1 pm. Tue
plot(sim, type = "network", col.status = TRUE, at = 2160*3, sims = 1, main="1 am. Wed") # 1 am. Wed
plot(sim, type = "network", col.status = TRUE, at = 2160*4, sims = 1, main="1 pm. Wed") # 1 pm. Wed
plot(sim, type = "network", col.status = TRUE, at = 2160*5, sims = 1, main="1 am. Thur") # 1 am. Thur
plot(sim, type = "network", col.status = TRUE, at = 2160*6, sims = 1, main="1 pm. Thur") # 1 pm. Thur
plot(sim, type = "network", col.status = TRUE, at = 2160*7, sims = 1, main="1 am. Fri") # 1 am. Fri.
par( mfrow = c(1, 1))





 '''
#####################################################################################################
####                                             Simulation by roles

ADM MED NUR PAT 
8  11  27  29

#####################################################################################################
'''


# update initialization

net.init.mod <- function(x, param, init, control, s) {
  
  # Master Data List
  dat <- list()
  dat$param <- param
  dat$init <- init
  dat$control <- control
  
  dat$attr <- list()
  dat$stats <- list()
  dat$temp <- list()
  
  # Network Parameters
  dat$nw <- x
  dat$param$modes <- 1
  
  #=========----------------------------------------- the initial infected by roles
  adm<-which(dat$nw%v%"label"=="ADM")
  med<-which(dat$nw%v%"label"=="MED")
  nur<-which(dat$nw%v%"label"=="NUR")
  pat<-which(dat$nw%v%"label"=="PAT")
  
  # Initialization
  
  ## Infection Status and Time Modules
  n <- network.size(dat$nw)
  dat$attr$status <- rep("s", n)
  
  
  #=========================--------the initial infected by roles  //  one by one
  dat$attr$status[sample(adm, init$i.num)] <- "i"    #### For ADM   
  #dat$attr$status[sample(med, init$i.num)] <- "i"    #### For med
  #dat$attr$status[sample(nur, init$i.num)] <- "i"    #### For nur
  #dat$attr$status[sample(pat, init$i.num)] <- "i"    #### For pat
  
  dat$attr$active <- rep(1, n)
  dat$attr$entrTime <- rep(1, n)
  dat$attr$exitTime <- rep(NA, n)
  
  dat$attr$infTime <- rep(NA, n)
  dat$attr$infTime[dat$attr$status == "i"] <- 1
  
  
  #
  idsInf <- which(dat$attr$active == 1 & dat$attr$status == "i")
  dat$nw <- activate.vertex.attribute(dat$nw, "testatus", value = "i", onset = 1, terminus = Inf, v = idsInf) 
  
  
  ## Get initial prevalence
  dat <- get_prev.net(dat, at = 1)
  return(dat)
}


# update infection
my.inf.mod <- function(dat, at) {
  
  ## Variables ##
  active <- dat$attr$active
  status <- dat$attr$status
  
  inf.prob <- dat$param$inf.prob
  act.rate <- dat$param$act.rate
  
  nw <- dat$nw
  
  # Vector of infected and susceptible IDs
  idsSus <- which(active == 1 & status == "s")
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  nElig <- length(idsInf)
  
  
  # Initialize vectors
  nInf <- totInf <- 0
  
  ## Processes ##
  # If some infected AND some susceptible, then proceed
  if (nElig > 0 && nElig < nActive) {
    
    # Get discordant edgelist
    del <- discord_edgelist(dat, at)
    
    # If some discordant edges, then proceed
    if (!(is.null(del))) {
      
      # Infection probabilities
      del$transProb <- inf.prob
      
      # Act rates
      del$actRate <- act.rate
      
      # Calculate final transmission probability per timestep
      del$finalProb <- 1 - (1 - del$transProb) ^ del$actRate
      
      # Randomize transmissions and subset df
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      
      # Set new infections vector
      idsNewInf <- unique(del$sus)
      totInf <- length(idsNewInf) 
      
      
      
      # Update attributes
      if (totInf > 0) {
        
        dat$attr$status[idsNewInf] <- "i"
        dat$attr$infTime[idsNewInf] <- at 
        
        
        # 
        activate.vertex.attribute(nw, "testatus", value = "i", onset = at, terminus = Inf, v = idsNewInf) 
      }
      
    } 
  } 
  
  ## Summary statistics ##
  if (at == 2) {
    dat$epi$si.flow <- c(0, totInf)
  } else {
    dat$epi$si.flow[at] <- totInf
  }
  
  dat$nw <- nw
  return(dat)
}



## Parameterization

inf_prob <- 0.1
ini.num <- 1

param <- param.net(inf.prob = inf_prob)
init <- init.net(i.num = ini.num)


control <- control.net(type = "SI", nsteps = 17377, nsims = 5,  # nsteps = 17377
                       initialize.FUN = net.init.mod, infection.FUN = my.inf.mod,
                       module.order = c("infection.FUN", "get_prev.FUN"),
                       skip.check = TRUE, save.nwstats = F, save.network = T)





'''
## Simulation and Analysis
'''


###------------------------------------------------------  ADM
sim_adm <- netsim(net, param, init, control) # Do nor run

load("sim_adm_epi.rda")

par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim_adm, main = "Prevalence")
plot(sim_adm, ylim=c(0, 0.03), y = "si.flow", main = "Incidence")




###------------------------------------------------------  MED
sim_med <- netsim(net, param, init, control)  # Do nor run

load("sim_med_epi.rda")

par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim_med, main = "Prevalence")
plot(sim_med, ylim=c(0, 0.03), y = "si.flow", main = "Incidence")



###------------------------------------------------------  NUR
sim_nur <- netsim(net, param, init, control)  # Do nor run

load("sim_nur_epi.rda")

par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim_nur, main = "Prevalence")
plot(sim_nur, ylim=c(0, 0.03), y = "si.flow", main = "Incidence")






###------------------------------------------------------  PAT
sim_pat <- netsim(net, param, init, control)  # Do nor run

load("sim_pat_epi.rda")


par(mfrow = c(1,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim_pat, main = "Prevalence")
plot(sim_pat, ylim=c(0, 0.03), y = "si.flow", main = "Incidence")






#----------plot ------------------------------------------------


par(mfrow = c(4,2), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim_adm, main = "Prevalence(ADM)")
plot(sim_adm, ylim=c(0, 0.03), y = "si.flow", main = "Incidence(ADM)")

plot(sim_med, main = "Prevalence(MED)")
plot(sim_med, ylim=c(0, 0.03), y = "si.flow", main = "Incidence(MED)")

plot(sim_nur, main = "Prevalence(NUR)")
plot(sim_nur, ylim=c(0, 0.03), y = "si.flow", main = "Incidence(NUR)")

plot(sim_pat, main = "Prevalence(PAT)")
plot(sim_pat, ylim=c(0, 0.03), y = "si.flow", main = "Incidence(PAT)")




