###############################################################
#                                                             #
#       Part 1: Simulating rockpool hydroperiod               #
#                                                             #
###############################################################


# First set up the different environments in a matrix
# so that the sensitivity analyses are based on exactly the same 
# sequences of inundations.


ave.hp <- 5           # Median length of the hydroperiod (vary between 5 and 12)
replicates <- 1000    # Number of replicates
ts.length <- 1100     # Set desired length of time series

# Set up hydorperiod matrix by drawing hydroperiod from a log-normal distribution.
env.mat <- matrix(NA,ncol=ts.length,nrow=replicates)
for (i in 1:replicates){
env.mat[i,] <- (rlnorm(ts.length,meanlog=log(ave.hp)))
}


###############################################################
#                                                             #
#       Part 2: Running the matrix population model           #
#                                                             #
###############################################################

# First enter all the free life-history parameters

# To run the sensitivity analysis, sequentially adjust these parameters WITHOUT
# simulating a new set of hydrological regimes. This ensures that the sensitivity 
# analysis is based on EXACTLY the same hydroperiods.

Es0 <- 0.88125        # survival of freshly produced eggs
Eh0 <- 0.47           # proportion of freshly produced eggs that hatch
Es1 <- 0.83225        # survival of eggs in the egg bank
Eh1 <- 0.09           # proportion of dormant eggs that hatch
As <- 0.74            # Survival rate of adults in the active population
Rep <-  13            # Reproduction rate (fecundity): number of eggs per pair
maturation.age <- 6   # This is the age, in days, at which individuals produce first offspring


# Set the starting nmber of individuals in each life-stage of the population
# ----------------------------------------
N0 <- 0               # Starting number of generation 1 eggs (Freshly hatched eggs)
N1 <- 25000           # Starting number of generation >2 eggs (egg bank)
N2 <- 0               # Starting number of adults



# Matrix population model
# ------------------------------------
# This is the matrix that describes population growth in instances where
# the hydorperiod is shorter than the maturation age. It assumes that eggs hatch as they always do
# (i.e. no hatching cues other than rain), but that hatchlings do not have enough
# time to reach maturity an reproduce successfullly.

mat.dry <- matrix(c(0, 0, 0,
		              Es0*(1-Eh0),Es1*(1-Eh1),0,
		              Es0*Eh0,Es1*Eh1,0), nrow=3,byrow=TRUE)


# The next command creates a blank temporary matrix with the three life stages
# (e.g. Newly hatched eggs, egg bank and adult) as columns and the numbers of
# inundations as rows.
temp.nr <- matrix(c(rep(NA, ts.length*3)),nrow=ts.length,ncol=3)
temp.nr[1,1:3] <- c(N0,N1,N2)	# Adds starting values for population vector

# This is a blank vector, which will be used to place the growth rates (lambdaS)
# for each of the replicates
lambdaS <- rep(NA,replicates)
# This is a blank vector, which will be used to place the variation in population changes
# for each of the replicates
Var <- rep(NA,replicates)

# This is the loop which runs the models for the number of replicates
for (k in 1:replicates) {
  environment <- env.mat[k,]
    for (i in 1:(ts.length-1)) {
      # If the length of the inundation is longer than the matruation age, then reproduction is successful
      if(environment[i] >= maturation.age){
        Rain <- environment[i]
        # This is the matrix that represents population dynamics when the length of hydroperiod
        # exceeeds the time required to reach maturation and produce offspring
        mat.rain <- matrix(c(0, 0, ((Rep*(sum(As^(1:Rain)))) - (Rep*(sum(As^(1:maturation.age))))),
                          Es0*(1-Eh0),Es1*(1-Eh1),0,
		                      Es0*Eh0,Es1*Eh1,0),nrow=3,byrow=TRUE)
        temp.nr[i+1,1:3] <- mat.rain%*%temp.nr[i,1:3]
      } else { # If the length of the inundation is shorter than the matruation age, then reproduction is unsuccessful
        temp.nr[i+1,1:3] <- mat.dry%*%temp.nr[i,1:3]
      }
      }

    # This loop summarises the change in egg bank size over each successive time-step
    ratioN <- rep(NA,ts.length-1)
      for (i in 1:ts.length-1) {
        ratioN[i] <- log((temp.nr[i+1,2])/(temp.nr[i,2]))
      }
  
  # Population growth rate [log(lambda)]: See supplementary appendix 
  # Growth rates are only estimated after a burn-in period of 100 time-steps and then is the average of the rest of the time series.
  lambdaS[k] <- (sum(ratioN[100:ts.length-1])/length(ratioN[100:ts.length-1]))
  #  Variation in the changes in population between successive time-steps
  Var[k] <- var(ratioN)
}

# Summarise the population growth rates and variation in population by avergaing across all replicates
mean(lambdaS,na.rm=T)
hist(lambdaS)
mean(Var,na.rm=T)

# These values can be saved in a spreadsheet to calculate the sensitivities and extinction risk using basic arithmatic.
# See main manuscript and appendices for more details.