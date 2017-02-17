# Start by changing the working directory. This is the folder where the figures will be saved
setwd("C:/ADD DIRECTORY")

# Create a vector of the hydroperiods (from median length of 5 days to 12 days)
hp <- 5:12


###############################################################################################
#                                                                                             #
# Step 1: Plot the extinction probabilities using the minimum survival values from experiment #
#                                                                                             #
###############################################################################################

# Read in the values of the extinction probabilities for the three treatments
# See attached Excel spreadsheet for data

min.18 <- c(1,1,1,0.00366231,4.57962e-05,1.69911e-06,1.16022e-07,1.12143e-08)
min.pres <- c(1,1,1,0.646942695,0.006042285,0.000189508,1.19511e-05,1.10445e-06)
min.cc <- c(1,1,1,1,0.713259586,0.02520785,0.001829811,0.000195124)

# Make the plot and save it as a .png file in the working directory
png(filename="Minimums_ext_prob.png",width=12,height=12,units="cm",res=600)
plot(min.18~hp, xlab="Median hydroperiod (days)", ylab="Extinction probability", type="o",col="red",pch=16) 
points(min.pres~hp, col="blue",pch=16)
lines(min.pres~hp, col="blue")
points(min.cc~hp, col="green",pch=16)
lines(min.cc~hp, col="green")
legend("topright",leg=c(expression(18*degree~C),"Present temp", "Climate change"), lty=1,pch=16,bty="n", col=c("red","blue","green"))
dev.off()

######################################################################################################
#                                                                                                    #
# Step 2: Plot the extinction probabilities using the mean (average) survival values from experiment #
#                                                                                                    #
######################################################################################################

# Read in the values of the extinction probabilities for the three treatments
# See attached Excel spreadsheet for data

ave.18 <- c(1,1,0.046793153,0.000136478,1.95611e-06,7.78315e-08,5.42359e-09,5.25103e-10)
ave.pres <- c(1,1,1,0.047181462,0.000469699,1.5084e-05,9.43279e-07,8.54797e-08)
ave.cc <- c(1,1,1,1,0.123356075,0.003928578,0.000258539,2.514e-05)

# Make the plot and save it as a .png file in the working directory
png(filename="Averages_ext_prob.png",width=12,height=12,units="cm",res=600)
plot(ave.18~hp, xlab="Median hydroperiod (days)", ylab="Extinction probability", type="o",col="red",pch=16) 
points(ave.pres~hp, col="blue",pch=16)
lines(ave.pres~hp, col="blue")
points(ave.cc~hp, col="green",pch=16)
lines(ave.cc~hp, col="green")
legend("topright",leg=c(expression(18*degree~C),"Present temp", "Climate change"), lty=1,pch=16,bty="n", col=c("red","blue","green"))
dev.off()

##############################################################################################
#                                                                                            #
# Step 3: Plot the population growth rates using the minimum survival values from experiment #
#                                                                                            #
##############################################################################################

# Read in the values of the population growth rates for the three treatments
# See attached Excel spreadsheet for data

lambda.min.18 <- c(-0.0963022,-0.05588814,-0.002904589,0.0412845,0.08273489,0.1198064,0.1528645,0.1839569)
lambda.min.pres <- c(-0.1399937,-0.0897854,-0.04243549,0.0035328,0.04655616,0.08494926,0.1191319,0.1512168)
lambda.min.cc <- c(-0.2013558,-0.1456045,-0.0933873,-0.04303057,0.00384984,0.04546317,0.0823764,0.116858)

# Make the plot and save it as a .png file in the working directory
png(filename="Minimum_lambda.png",width=12,height=12,units="cm",res=600)
plot(lambda.min.18~hp, xlab="Median hydroperiod (days)", ylab= expression(paste("Population growth rate ( ",lambda," )")), type="o",col="red",pch=16,ylim=c(-0.2,0.2)) 
points(lambda.min.pres~hp, col="blue",pch=16)
lines(lambda.min.pres~hp, col="blue")
points(lambda.min.cc~hp, col="green",pch=16)
lines(lambda.min.cc~hp, col="green")
abline(h=0,lty=2)
legend("bottomright",leg=c(expression(18*degree~C),"Present temp", "Climate change"), lty=1,pch=16,bty="n", col=c("red","blue","green"))
dev.off()

#####################################################################################################
#                                                                                                   #
# Step 4: Plot the population growth rates using the average (mean) survival values from experiment #
#                                                                                                   #
#####################################################################################################

# Read in the values of the population growth rates for the three treatments
# See attached Excel spreadsheet for data

lambda.ave.18 <- c(-0.07241156,-0.02610713,0.01777455,0.06057971,0.1008026,0.1368392,0.1690149,0.1993262)
lambda.ave.pres <- c(-0.115828,-0.0673991,-0.02162571,0.02290865,0.06466474,0.1019935,0.1352709,0.1665567)
lambda.ave.cc <- c(-0.1743742,-0.1214575,-0.07172126,-0.02359435,0.0213301,0.06131392,0.09684699,0.1301194)

# Make the plot and save it as a .png file in the working directory
png(filename="Averages_lambda.png",width=12,height=12,units="cm",res=600)
plot(lambda.ave.18~hp, xlab="Median hydroperiod (days)", ylab= expression(paste("Population growth rate ( ",lambda," )")), type="o",col="red",pch=16,ylim=c(-0.2,0.2)) 
points(lambda.ave.pres~hp, col="blue",pch=16)
lines(lambda.ave.pres~hp, col="blue")
points(lambda.ave.cc~hp, col="green",pch=16)
lines(lambda.ave.cc~hp, col="green")
abline(h=0,lty=2)
legend("bottomright",leg=c(expression(18*degree~C),"Present temp", "Climate change"), lty=1,pch=16,bty="n", col=c("red","blue","green"))
dev.off()


#########################################################################################################












