############################################
#	           TODO         	   #
############################################

# Fundamental changes

# Parameters needed to start:
#	BREEDING_SEASON_DAYS
#       metabolic input and output for long and short trips (or net mass change)
# 	grow() function behavior (how does a chick accumulate mass. For more detail-- how does energy accumulation become tarsus and wing mass)
#	provision() function behavior + chickMeal lines in shortTrip() and longTrip() (how does parent mass/energy become chick mass/energy?)

############################################
#	         Logistics         	   #
############################################
library(tidyverse)

# SET OWN WORKING DIRECTORY *****
setwd("C://Users/Liam/Documents/Provisioning")

############################################
#	      Model Parameters         	   #
############################################

OUTPUT_FNAME	     <- "sims.txt"	# Output file storage
NUM_REPLICATES 	     <- 10		# Breeding season replicates
BREEDING_SEASON_DAYS <- 40 		# Length of a chick provisioning season (days)

BASE_MASS 	     <- 100		# Starting energy for parent

SHORT_METABOLISM     <- 10		# Daily metabolic cost during short trip
SHORT_MEAN 	     <- 20		# Mean of normal daily foraging intake during short trip
SHORT_SD 	     <- 5		# S.D. of normal daily foraging intake during short trip
MAX_SHORT_DAYS 	     <- 1		# Length of short trip (days) 
MAX_SHORT_TRIPS      <- 3		# Number of short trips within a cycle of short trips

LONG_METABOLISM      <- 8		# Daily metabolic cost during long trip
LONG_MEAN 	     <- 25		# Mean of normal daily foraging intake during long trip
LONG_SD 	     <- 3		# S.D. of normal daily foraging intake during long trip
MAX_LONG_DAYS 	     <- 3		# Length of long trip (days)
MAX_LONG_TRIPS 	     <- 1		# Number of long trips within a cycle of long trips

############################################
#	   Source Class Definitions        #
############################################

# The Parent.R script contains:
#	Class definition for Parent
# 	forage()	-- Calls either shortTrip or longTrip behavior
#	shortTrip()	-- Foraging behavior during a short trip
#	longTrip()	-- Foraging behavior during a long trip
#	provision()	-- Transfer of meal from parent to chick
source("Scripts/Parent.R")

# The Chick.R script contains:
# 	Class definition for Chick
#	grow() 		-- Assimilation of chick energy into morphological growth
source("Scripts/Chick.R")

############################################
#	    Breeding Season Model          #
############################################
breedingSeason <- function(ndays,
			   pf,
			   pm,
			   chick)
{
	for (day in 1:ndays) {
		ret <- forage(pf, chick)
		pf <- ret$parent
		chick <- ret$chick

		ret <- forage(pm, chick)
		pm <- ret$parent
		chick <- ret$chick

		chick <- grow(chick)
	}

	return(list(pf = pf, pm = pm, chick = chick))
}

############################################
#		 Run Model         	   #
############################################

# Initialize results storage.
# Right now we are storing one line for every replicate. Later on,
# we may need to store one line for every parameter set, etc.,
# depending on how large our simulation experiments scale
# NOTE we write to an output file and then read it back in to
# 	avoid storing really long vectors in dynamic memory,
#	which could cause issues as we add more test cases,
#	sensitivity analyses, etc., in the future
outFilename <- paste("Output/", OUTPUT_FNAME, sep="")
invisible(file.create(outFilename))

# The header line for the output csv file
# (i.e., the column names for the output dataframe)
outHeader <- paste("baseEnergy",		# Parent starting energy
		   "shortMetabolism",		# Daily metabolic cost during short trip
		   "shortMean",			# Mean of normal daily foraging intake during short trip
		   "shortSD",			# S.D. of normal daily foraging intake during short trip
		   "maxShortDays",		# Length of short trip (days)
		   "maxShortTrips",		# Number of short trips within a cycle of short trips
		   "longMetabolism",		# Daily metabolic cost during long trip
		   "longMean",			# Mean of normal daily foraging intake during long trip
		   "longSD",			# S.D. of normal daily foraging intake during long trip
		   "maxLongDays",		# Length of long trip (days)
		   "maxLongTrips",		# Number of long trips within a cycle of long trips
		   "parentEnergy_F",		# Finale FEMALE parent energy
		   "parentEnergy_M",		# Final MALE parent energy
		   "chickMass",			# Final chick mass (g)
		   "chickWing",			# Final chick wing length (mm)
		   "chickTarsus",		# Final chick tarsus length (mm)
		   sep = ",")

cat(text = outHeader, sep = "\n", file = outFilename, append = FALSE)

# NOTE if we want to access within-season results (such as track variance in parent energy levels),
#	we will need to either pass vectors within the forage functions,
#	or change to a full object-oriented approach with references.
# 	Right now everything is just the final result at the end of the season

for (replicate in 1:NUM_REPLICATES) {
	# Initialize parents and chick for this season
	# NOTE arbitrary starting trip types
	pf <- Parent(sex = "female",
		     tripType = "long")
	pm <- Parent(sex = "male",
		     tripType = "short")

	chick <- Chick()

	# Run the breedingSeason model
	ret <- breedingSeason(BREEDING_SEASON_DAYS,
			      pf,
			      pm,
			      chick)

	# Obtain seasonal results
	pf <- ret$pf
	pm <- ret$pm
	chick <- ret$chick

	# Compile output for the replicate to write to file
	# NOTE we access parameter values from the parents themselves,
	#      rather than from the constants defined at the top of the model,
	#      for when we want to test across different parameters. 
	#      Right now both parents are the same, so just taking one parameter
	#      value from the female parent.
	outLine <- paste(pf@baseEnergy,		# Parent starting energy
		  	 pf@shortMetabolism,	# Daily metabolic cost during short trip
		   	 pf@shortMean,		# Mean of normal daily foraging intake during short trip
		   	 pf@shortSD,		# S.D. of normal daily foraging intake during short trip
		   	 pf@maxShortDays,	# Length of short trip (days)
		   	 pf@maxShortTrips,	# Number of short trips within a cycle of short trips
		   	 pf@longMetabolism,	# Daily metabolic cost during long trip
		   	 pf@longMean,		# Mean of normal daily foraging intake during long trip
		   	 pf@longSD,		# S.D. of normal daily foraging intake during long trip
		   	 pf@maxLongDays,	# Length of long trip (days)
		   	 pf@maxLongTrips,	# Number of long trips within a cycle of long trips
		   	 pf@energy,		# Finale FEMALE parent energy
		   	 pm@energy,		# Final MALE parent energy
		   	 chick@mass,		# Final chick mass (g)
		   	 chick@wing,		# Final chick wing length (mm)
		   	 chick@tarsus,		# Final chick tarsus length (mm)
		   	 sep = ",")

	cat(text = outLine, sep = "\n", file = outFilename, append=TRUE)
}

# Read in file
results <- read_csv(outFilename, skip=0)
