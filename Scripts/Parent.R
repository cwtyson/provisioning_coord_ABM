############################################
# 		Parent Class   		   #
############################################

# Parent class, which stores trip parameters and current parent energy
Parent <- setClass("Parent", 
	 	   slots=c(sex             = "character",	# Sex ("female" or "male")
	  	  	   tripType        = "character",	# Current trip type ("short" or "short")
	  	   	   baseEnergy	   = "numeric",		# Starting energy
	  	   	   energy          = "numeric",		# Current energy (starts at starting energy)
	  	   	   currDays        = "numeric",		# Current days on current trip
	  	   	   currTrips       = "numeric",		# Current trip in trip cycle
	  	   	   chickMeal       = "numeric",		# Stored energy to be transferred to chick
		   	   shortMetabolism = "numeric",		# Daily metabolic cost during short trip
		   	   shortMean       = "numeric",		# Mean of normal daily foraging intake during short trip
		   	   shortSD         = "numeric",		# S.D. of normal daily foraging intake during short trip
		   	   maxShortDays    = "numeric",		# Length of short trip (days)
		   	   maxShortTrips   = "numeric",		# Number of short trips within a cycle of short trips
		   	   longMetabolism  = "numeric",		# Daily metabolic cost during long trip
		   	   longMean        = "numeric",		# Mean of normal daily foraging intake during long trip
		   	   longSD          = "numeric",		# S.D. of normal daily foraging intake during long trip
		   	   maxLongDays     = "numeric",		# Length of long trip (days)
		   	   maxLongTrips    = "numeric"		# Number of long trips within a cycle of long trips
		  )
	 )

# Define "initialize" method for Parent.
# Sets default parameter values when new parent is constructed
setMethod("initialize", "Parent",
	  function(.Object, sex, tripType) {
	  	   .Object <- callNextMethod()
	  	   if (!(sex %in% c("female", "male"))) {
	  	   	stop("ERROR in parent initialization.
	  	   	     Options: 'female', 'male'")
	  	   } else if (!(tripType %in% c("short", "long"))) {
	  	   	stop("ERROR in parent initialization.
	  	   	      Options: 'short', 'long'")
	  	   } else {
	  	   	.Object@sex 		<- sex              	# Parent sex
	  	  	.Object@tripType        <- tripType		# Starting current trip type
	  	   	.Object@baseEnergy	<- BASE_ENERGY		# Starting energy
	  	   	.Object@energy          <- BASE_ENERGY		# Current energy (starts at starting energy)
	  	   	.Object@currDays        <- 0			# Current days on current trip
	  	   	.Object@currTrips       <- 0			# Current trip in trip cycle
	  	   	.Object@chickMeal       <- 0			# Stored energy to be transferred to chick
		   	.Object@shortMetabolism <- SHORT_METABOLISM	# Daily metabolic cost during short trip
		   	.Object@shortMean       <- SHORT_MEAN		# Mean of normal daily foraging intake during short trip
		   	.Object@shortSD         <- SHORT_SD		# S.D. of normal daily foraging intake during short trip
		   	.Object@maxShortDays    <- MAX_SHORT_DAYS	# Length of short trip (days)
		   	.Object@maxShortTrips   <- MAX_SHORT_TRIPS	# Number of short trips within a cycle of short trips
		   	.Object@longMetabolism  <- LONG_METABOLISM	# Daily metabolic cost during long trip
		   	.Object@longMean        <- LONG_MEAN		# Mean of normal daily foraging intake during long trip
		   	.Object@longSD          <- LONG_SD		# S.D. of normal daily foraging intake during long trip
		   	.Object@maxLongDays     <- MAX_LONG_DAYS	# Length of long trip (days)
		   	.Object@maxLongTrips    <- MAX_LONG_TRIPS	# Number of long trips within a cycle of long trips
		   	return(.Object)
	  	   }
	  	  
	 })


############################################
# 	       Parent Behavior             #
############################################

# Daily foraging routine for either kind of trip
forage <- function(parent, chick)
{
	if (parent@tripType == "short") {
		ret <- shortTrip(parent, chick)
	} else if (parent@tripType == "long") {
		ret <- longTrip(parent, chick)
	} else {
		stop("ERROR IN TRIP TYPE SPECIFICATION. STOPPING")
	}

	return(ret)
}

# Details of energy flux for a parent on a short trip
shortTrip <- function(parent, chick)
{
	# this is where we determine foraging intake values.
	# here we are just drawing a random value from a 
	# normal distribution with some characteristic mean and
	# standard deviation 
	intake <- rnorm(1, mean = parent@shortMean, sd = parent@shortSD)

	# add intake energy and remove daily metabolism
	parent@energy = parent@energy - 
			parent@shortMetabolism + 
			intake

	# Right now we just add one chick meal "token" for each day.
	# This is just for testing
	parent@chickMeal <- parent@chickMeal + 1

	# Count the current day
	parent@currDays <- parent@currDays + 1

	# is the trip over?
	if (parent@currDays == parent@maxShortDays) {

		# provision the chick when the trip is over
		ret <- provision(parent, chick)
		parent <- ret$parent
		chick <- ret$chick

		parent@currDays <- 0
		parent@currTrips <- parent@currTrips + 1

		# is the trip cycle over?
		if (parent@currTrips == parent@maxShortTrips) {
			parent@currTrips <- 0

			# switch trip types
			parent@tripType <- "long" 
		}
	}

	return(list(parent = parent, chick =chick))
}

# Details of energy flux for a parent on a long trip
longTrip <- function(parent, chick)
{
	# this is where we determine foraging intake values.
	# here we are just drawing a random value from a 
	# normal distribution with some characteristic mean and
	# standard deviation 
	intake <- rnorm(1, mean = parent@longMean, sd = parent@longSD)

	# add intake energy and remove daily metabolism
	parent@energy = parent@energy - 
			parent@longMetabolism + 
			intake

	# Right now we just add one chick meal "token" for each day.
	# This is just for testing
	parent@chickMeal <- parent@chickMeal + 1

	# Count the current day
	parent@currDays <- parent@currDays + 1	

	# is the trip over?
	if (parent@currDays == parent@maxLongDays) {

		# provision the chick when the trip is over
		ret <- provision(parent, chick)
		parent <- ret$parent
		chick <- ret$chick

		parent@currDays <- 0
		parent@currTrips <- parent@currTrips + 1

		# is the trip cycle over?
		if (parent@currTrips == parent@maxLongTrips) {
			parent@currTrips <- 0

			# switch trip types
			parent@tripType <- "short" 
		}
	}
	return(list(parent = parent, chick =chick))
}

# Parent feeding a chick
provision <- function(parent, chick)
{
	chick@energy <- chick@energy + parent@chickMeal
	parent@chickMeal <- 0

	return(list(parent = parent, chick = chick))
}

