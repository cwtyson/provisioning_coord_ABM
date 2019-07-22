############################################
#	        Chick Class      	   #
############################################

# Chick class, which stores current growth values for chick
Chick <- setClass("Chick", 
	 	  slots=list(energy = "numeric",	# Current chick energy
	 	    	     mass   = "numeric",	# Mass (g)
	 	    	     tarsus = "numeric",	# Tarsus length (mm)
	 	    	     wing   = "numeric"		# Wing length (mm)
	 	  )
	 )

setMethod("initialize", "Chick",
	  function(.Object) {
	  	.Object <- callNextMethod()
	  	.Object@energy <- 0		# Current chick energy
	  	.Object@mass   <- 0		# Mass (g)
	  	.Object@tarsus <- 0		# Tarsus length (mm)
	  	.Object@wing   <- 0		# Wing length (mm)
	  	return(.Object)
	  })


############################################
#	        Chick Behavior   	   #
############################################

# Daily chick growth, incorporating energy into mrophological changes
grow <- function(chick) {
	chick@mass <- chick@mass + chick@energy
	chick@tarsus <- chick@tarsus + chick@energy
	chick@wing <- chick@wing + chick@energy 

	chick@energy <- 0

	return(chick)
}