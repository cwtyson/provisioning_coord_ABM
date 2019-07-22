## Necessary inputs
## p.mass = parent mass (g)
## ad.speed = adult speed (m/s)
## bmr = basal metabolic rate in J per min per kg
forage <- function(parent){
  
  ## If parent's mass is below threshold, go on a short trip:
  trip.type <- ifelse(p.mass < min.threshold, trip.type = "ST", trip.type = "LT")
  
  ## We will assume two different foraging zones. 
  ## One less productive, but closer and used during short trips. 
  ## The other, farther, but more productive and used during long trips.
  ## Amount gained each day should be a function of how:
  ## 1) how productive the area is, 2) how far the individual has traveled, and 3) how successful the individual is while foraging.
  
  ## If short trip, daily energy gain will be lower (less productive) and the average daily commuting cost will be higher (must fly there and back)
  if(trip.type == "st"){
    
    ## Calculate flight cost per minute on the outbound trip. The primary variable affecting this will be the bird's mass. 
    ## The flight cost (in minutes) will be multiplied by the distance to the foraging ground over speed. 
    ## So if the foraging ground is 100 km away and the bird flies at 50 km an hour, then the flight cost would be multiplied by 120 minutes.
    flight.cost.pmin.outbound <- flight.cost(body.mass = p.mass, 
                                             speed = ad.speed,
                                             food.carried = ((f.gut.cont)/1000),
                                             bmr.w = ((f.BMR/60)*(female.res[pair,day]/1000)))*60
    
    ## Adust cost based on distance and speed
    flight.cost.pmin.outbound <- flight.cost.pmin.outbound * (f.dist/ad.speed)
    
    ## Calculate foraging gains
    CPUE<-10.24*((PREY.DENS^24.07)/((PREY.DENS^24.07)+(0.34^24.07)))  #determining the foraging efficiency from the input prey density.
    
    
    ## Subtract flight cost and add foraging gains to parent mass
    p.mass <- p.mass - flight.cost.pmin.outbound + CPUE
    
    ## Reassess adults 'condition'. While it is below a threshold value, continue to forage at the ST zone.
    while(p.mass < min.threshold){
      
      ## Continue to forage.
      CPUE<-10.24*((PREY.DENS^24.07)/((PREY.DENS^24.07)+(0.34^24.07)))  #determining the foraging efficiency from the input prey density.
      
      ##  No longer paying flight cost.
      p.mass <- p.mass + CPUE
      
      ## Add a day to the trip length
      p.trip_length <- p.trip_length + 1
      
    }
    
    ## Calculate flight cost per minute on the inbound trip.
    flight.cost.pmin.inbound <- flight.cost(body.mass = p.mass, 
                                            speed = f.speed/60,
                                            bmr.w = (f.BMR/60)*(female.res[pair,day]/1000)*60)
    
    ## Multiply the cost by the distance
    
    ## Subtract this cost from the adults condition at feeding. The remainder is what is available for the chick.
    
    
    
    ## Add a day to the trip length
    p.trip_length <- p.trip_length + 1
    
  }
  
  ## If long trip:
  if(trip.type == "lt"){
    
    flight.cost.pmin.outbound <- (flight.cost(body.mass=(female.res[pair,day]/1000), 
                                              speed=(f.speed/60),
                                              food.carried=((f.gut.cont)/1000),
                                              bmr.w=((f.BMR/60)*(female.res[pair,day]/1000)))*60)
    ## Calculate foraging gains
    CPUE<-10.24*((PREY.DENS^24.07)/((PREY.DENS^24.07)+(0.34^24.07)))  #determining the foraging efficiency from the input prey density.
    # f.gut.cont[2] <- (f.gut.cont[1]+f.cpue-f.dig.rate)},{f.gut.cont[1]<-f.night.gc
    
    ## Reassess adults 'condition'. Forage until crosses upper threshold.
    while(p.mass < upper.threshold){
      
      ## Continue to forage.
      CPUE<-10.24*((PREY.DENS^24.07)/((PREY.DENS^24.07)+(0.34^24.07)))  #determining the foraging efficiency from the input prey density.
      
      ##  No longer paying flight cost.
      p.mass <- p.mass + CPUE
      
      ## Add a day to the trip length
      p.trip_length <- p.trip_length + 1
      
      
      
    }
      
      ## Calculate flight cost per minute on the inbound trip.
      flight.cost.pmin.inbound <- (flight.cost(body.mass=(female.res[pair,day]/1000), 
                                               speed=(f.speed/60),
                                               food.carried=((f.gut.cont)/1000),
                                               bmr.w=((f.BMR/60)*(female.res[pair,day]/1000)))*60)
    
  }
  
  
  ## At the end, return the parent with their body mass and 
  
  
}
