flight.cost<-function(wing.span=0.707, #wing span (m)
                      wing.area=0.0544, #wing area (m2)
                      body.mass=p., #body mass (kg)
                      # food.carried=0, #food being carried (kg)
                      air.dens=1.225,  #air density (kg/m3)
                      bmr.w=3.512, #bmr(w)
                      speed=20) #flight velocity (m/s) 
{                           
  v<-speed
  a.r<-(wing.span^2)/wing.area          # aspect ratio    
  
  ####
  
  # all.up.mass<-body.mass+food.carried                   #total mass that the bird has to keep in the air
  
  all.up.mass<-p.mass
  
  body.frontal.area<-0.00813*(all.up.mass^0.666)        ##cross sectional area of body at widest part
  flat.plate.area<-body.frontal.area*0.10               ##0.10 is the default value of body dag coefficient
  disc.area<-pi*wing.span^2/4                           ##the area of a circle who's diameter is equal to wingspan
  
  induced.power<-1.2*all.up.mass^2*9.81^2/(2*air.dens*disc.area*v)        ##induced power, power needed to support the weight
  parasite.power<- air.dens*flat.plate.area*v^3/2                         ##parasite power power needed to overcome drag
  x1<-8.4/a.r                                                             ##profile power ratio, 8.4 is default value of profile power constant
  estimate.vmp<-(0.807*1.2^0.25*all.up.mass^0.5*9.81^0.5)/(air.dens^0.5*wing.span^0.5*body.frontal.area^0.250*0.10^0.25)
  #estimated minimum power speed assuming induced power factor is 1.2 and using default body drag coefficient see eqn 2 box 3.3 in mechanics of level flight
  induced.power.min<-1.2*all.up.mass^2*9.81^2/(2*air.dens*disc.area*estimate.vmp)  
  parasite.power.min<- air.dens*flat.plate.area*estimate.vmp^3/2                                                 
  profile.power<-(induced.power.min+parasite.power.min)*x1                       
  
  
  p<-induced.power+parasite.power+profile.power    
  #mechanical power is the rate at which flight muscles have to do work.  
  r<-1.1                                                                          #respiration factor default 1.1
  n<-0.23                                                                         #conversion efficiency proportion of fuel energy converted to                                                                                   mechanical work
  pw<-r*(p+(n*bmr.w))/n                                          #the chemical cost of flight in W
  
  ans<-pw
  
}
