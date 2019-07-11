#####################################################################
#####################################################################


####FUNCTION WHICH CALCULATES THE COST OF FLIGHT GIVEN A LOAD CARRIED AND A BODY MASS AND BMR
#Out put is a single number in watts (j/s)
#using information from pennycuick model help file. The function outputs have been checked aganst outputs from FLIGHT for windows v1.22
flight.cost<-function(wing.span=0.707, wing.area=0.0544, body.mass=0.83, food.carried=0, air.dens=1.225,  bmr.w=3.512, speed=20)
{                           #wing span (m), wing area (m2), body mass (kg), food being carried (kg), air density (kg/m3), flight velocity (m/s) bmr(w)
  v<-speed
  a.r<-(wing.span^2)/wing.area          # aspect ratio    
  
  ####
  
  all.up.mass<-body.mass+food.carried                   #total mass that the bird has to keep in the air
  
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

###########################################################################################
###########################################################################################
#####
#FUNCTION FOR CALCULATING THE MASS (GRAMS) OF THE CHICK ON THE NEXT DAY GIVEN THE PREVIOUS MASS AND ENERGY (KJ) RECIEBVED FROM PARENTS 
chick.growth<-function(prev.mass=73, a.e=0.79, b=0.011, food=353,  d=1.285, e1=3.51, e2=4.82)
{                        #prev.mass=mass on previous day (g), a.e.=chick's assimilation efficiency, food=daily food intake in kJ b,d,e1 & e2 are all constants
  fledge.mass<-249                                      #mean mass at fledging (wanless et al 2005)  
  
  
  ro<-b*(prev.mass^d)                                   #resting metabolism
  cost<-(24*ro)                                         #total metabolic cost
  energy.dens<-e1+(e2*(prev.mass/fledge.mass))          #energy density of the tissue of a growing chick
  new.mass<-prev.mass+(((a.e*food)-cost)/energy.dens)   #calculating the new chick mass
  
}

##################################################################################
##################################################################################
##MODEL FUNCTION VERSION 19.1

guillemot<-function(DAYS=25, ## number of days to simulate
                    PAIRS=1, ## number of pairs
                    PREY.DENS=0.31, ## prey density
                    AD.PREY.ED=5100, ##  energy density of adult bird prey in J per g
                    CK.PREY.ED=8500, ##  energy density of chick prey in J per g
                    CK.MEAL.MASS=rep(c(7.01,7.36,6.7,5.08), times=c(4,5,5,((25+1)-14))), ## the mass of an individual fish fed to chick (g) ; 
                    F.MASS=943, ## F.MASS mean body mass of the female (g); 
                    M.MASS=943, ## M.MASS mean body mass of the male (g);
                    F.MASS.SD=28.5, ## F.MASS.SD the standard deviation in the mass of adults;
                    M.MASS.SD=28.5,  ## M.MASS.SD the standard deviation in the mass of adults ;
                    F.CRIT.MASS=750, ## F.CRIT.MASS the lower critical body mass of the female below which behaviour decisions change (g); 
                    M.CRIT.MASS=750, ## M.CRIT.MASS the lower critical body mass of the male below which behaviour decisions change (g); 
                    F.DEAD=600, ## F.DEAD the body mass below which the female adult is dead (g), 
                    M.DEAD=600,  ## M.DEAD the body mass below which the male adult is dead (g); 
                    F.SPEED=19.1, ## F.SPEED female flight speed in metres per second, 
                    M.SPEED=19.1, ## M.SPEED is male flight speed in metres per second; 
                    F.RANGE=7080, ## F.RANGE the distance between the nest and the foraging site for the female (m); 
                    M.RANGE=7080, ## M.RANGE the distance between the nest and the foraging site for the male (m); 
                    F.DIG.RATE=0.333, ## F.DIG.RATE the digestion rate (g/?) <-- Not sure about this parameter
                    M.DIG.RATE=0.333, ## M.DIG.RATE the digestion rate (g/?) <-- Not sure about this parameter
                    F.GUT.MAX=200, ## F.GUT.MAX the maximum mass of food the female can have in her gut at once (g),
                    M.GUT.MAX=200, ##  M.GUT.MAX the maximum mass of food the male can have in his gut at once (g)
                    F.MEAL.SIZE=50, ## F.MEAL.SIZE the mass of normal meal of adult female if fed regularly (g), 
                    M.MEAL.SIZE=50, ## M.MEAL.SIZE the mass of a normal meal of adult male if fed regularly (g); 
                    F.GUT.MIN=1, ## F.GUT.MIN the minimum gut content before the female will start feeding again (g); 
                    M.GUT.MIN=1, ## M.GUT.MIN the minimum gut content before the male will start feeding again (g); 
                    F.MAX.BOUT=24.9, ## F.MAX.BOUT is the maximum duration of a diving bout for the female (min); 
                    M.MAX.BOUT=24.9, ## M.MAX.BOUT is the maximum duration of a diving bout for the male bird (min); 
                    F.AE=0.79, ## F.AE female assimilation efficiency; 
                    M.AE=0.79, ## M.AE male assimilation efficiency; 
                    F.ED=38500, ## F.ED the energy density of female body tissue in J per g; 
                    M.ED=38500,  ## M.ED the energy density of male body tissue in J per g, 
                    F.INIT.REST=10.7,## F.INIT.REST the amount of time the female rests on water just after arriving at a foraging site (min), 
                    M.INIT.REST=10.7, ## M.INIT.BOUT the amount of time the male rests on the water before the start of the first foraging bout (min);
                    F.IB.REST=26.5, ## F.IB.REST the duration of time resting on the surface between consecutive diving bouts for the female in mins,
                    M.IB.REST=26.5, ##  M.IB.REST the duration of time resting on the water surface between consecutive diving bouts for the male (min), 
                    F.END.REST=11.2, ## F.END.REST the duration of the rest after the last loaf time and before the female returns to the colony (min), 
                    M.END.REST=11.2, ## M.END.REST the duration of the resting time after the male has finished feeding and before he returns to the colony (min), 
                    F.BMR=430.2, ## F.BMR the female's basal metabolic rate in J per min per kg,
                    M.BMR=430.2, ##  M.BMR the male's basal metabolic rate (J per min per kg), 
                    F.NEST.MR=882, ## F.NEST.MR the metabolic rate while the female is in the colony (j per min per kg), 
                    M.NEST.MR=882, ## M.NEST.MR the male's metabolic rate while he is at the nest (J per min per kg),
                    F.SURF.MR=611.4, ##  F.SURF.MR the female's metabolic rate while resting on the water surface (J per min per kg),
                    M.SURF.MR=611.4, ##  M.SURF.MR the male's metabolic rate while resting on the water surface (J per min per kg), 
                    F.DIVE.MR=1429.8, ## F.DIVE.MR is the metabolic rate of the female engaged in foraging activity (J per min per kg - including dives and between dive rests), 
                    M.DIVE.MR=1429.8, ## M.DIVE.MR the metabolic rate of a male enganged in foraging activity (J per min per kg - including both dives and between dive rests), 
                    HATCH=72.1, ## HATCH is the hatching mass of the chick (g),
                    HATCH.SD=7.7, ##  HATCH.SD is the standard deviation of the hatching mass; 
                    CK.MAX=c(32,34,36,38,40,42,44,46,48,50,rep(50, times=22)), ## CK.MAX the maximum mass of food a chick can consume in a day is a vector for mass at each day (g), 
                    CK.DEAD=0.6,
                    PROV.SUCC=0.5, 
                    PRED.RATE=0, ## PRED.RATE is the probability that a chick left unattended will die in a minute, 
                    COLL.RATE=0)  ## COLL.RATE is the probability that an adult engaged in foraging activity will die.   
  
  #ARGUEMENTS: DAYS no.of day timesteps; PAIRS no. of pairs/iterations;;; CK.MEAL.MASS 
  
  #NOTE THAT HAVING VERY DIFFERENT GUT MINIMUM PARAMETERS FOR BIRDS IN THE SAME PAIR CAN LEAD TO PROBLEMS IF THEY EVER GET IN A SITUATION WHERE A BIRD WITH HIGHER GUT CONTENT CAN BE bELOW GUT MIN WHILE THERE PARTNer HAS A LOWER GUT CONTENT BUT IS HIGHER THAN GUT MIN.
  
  ################################################################################
{                   ##OPENING THE FUNCTION
  ################################################################################
  
  ################################################################################
  ####DEFINE THE ENVIRONMENT                                                                    
  ################################################################################
  
  
  prey.energy.density<-AD.PREY.ED    #energy density of prey j/g for the prey of adults
  chick.prey.energy.dens<-CK.PREY.ED #energy density of prey j/g for the prey of chicks
  prey.dens<-PREY.DENS                #density of prey in the environment (relative to maximum biomass since 1983 levels)
  
  ################################################################################
  ###DEFINE THE NUMBER OF PAIRS AND TIME STEPS
  ################################################################################                       
  
  no.of.pairs<-PAIRS    #number of pairs that will be modelled
  time.step<-DAYS     #number of time steps (days) in the breeding season which will be modelled
  
  ################################################################################
  ##CREATE MATRICES FOR OUTPUT
  ################################################################################ 
  #CREATES 3 MATRICES, ONE FOR EACH OF THE BIRDS IN THE FAMILY UNIT. EACH ROW REPRESENTS A DIFFERENT INDIVIDUAL AND EACH COLUMN ANOTHER DAY IN THE SEASON
  
  #CREATES SEQUENCE FOR THE NAMES OF THE ROWS AND COLUMNS                     
  rows<-seq(1,no.of.pairs,1)       #set number of rows in the matrix
  columns<-seq(1,(time.step+1),1)  #set number of columns in the matrix
  columns2<-seq(1,time.step,1)
  
  #MATRIX FOR MALE PARENT MASS
  male.res <- matrix((rep(NA,(no.of.pairs*(time.step+1)))), ncol=(time.step+1))   #create a matrix called male.res to store male body mass
  rownames(male.res)<-rows     #row names is the number of pairs
  colnames(male.res)<-columns  #columns is the mumber of time steps
  
  #MATRIX FOR FEMALE PARENT MASS
  female.res <- matrix((rep(NA,(no.of.pairs*(time.step+1)))), ncol=(time.step+1))           
  rownames(female.res)<-rows         #same as above but for the female bird
  colnames(female.res)<-columns
  
  #MATRIX FOR CHICK MASS
  chick.mass <- matrix((rep(NA,(no.of.pairs*(time.step+1)))), ncol=(time.step+1))           
  rownames(chick.mass)<-rows        #matrix for chick mass
  colnames(chick.mass)<-columns
  
  #MATRIX FOR MALE DAILY ENERGY EXPENDITURE
  male.dee <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.dee to store male daily energy expenditure
  rownames(male.dee)<-rows     #row names is the number of pairs
  colnames(male.dee)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR FEMALE ENERGY EXPENDITURE
  female.dee <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.dee to store female daily energy expenditure
  rownames(female.dee)<-rows     #row names is the number of pairs
  colnames(female.dee)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR MALE ENERGY GAIN
  male.gain <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.gain to store male daily energy gain 
  rownames(male.gain)<-rows     #row names is the number of pairs
  colnames(male.gain)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR FEMALE ENERGY GAIN
  female.gain <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.gain to store female daily energy gain
  rownames(female.gain)<-rows     #row names is the number of pairs
  colnames(female.gain)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR MALE FOOD INTAKE
  male.food <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.food to store male daily food intake 
  rownames(male.food)<-rows     #row names is the number of pairs
  colnames(male.food)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR FEMALE FOOD INTAKE
  female.food <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.food to store male daily food intake
  rownames(female.food)<-rows     #row names is the number of pairs
  colnames(female.food)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR MALE FORAGING BOUTS
  male.bouts <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.bouts to store number of  foraging bouts by the male each day
  rownames(male.bouts)<-rows     #row names is the number of pairs
  colnames(male.bouts)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR FEMALE FORAGING BOUTS
  female.bouts <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.bouts to store number of  foraging bouts by the male each day
  rownames(female.bouts)<-rows     #row names is the number of pairs
  colnames(female.bouts)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR NUMBER OF TIMES CHICK IS FED
  chick.feeds <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called chick.feeds for the number of times the chick is fed
  rownames(chick.feeds)<-rows     #row names is the number of pairs
  colnames(chick.feeds)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR CHICK INTAKE AMOUNT
  chick.intake <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called chick intake which stores the mass of food fed to the chick each day
  rownames(chick.intake)<-rows     #row names is the number of pairs
  colnames(chick.intake)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR THE % DAYLIGHT SPENT AT NEST BY MALE
  male.per.nest <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.per.nest to calculate the percentage of daylight hours males spend at the nest
  rownames(male.per.nest)<-rows     #row names is the number of pairs
  colnames(male.per.nest)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR THE  % DAYLIGHT SPENT AT NEST BY FEMALE
  female.per.nest <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.per.nest to calculate the percentage of daylight hours males spend at the nest
  rownames(female.per.nest)<-rows     #row names is the number of pairs
  colnames(female.per.nest)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR % DAYLIGHT SPENT RESTING ON WATER SURFACE BY MALE
  male.per.rest <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.per.rest to calculate the percentage of daylight hours males spent resting on the water surface
  rownames(male.per.rest)<-rows     #row names is the number of pairs
  colnames(male.per.rest)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR THE  % DAYLIGHT SPENT RESTING ON WATER SURFACE BY FEMALE
  female.per.rest <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.per.rest to calculate the percentage of daylight hours males spend resting on the water surface
  rownames(female.per.rest)<-rows     #row names is the number of pairs
  colnames(female.per.rest)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR % DAYLIGHT SPENT FORAGING BY MALE
  male.per.dive <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.per.dive to calculate the percentage of daylight hours males spent foraging
  rownames(male.per.dive)<-rows     #row names is the number of pairs
  colnames(male.per.dive)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR THE  % DAYLIGHT SPENT FORAGING BY FEMALE
  female.per.dive <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.per.dive to calculate the percentage of daylight hours males spend foraging
  rownames(female.per.dive)<-rows     #row names is the number of pairs
  colnames(female.per.dive)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR % DAYLIGHT SPENT IN FLIGHT BY MALE
  male.per.fly <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called male.per.fly to calculate the percentage of daylight hours males spent flying
  rownames(male.per.fly)<-rows     #row names is the number of pairs
  colnames(male.per.fly)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR THE  % DAYLIGHT SPENT IN FLIGHT BY FEMALE
  female.per.fly <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called female.per.fly to calculate the percentage of daylight hours males spend flying
  rownames(female.per.fly)<-rows     #row names is the number of pairs
  colnames(female.per.fly)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR % OF WHOLE DAY CHCIK IS ALONE
  chick.ab <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called chick.ab for the percentage of the whole day that the chick is left alone at the colony
  rownames(chick.ab)<-rows     #row names is the number of pairs
  colnames(chick.ab)<-columns2  #columns is the mumber of time steps
  
  #MATRIX FOR CHICK CONDITION
  ck.condition <- matrix((rep(NA,(no.of.pairs*time.step))), ncol=time.step)   #create a matrix called ck.condition which shows the condition of the chick 
  rownames(ck.condition)<-rows     #row names is the number of pairs
  colnames(ck.condition)<-columns2  #columns is the mumber of time steps
  
  fem.b<-array(dim=c(1440, time.step, no.of.pairs))     #creates an array to store the minute by minute behaviour decisions
  mal.b<-array(dim=c(1440, time.step, no.of.pairs))
  
  fem.gc<-array(dim=c(1440, time.step, no.of.pairs))    #creates an array to store the minute by minute gut content 
  mal.gc<-array(dim=c(1440, time.step, no.of.pairs))
  ################################################################################
  ##FOR EACH PAIR IN THE POPULATION
  ################################################################################                  
  for (pair in 1:no.of.pairs)         #for each pair in the modelled population
  {
    
    ################################################################################
    ##SETTING INDIVIDUAL CHARACTERISTICS
    ################################################################################
    ##ENVIRONMENT
    success<-PROV.SUCC                                  #the probability of an adult finding food for its chick during a foraging bout
    CPUE<-10.24*((PREY.DENS^24.07)/((PREY.DENS^24.07)+(0.34^24.07)))  #determining the foraging efficiency from the input prey density.
    
    
    ##FEMALE
    
    f.hungry.reserve<-F.CRIT.MASS   #the critical weight threshold below which adult is "starving" and behavioural decisions change
    f.dead.mass<-F.DEAD # the mass below which the female is dead
    
    f.forage.dist<-F.RANGE     #sets foraging distance (metres) 
    f.speed<-(F.SPEED*60)       # sets female flight speed (metres/minute)
    f.flight.duration<-f.forage.dist/f.speed    #amount of time will take to fly between nest and foraging area
    f.cpue<-CPUE          #catch per unit effort (grams of fish per minute) --will be determines by relationship with prey density
    f.dig.rate<-F.DIG.RATE       #rate at which food is removed from the gut (grams per minute)
    f.gut.max<-F.GUT.MAX          #maximum mass of food can hold in gut
    f.gut.int<-F.MEAL.SIZE           #normal meal sizes
    f.gut.min<-F.GUT.MIN            #gut content below which bird willing to iniate feeding again
    f.a<-F.AE               #assimilation efficency
    f.g<-F.ED              #energy density of storage tissue (j/g)
    
    
    f.forage.max<-F.MAX.BOUT                   #setting the maximum duration of the diving bout (minutes)
    
    f.before.loaf<-F.INIT.REST    #number of minutes bird will rest on water surface when it arrives at a foraging site.
    f.end.loaf<-F.END.REST       #number of minutes bird will rest on water surface after its last foraging bout before leaving
    f.inter.loaf<-F.IB.REST     #number of minutes between consecutive foraging bouts
    
    f.Rforage<-F.DIVE.MR      #metabolic cost while foraging (J/min kg)
    f.Rloaf<-F.SURF.MR        #metabolic cost while resting on water surface (J/min kg)
    f.Rnest<-F.NEST.MR        #metabolic cost of being at the colony (J/min kg)
    f.BMR<-F.BMR          #bmr in J/min kg   (needed for flight cost function)
    
    
    ###MALE  --- same as above but for the male parent
    
    
    m.hungry.reserve<-M.CRIT.MASS 
    m.dead.mass<-M.DEAD
    
    m.forage.dist<-M.RANGE               
    m.speed<- (M.SPEED*60)                                                      
    m.flight.duration<-m.forage.dist/m.speed                            
    m.cpue<-CPUE                                                   
    m.dig.rate<-M.DIG.RATE                                                   
    m.gut.max<-M.GUT.MAX  
    m.gut.int<-M.MEAL.SIZE                                                   
    m.gut.min<-M.GUT.MIN
    m.a<-M.AE
    m.g<-M.ED       
    
    
    m.forage.max<-M.MAX.BOUT
    
    m.before.loaf<-M.INIT.REST             
    m.end.loaf<-M.END.REST                                                             
    m.inter.loaf<-M.IB.REST                                                          
    
    m.Rforage<-M.DIVE.MR  
    m.Rloaf<-M.SURF.MR  
    m.Rnest<-M.NEST.MR  
    m.BMR<-M.BMR    
    
    ################################################################################
    ##INITIAL VALUES FOR VARIABLES
    ################################################################################                    
    
    ##CHICK
    chick.mass[pair,1]<-rnorm(1,HATCH,HATCH.SD)     #chick mass on day 1 (hatching) in grams
    
    
    ##FEMALE
    female.res[pair,1]<-rnorm(1,F.MASS,F.MASS.SD)    #sets body mass for day 1
    
    f.beak.cont<-0    #defines mass in grams being carried in the females beak. starts at zero g. If this is set in this loop rather than each day loop it means the adult remembers it is holding a fish in the morning if it caught one the night before.
    
    f.trip.type<-"none"     #sets the trip type, which affects the behaviour rules. initally set as none but will change to parent only, chick only or both depending on parent and chick physiological states
    f.loaf.time<-0          #a counter for the number of minutes spent loafing, set as zero to start
    f.chk.provisioned<-0     #a value of 0 or one which determines whether or not the female has done a chick provisioning bout
    f.collision<-0          #a value of zero indicates that the female has not collided with a device and is still alive
    
    ##MALE
    ##as above but for the male parent
    male.res[pair,1]<-rnorm(1,M.MASS,M.MASS.SD)                             
    
    m.beak.cont<-0 
    
    m.trip.type<-"none"
    m.chk.provisioned<-0 
    m.loaf.time<-0  
    m.collision<-0
    
    ################################################################################
    ##FOR EACH DAY IN THE SEASON                                          
    ################################################################################
    for (day in 1:time.step)   #for every day in the season
    {
      ################################################################################
      ##CREATE VECTORS TO STORE BEHAVIOURAL AND PHYSIOLOGICAL STATE
      ################################################################################
      
      f.behaviour<-rep(NA, times=1440) #create a vector to store the behaviour the bird was engaged in each minute of the day
      
      f.gut.cont<-rep(0, times=1441)   #creates a vector to store gut content each minute through the day (g), to start with set to zero but this will be over written as the model runs
      
      f.e.expend<-rep(0, times=1441)   #creates a vector which will store the energy expended each minute (j)
      
      m.behaviour<-rep(NA, times=1440)   #as above but for male parent
      m.gut.cont<-rep(0,times=1441)
      m.e.expend<-rep(0, times=1441)    
      
      
      ################################################################################
      ##SET INITIAL BEHAVIOURAL STATE
      ################################################################################
      
      ifelse(day==1, f.behaviour[1]<-"nest", f.behaviour[1]<-f.night.behaviour)       #sets the behaviour in the first minute as being at nest for day 1 or how bird spent the night if another day  
      if(day==1 & f.behaviour[1]!="nest")f.trip.type<-"both"         ##if away from nest at first minute of first day, gives trip type of "both" 
      
      ifelse(day==1,{f.gut.cont[1]<-0
      f.gut.cont[2]<-0},ifelse(f.behaviour[1]=="forage",{f.gut.cont[1]<-f.night.gc
      f.gut.cont[2] <- (f.gut.cont[1]+f.cpue-f.dig.rate)},{f.gut.cont[1]<-f.night.gc
      f.gut.cont[2]<-f.gut.cont[1]-f.dig.rate}))
      if(f.gut.cont[2]<0) f.gut.cont[2]<-0
      ##allows the gut content from the previous day to be "remembered", have to calculate the first two minutes becuase run minutes from minute 2 and calculates the minute+1 at the end. 
      ##The loop for if f.behaviour[1]==forage should never be entered because the rules as they currently are mean that the bird should not be foraging at the end of the night
      
      
      ifelse(day==1, m.behaviour[1]<-"nest", m.behaviour[1]<-m.night.behaviour) #as above but for male parent      
      if(day==1 & m.behaviour[1]!="nest")m.trip.type<-"both"  
      ifelse(day==1,{m.gut.cont[1]<-0
      m.gut.cont[2]<-0},ifelse(m.behaviour[1]=="forage",{m.gut.cont[1]<-m.night.gc
      m.gut.cont[2] <- (m.gut.cont[1]+m.cpue-m.dig.rate)},{m.gut.cont[1]<-m.night.gc
      m.gut.cont[2]<-m.gut.cont[1]-m.dig.rate}))
      if(m.gut.cont[2]<0) m.gut.cont[2]<-0
      
      
      ################################################################################
      ##DEFINING THE CONDITION OF CHICK
      ################################################################################
      ifelse(day>=3 & day<=18, ifelse((chick.mass[pair,day]<chick.mass[pair,day-1]) & (chick.mass[pair,day-1]<chick.mass[pair,day-2]),
                                      chick.cond<-"critical", ifelse(day>=10 & chick.mass[pair,day]<100, chick.cond<-"critical", chick.cond<-"ok")),
             ifelse(day>=10 & chick.mass[pair,day]<100, chick.cond<-"critical", chick.cond<-"ok"))
      
      
      
      #chick is in critical condition if it is between 5 and 18 days old and has declines in mass on two consecutive days or is older than 10 days and under 100 g.
      
      if(chick.mass[pair,day]<(chick.mass[pair,1]*CK.DEAD)|chick.cond=="dead") chick.cond<-"dead"
      
      #if chick mass drops to 60% OF ITS HATCHING MASS or has been predated THEN IT IS DEAD.
      
      ################################################################################
      
      ################################################################################
      ##RESET MEAL SIZES TO ZERO
      ################################################################################                    
      ifelse(day==1,f.meal.size<-0,ifelse(female.res[pair,day]<f.hungry.reserve,f.meal.size<-0,f.meal.size<-f.meal.size))
      #1st day meal size is zero, if bird is critical meal size is zero, otherwise remembers from the night before
      ifelse(day==1,f.beak.cont<-0,ifelse(female.res[pair,day]<f.hungry.reserve,f.beak.cont<-0,f.beak.cont<-f.beak.cont))
      #1st day beak content is set to zero after that it is remembered from the night before, unless the adult is below critical condition mass in which case it is set to zero (otherwise bird doesn't start foraging again)
      ifelse(day==1,f.chk.provisioned<-0,ifelse(female.res[pair,day]<f.hungry.reserve,f.chk.provisioned<-0,f.chk.provisioned<-f.chk.provisioned))
      #1st day the female hasn't performed a foraging bout to provision the chick after that it is remembered from the night before, unless the adult is below critical condition mass in which case it is set to zero (otherwise bird doesn't start foraging again)
      
      
      ifelse(day==1,m.meal.size<-0,ifelse(male.res[pair,day]<m.hungry.reserve,m.meal.size<-0,m.meal.size<-m.meal.size))  #as above but for male bird
      
      ifelse(day==1,m.beak.cont<-0,ifelse(male.res[pair,day]<m.hungry.reserve,m.beak.cont<-0,m.beak.cont<-m.beak.cont))
      ifelse(day==1,m.chk.provisioned<-0,ifelse(female.res[pair,day]<m.hungry.reserve,m.chk.provisioned<-0,m.chk.provisioned<-m.chk.provisioned))
      
      chick.food<-0       #a variable to keep track of mass of food fed to chick over day. 
      chick.meal.max<-CK.MAX[day]  #maximum mass of food chick can intake in a day (g) 
      chick.prey.size<-CK.MEAL.MASS[day] #the mass (grams) of food fed to chick each feed  
      
      ################################################################################
      ##SET UP COUNTERS FOR TOTAL DAILY BEHAVIOUR
      ################################################################################
      
      #for female
      ifelse (f.behaviour[1]=="nest", 
              {f.T.nest<-1
              f.e.expend[1]<-f.Rnest*(female.res[pair,day]/1000)}, 
              f.T.nest<-0) 
      #if first minute at nest the total at nest counter is set to one and stores relevant energy expenditure in the vector. otherwise the total at nest counter is set to zero
      
      ifelse (f.behaviour[1]=="loaf", {f.T.loaf<-1                     
      f.e.expend[1]<-f.Rloaf*(female.res[pair,day]/1000)},
      f.T.loaf<-0)
      #if first minute loafing on water surface the total loaf counter is set to one  and stores relevant energy expenditure in the vector otherwise the total loaf counter is set to zero
      
      ifelse (f.behaviour[1]=="out.flight" | f.behaviour[1]=="in.flight", {f.T.flight<-1
      f.e.expend[1]<-(flight.cost(body.mass=(female.res[pair,day]/1000), speed=(f.speed/60),food.carried=((f.gut.cont[1]+f.beak.cont)/1000),bmr.w=((f.BMR/60)*(female.res[pair,day]/1000)))*60)},
      f.T.flight<-0)  
      #if first minute was flying to or from nest the total flight counter is set to one and energy expended is calculated using the flight cost function and stored in the vector.Otherwise total flight vector is set to zero
      
      ifelse (f.behaviour[1]=="forage", {f.T.forage<-1
      f.T.parent.only<-1   
      f.e.expend[1]<-f.Rforage*(female.res[pair,day]/1000)},{
        f.T.parent.only<-0
        f.T.forage<-0}) 
      #if first minute was foraging the total forage counter set to one, the parent only foraging counter is set to one and stores the relevant energy expenditure in the vector. Otherwise total forage counter and parent only forage counter are set to zero
      
      ifelse (f.behaviour[1]=="initial", {f.T.loaf<-1
      f.e.expend[1]<-f.Rloaf*(female.res[pair,day]/1000)},
      f.T.loaf<-0) #if first minute loafing on water surface before a dive bout the total loaf counter is set to one  and stores relevant energy expenditure in the vector otherwise the total loaf counter is set to zero
      
      ###same as above but for the male bird.
      ifelse (m.behaviour[1]=="nest", {m.T.nest<-1
      m.e.expend[1]<-m.Rnest*(male.res[pair,day]/1000)},
      m.T.nest<-0)
      
      ifelse (m.behaviour[1]=="loaf", {m.T.loaf<-1
      m.e.expend[1]<-m.Rloaf*(male.res[pair,day]/1000)},
      m.T.loaf<-0)
      
      ifelse (m.behaviour[1]=="out.flight" | m.behaviour[1]=="in.flight", {m.T.flight<-1
      m.e.expend[1]<-(flight.cost(body.mass=(male.res[pair,day]/1000), speed=(m.speed/60),food.carried=((m.gut.cont[1]+m.beak.cont)/1000),bmr.w=((m.BMR/60)*(male.res[pair,day]/1000)))*60)},
      m.T.flight<-0)
      
      ifelse (m.behaviour[1]=="forage", {m.T.forage<-1
      m.T.parent.only<-1
      m.e.expend[1]<-m.Rforage*(male.res[pair,day]/1000)},
      { m.T.parent.only<-0
      m.T.forage<-0})   
      
      ifelse (m.behaviour[1]=="initial", {m.T.loaf<-1
      m.e.expend[1]<-m.Rloaf*(male.res[pair,day]/1000)},
      m.T.loaf<-0) 
      
      ################################################################################
      ##SETTING COUNTERS TO TRACK THE DURATION OF CURRENT BEHAVIOUR
      ################################################################################
      
      #for female
      
      ifelse(f.behaviour[1]=="nest", ifelse(day==1, f.nest.duration<-1, f.nest.duration<-f.nest.duration+1), f.nest.duration<-0)     #if it is the first day  and bird on nest, nest duration is equal to one, any other day it is the duration from the end of the previous day +1.
      
      
      ifelse(f.behaviour[1]=="loaf", f.loaf.duration<-1, f.loaf.duration<-0)     #set counter for the amount of time bird has currently been loafing on the water for interbout periods and end of foraging trip. if bird was loafing in the 1st minute of day counter is 1, otherwise is 0
      
      ifelse(f.behaviour[1]=="initial", f.initloaf.duration<-1, f.initloaf.duration<-0) #set counter for the amount of time bird has currently been loafing on the water for loafing period immediately after arriving at foraging site. if bird was performing an initial loaf period in the 1st minute of the day counter is set to 1, otherwise is zero.
      
      ifelse(f.behaviour[1]=="out.flight", ifelse(day==1,f.outflight.duration<- 1,f.outflight.duration<-f.outflight.duration+1), f.outflight.duration<-0)  #sets the counter for the duration of time bird has currently being flying towards a forgaing site, since it set off. if it is the first minute of the first day the counter is set to one. if it is the first minute of any other day, one is added to the counter. this means the bird always remembers how long it has been flying even if it was doing so overnight. If the behaviour in the first minute is not outflight the counter is set to zero. 
      
      ifelse(f.behaviour[1]=="in.flight", ifelse(day==1, f.inflight.duration<- 1, f.inflight.duration<-f.inflight.duration+1), f.inflight.duration<-0)     #as above but for birds flying back to the colony.
      
      
      f.forage.duration<-f.T.forage    #time spent in foraging behaviour during current trip is set as total time spent foraging (this is setting the initial value)
      f.dive.duration<-f.T.forage      #time spent in current diving session set as total foraging time (this is setting the initial value)
      f.trip.duration<-0               #counter for the total duration of the trip
      
      f.bout<-0                       #setting a counter for number of foraging trips during a day
      f.intbout.duration<-0           #setting counter for the duration of an interbout rest.
      
      
      
      ##as above but for the male parent
      
      ifelse(m.behaviour[1]=="nest", ifelse(day==1, m.nest.duration<- 1, m.nest.duration<-m.nest.duration+1), m.nest.duration<-0)          
      ifelse(m.behaviour[1]=="loaf", m.loaf.duration<-1, m.loaf.duration<-0)           
      ifelse(m.behaviour[1]=="initial", m.initloaf.duration<-1, m.initloaf.duration<-0)
      ifelse(m.behaviour[1]=="out.flight", ifelse(day==1,m.outflight.duration<- 1,m.outflight.duration<-m.outflight.duration+1), m.outflight.duration<-0)  
      ifelse(m.behaviour[1]=="in.flight", ifelse(day==1, m.inflight.duration<- 1, m.inflight.duration<-m.inflight.duration+1), m.inflight.duration<-0)  
      m.forage.duration<-m.T.forage
      m.dive.duration<-m.T.forage  
      
      m.trip.duration<-0                                                      
      m.bout<-0 
      m.intbout.duration<-0
      
      ##CHICK
      no.chick.feeds<-0            #counter for the number of times the chick is fed
      chick.alone<-0               #counter to count the number of minutes the chick is left alone
      
      ################################################################################
      #FOR EACH MINUTE OF THE DAY
      ################################################################################
      
      for(minute in 2:1440)  #opens a loop to repeat the code every minute during the day (starts at minute 2 because the first minute is either set (if its the first day) for will be the same as the last minute of the previous day 
      {
        
        ################################################################################
        #DURING DAYLIGHT
        ################################################################################ 
        
        if(minute<=1200)           #in the first 1200 minutes of the day do the following (this needed because behaviour rules are different during the day and night. set as a 20 hour day like monaghan et al 1994)
        { 
          ################################################################################
          ##MALE AT NEST 
          ################################################################################    
          
          if (m.behaviour[minute-1]== "nest")      #if the male was at the nest in the previous minute
          {
            if (f.behaviour[minute-1]!="nest")       #if partner was not at the nest in the previous minute
            {
              
              if(m.gut.cont[minute]>m.gut.min) 
                ifelse(chick.cond=="critical", ifelse(chick.food>=chick.meal.max,m.behaviour[minute]<-"nest",{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"chick.only"}),m.behaviour[minute]<-"nest")    
              #if the adult gut content is not low enough to initiate feeding and the chick is not critical the parent will stay at the nest. If the chick is critical but has already recieved its maximum daily food intake the parent will also remain at the nest. If the chick is in a critical condition and hasn't recieved the amximum possible amount of food the adult will fly out towards a foraging site and the trip type will be chick only        
              
              
              if(m.gut.cont[minute] <= m.gut.min) 
                ifelse (male.res[pair,day]<m.hungry.reserve,  {m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"}, ifelse(chick.cond=="critical", ifelse(chick.food>=chick.meal.max,m.behaviour[minute]<-"nest",{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"}),m.behaviour[minute]<-"nest")) 
              #if the adult gut content is low enough to initiate a feed and its body mass is below a critical level it will fly towards a foraging site and the trip type will be parent only. if the chick is in critical condition but has been fed its maximum daily food intake the adult will remain on the nest, however if the chick is critical and hasn't recieved its maximum possible daily food intake the adult will fly to forage and the trip type will be both. if neither the parent nor the chick are in a physiologically critical condition the parent will remain at the nest. This prevents the chick been left unattended-unless conditions are critical        
              
              
            } #CLOSE OF IF PARTNER IS NOT AT NEST TOO LOOP
            
            if (f.behaviour[minute-1]=="nest")     #if partner was at nest in the previous minute
            {
              priority<-rbinom(1,1, 0.5)       ##gives a random 0 or 1 which can be used to determine if the male or female gets priority if conditions are the same 
              if(priority==0)
              {
                
                if(m.gut.cont[minute]>m.gut.min)   #if gut content is too large to initiate adult feeding
                {ifelse(chick.cond=="critical", 
                        ifelse(f.gut.cont[minute]<=f.gut.min, m.behaviour[minute]<-"nest", ifelse(m.nest.duration>=f.nest.duration,ifelse(chick.food>=chick.meal.max,m.behaviour[minute]<-"nest",{m.behaviour[minute]<-"out.flight"
                        m.trip.type<-"chick.only"}), m.behaviour[minute]<-"nest")),m.behaviour[minute]<-"nest") }        #if gut content larger than required to initiate feeding will stay on the nest unless chick is in critical condition. if chick is critical and the partner has a gut content low enough to initiate feeding the male will stay at nest. if the male has been on the nest less time than the partner the male will remain on the nest. if male has been on nest longer or equal to the female he will fly out to forage with a trip type of chick only, unless the chick has received its maximum daily food intake - in which case the male will stay at the nest.
                
                if(m.gut.cont[minute]<=m.gut.min)   #if gut content is low enough to initiate feeding
                {ifelse(f.gut.cont[minute]>=m.gut.cont[minute],ifelse(m.nest.duration>=f.nest.duration,ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }),ifelse(f.gut.cont[minute]<=f.gut.min,m.behaviour[minute]<-"nest",ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }))            
                ),ifelse(m.nest.duration>=f.nest.duration,ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }),m.behaviour[minute]<-"nest"))
                  # if the male has a gut content low enough to initiate feeding and the female does not have a gut content low enough to initiate feeding the male will leave on a foraging trip, if the chick has eaten the maximum mass of food it is capable of eating that day then the trip will be a parent only trip, otherwise it will be a both trip. if both parents have a gut content low enough to intitate feeding then if the male has been on the nest less time than the female it will stay on the nest. If the male has been on the nest longer than the female he will leave on a trip. if the chick has eaten the maximum mass of food it is capable of eating that day then the trip will be a parent only trip, otherwise it will be a both trip.
                }
              }  #CLOSE OF IF PRIOROTY IS ZERO LOOP
              
              
              if(priority==1)
              {
                
                if(m.gut.cont[minute]>m.gut.min) 
                {ifelse(chick.cond=="critical", 
                        ifelse(f.gut.cont[minute]<=f.gut.min, m.behaviour[minute]<-"nest", ifelse(m.nest.duration>f.nest.duration,ifelse(chick.food>=chick.meal.max,m.behaviour[minute]<-"nest",{m.behaviour[minute]<-"out.flight"
                        m.trip.type<-"chick.only"}), m.behaviour[minute]<-"nest")),m.behaviour[minute]<-"nest") }       
                #as above but if the nests duration times are equal the female will fly out to forage rather than the male
                
                if(m.gut.cont[minute]<=m.gut.min)
                {ifelse(f.gut.cont[minute]>m.gut.cont[minute],ifelse(m.nest.duration>f.nest.duration,ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }),ifelse(f.gut.cont[minute]<=f.gut.min,m.behaviour[minute]<-"nest",ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }))  
                ),ifelse(f.gut.cont[minute]==m.gut.cont[minute] & m.nest.duration>f.nest.duration,ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }),ifelse(m.nest.duration>f.nest.duration,ifelse(chick.food>=chick.meal.max,{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"parent.only"
                },{m.behaviour[minute]<-"out.flight"
                m.trip.type<-"both"
                }),m.behaviour[minute]<-"nest")
                )
                )
                }  
                #as above but changes so male stays at nest if conditions are equal
                
                
              }        #CLOSE OF IF PRIORITY = 1 LOOP
              
              
              
            } #CLOSE OF IF BOTH ADULTS ARE NEST LOOP
          }    #CLOSE OF MALE AT NEST LOOP
          
          
          
          ################################################################################
          ###SETTING DIFFERENT BEHAVIOURS FOR DIFFERENT TRIP TYPES
          ################################################################################
          
          ################################################################################
          ####TRIP TYPE OF BOTH
          ################################################################################                     
          
          if(m.trip.type=="both")                       #opens a loop to decide behaviour if trip type is both
          {   
            
            if(m.behaviour[minute-1]=="out.flight")      ##if male was in flight towards a feeding site in the previous minute...
            {
              ifelse(m.outflight.duration<m.flight.duration, m.behaviour[minute]<-"out.flight", m.behaviour[minute]<-"initial")
            }                         ##if hasn't been in flight long enough to reach the destination will stay in flight, otherwise start resting. 
            
            if(m.behaviour[minute-1]=="in.flight" )     ##if male was in flight towards nest in the previous minute...
            {
              ifelse(m.inflight.duration<m.flight.duration, m.behaviour[minute]<-"in.flight", m.behaviour[minute]<-"nest")
            }                         ##if hasn't been in flight long enough to reach the destination will stay in flight, otherwise start sitting at nest. 
            
            if(m.behaviour[minute-1]=="forage" )       #if male was foraging for himself in the previous minute
            {
              ifelse(m.gut.cont[minute] < m.gut.max,
                     ifelse(m.dive.duration >= m.forage.max, {m.behaviour[minute]<-"loaf"
                     m.loaf.time<-m.inter.loaf}, m.behaviour[minute]<-"forage")    ,{m.behaviour[minute]<-"loaf"
                     m.loaf.time<-m.inter.loaf})            #if haven't reached maximum gut content or exceed max duration of a diving bout will forage for itsself, otherwise will loaf on the water and it will be an interbout rest (because still need to forage for chick)
            }
            
            if(m.behaviour[minute-1]=="chick.prov" )       #if male was foraging for the chick in the previous minute
            {
              
              ifelse(m.dive.duration >= m.forage.max, {m.behaviour[minute]<-"loaf"
              m.loaf.time<-m.end.loaf}, m.behaviour[minute]<-"chick.prov")             #if haven't reached maximum forage duration will continue to dive for the chick otherwise will loaf on the water surface and the duration of this rest will be the end of rest duration prior to returning to the nest
            }
            
            if(m.behaviour[minute-1]=="initial")                 #if male was resting on the water and it was the rest between arriving at the foraging site and starting to dive
            {
              ifelse(m.before.loaf > m.initloaf.duration, m.behaviour[minute]<-"initial",m.behaviour[minute]<-"forage") 
            }       #if he has been resting on the surface longer than the initial loaf duration he will start diving to feed himself, otherwise will keep on the surface until he has been doing that long enough
            
            
            if(m.behaviour[minute-1]=="loaf")        #if the male was loafing in the previous minute
            {
              if(m.gut.cont[minute]<m.gut.min & male.res[pair,day]<m.hungry.reserve) m.meal.size<-0           #if the males gut content is less than the min to initiate feeding and below critical mass then his meal size is set to zero-this allows the bird to continue with another diving bout without returning to the nest
              ifelse(m.meal.size>=m.gut.int|m.chk.provisioned==1,
                     ifelse(m.intbout.duration>=m.loaf.time & male.res[pair,day]>m.hungry.reserve, ifelse(m.chk.provisioned==0, m.behaviour[minute]<-"chick.prov",m.behaviour[minute]<-"in.flight" ),{m.behaviour[minute]<-"loaf"
                     m.intbout.duration<-m.intbout.duration+1}),
                     ifelse(m.intbout.duration<m.loaf.time, {m.behaviour[minute]<-"loaf" 
                     m.intbout.duration<-m.intbout.duration+1}, m.behaviour[minute]<-"forage"))
              #if the male has caught enough food for a normal meal size or has something in his beak, if he has been loafing long enough and isn't in critical condition he will fly back, unless he hasn't yet foraged for the chick-in which case he'll forage for the chick. if the male hasn't been loafing long enough or is below a critical weight he will stay loafing and the loaf counter will increase by 1.
              #if the male hasn't had a large enough meal he will stay loafing (and the counter will increase) until has rested enough and will then start foraging for himself again 
              
            } 
          }                #CLOSING TRIP TYPE= BOTH LOOP.
          
          ################################################################################
          ###TRIP TYPE OF CHICK ONLY
          ################################################################################                       
          
          
          if(m.trip.type=="chick.only")
          {                                              #opening the loop for chick only trip type
            
            if(m.behaviour[minute-1]=="out.flight")      ##if male was in flight towards a feed in the previous minute...
            {
              ifelse(m.outflight.duration<m.flight.duration, m.behaviour[minute]<-"out.flight", m.behaviour[minute]<-"initial")
            }                         ##if hasn't been in flight long enough to reach the destination will stay in flight, otherwise start resting. 
            
            if(m.behaviour[minute-1]=="in.flight" )     ##if male was in flight towards nest in the previous minute...
            {
              ifelse(m.inflight.duration<m.flight.duration, m.behaviour[minute]<-"in.flight", m.behaviour[minute]<-"nest")
            }                         ##if hasn't been in flight long enough to reach the destination will stay in flight, otherwise start sitting at nest. 
            
            if(m.behaviour[minute-1]=="initial")          #if in the previous minute the male was resting on the surface after arriving at the foraging site
            {
              ifelse(m.before.loaf > m.initloaf.duration, m.behaviour[minute]<-"initial",m.behaviour[minute]<-"chick.prov") 
            }                #if he hasn't been resting long enough he will remain on the surface, otherwise he will start diving to feed the chick
            
            if(m.behaviour[minute-1]=="chick.prov" )       #if male was foraging for the chick in the previous minute
            {    
              ifelse(m.dive.duration >= m.forage.max, {m.behaviour[minute]<-"loaf"
              m.loaf.time<-m.end.loaf}, m.behaviour[minute]<-"chick.prov")             
            }                #if the male hasn't been foraging long enough he will continue to dive for the chick. otherwise he will rest on the water and the duration of that rest will be set as the duration of rest period prior to flying back to nest
            
            if(m.behaviour[minute-1]=="loaf")        #if the male was resting on the water in the previous minute
            {
              if(m.gut.cont[minute]<m.gut.min & male.res[pair,day]<m.hungry.reserve) m.meal.size<-0      #if his gut content is less than the min to initiate feeding and below critical mass then his meal size is set to zero-this allows the bird to continue with another diving bout without returning to the nest
              ifelse(m.intbout.duration>=m.loaf.time, m.behaviour[minute]<-"in.flight",{m.behaviour[minute]<-"loaf"
              m.intbout.duration<-m.intbout.duration+1})   #if the male hasn't been resting on the water long enough he will continue to do so and the counter will increase by 1. if he has rested long enough he will start to fly back to the nest.
            }
            
          }   #closing the type=chick loop
          
          ################################################################################
          ##TRIP TYPE OF PARENT ONLY
          ################################################################################                  
          
          
          if(m.trip.type=="parent.only")
          {                                       #opening loop to decide behaviour if is on a parent only trip
            
            if(m.behaviour[minute-1]=="out.flight")      ##if male was in flight towards a feed in the previous minute...
            {
              ifelse(m.outflight.duration<m.flight.duration, m.behaviour[minute]<-"out.flight", m.behaviour[minute]<-"initial")
            }                         ##if hasn't been in flight long enough to reach the destination will stay in flight, otherwise start resting on the water surface. 
            
            if(m.behaviour[minute-1]=="in.flight" )     ##if male was in flight towards nest in the previous minute...
            {
              ifelse(m.inflight.duration<m.flight.duration, m.behaviour[minute]<-"in.flight", m.behaviour[minute]<-"nest")
            }                         ##if hasn't been in flight long enough to reach the destination will stay in flight, otherwise start sitting at nest. 
            
            
            if(m.behaviour[minute-1]=="forage" )       #if male was foraging for himeself in the previous minute
            {
              ifelse(m.gut.cont[minute] < m.gut.max,
                     ifelse(m.dive.duration >= m.forage.max, {m.behaviour[minute]<-"loaf"
                     m.loaf.time<-m.inter.loaf}, m.behaviour[minute]<-"forage")    ,{m.behaviour[minute]<-"loaf"
                     m.loaf.time<-m.inter.loaf})            #if haven't reached maximum gut content or exceed max duration of a diving bout will continue to forage otherwise will rest on the water surface
            }
            
            
            if(m.behaviour[minute-1]=="initial")
            {
              ifelse(m.before.loaf > m.initloaf.duration, m.behaviour[minute]<-"initial",m.behaviour[minute]<-"forage") 
            }
            
            
            if(m.behaviour[minute-1]=="loaf")        #if the male was loafing in the previous minute
            {
              if(m.gut.cont[minute]<m.gut.min & male.res[pair,day]<m.hungry.reserve) m.meal.size<-0      ##if gut content is lower than the minimum to intiate a feed and the bird is critical (so will continue eating without returning to nest)the meal size is zero. this is needed to make sure adult still continues to feed itself after has left the nest and having trouble finding food.
              ifelse(m.meal.size>=m.gut.int,
                     ifelse(m.intbout.duration>=m.loaf.time & male.res[pair,day]>m.hungry.reserve, m.behaviour[minute]<-"in.flight",{m.behaviour[minute]<-"loaf"
                     m.loaf.time<-m.end.loaf
                     m.intbout.duration<-m.intbout.duration+1}),
                     ifelse(m.intbout.duration<m.loaf.time, {m.behaviour[minute]<-"loaf"
                     m.loaf.time<-m.inter.loaf 
                     m.intbout.duration<-m.intbout.duration+1}, m.behaviour[minute]<-"forage"))
              #if the male has had a large enough meal and he has rested long enough and is not below a critical mass he will fly back to the nest. if he has eaten enough but has not rested long enough or is below a critical mass then he will continue to rest and the counter will be increased by 1.
              #if the male hasn't had a normal meal size and hasn't being loafing long enough, he will continue to do so and the counter will increase by 1. If he has rested for long enough he will start foraging again 
              
            } 
          }            #closing the loop for parent only trip.
          
          ################################################################################
          #FEMALE BEHAVIOUR
          ################################################################################
          
          
          #same as above for but for the female bird
          
          ################################################################################
          #FEMALE AT NEST
          ################################################################################                     
          
          if (f.behaviour[minute-1]== "nest")  
          {
            if (m.behaviour[minute-1]!="nest")  
            {
              
              if(f.gut.cont[minute]>f.gut.min) ifelse(chick.cond=="critical", ifelse(chick.food>=chick.meal.max,f.behaviour[minute]<-"nest",{f.behaviour[minute]<-"out.flight"
              f.trip.type<-"chick.only"}),f.behaviour[minute]<-"nest")     
              if(f.gut.cont[minute] <= f.gut.min)                                  
                ifelse (female.res[pair,day]<f.hungry.reserve,  {f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"}, ifelse(chick.cond=="critical", ifelse(chick.food>=chick.meal.max,f.behaviour[minute]<-"nest",{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"}),f.behaviour[minute]<-"nest")) 
              
              
            }    #CLOSE LOOP FOR IF MALE NOT AT NEST
            if (m.behaviour[minute-1]=="nest") 
            {                                  
              if(priority==1)
              {
                
                if(f.gut.cont[minute]>f.gut.min) 
                {ifelse(chick.cond=="critical", 
                        ifelse(m.gut.cont[minute]<=m.gut.min, f.behaviour[minute]<-"nest", ifelse(f.nest.duration>=m.nest.duration,ifelse(chick.food>=chick.meal.max,f.behaviour[minute]<-"nest",{f.behaviour[minute]<-"out.flight"
                        f.trip.type<-"chick.only"}), f.behaviour[minute]<-"nest")),f.behaviour[minute]<-"nest") }        
                
                if(f.gut.cont[minute]<=f.gut.min)                                    
                {ifelse(m.gut.cont[minute]>=f.gut.cont[minute],ifelse(f.nest.duration>=m.nest.duration,ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"},{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }),ifelse(m.gut.cont[minute]<=m.gut.min,f.behaviour[minute]<-"nest",ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"
                },{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }))
                ),ifelse(f.nest.duration>=m.nest.duration,ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"},{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }),f.behaviour[minute]<-"nest"))
                  
                }
              } #CLOSE LOOP FOR IF PRIORITY ==1
              
              if(priority==0)
              {
                
                if(f.gut.cont[minute]>f.gut.min) 
                {ifelse(chick.cond=="critical", 
                        ifelse(m.gut.cont[minute]<=m.gut.min, f.behaviour[minute]<-"nest", ifelse(f.nest.duration>m.nest.duration,ifelse(chick.food>=chick.meal.max,f.behaviour[minute]<-"nest",{f.behaviour[minute]<-"out.flight"
                        f.trip.type<-"chick.only"}), f.behaviour[minute]<-"nest")),f.behaviour[minute]<-"nest") }        
                
                if(f.gut.cont[minute]<=f.gut.min)                                    
                {ifelse(m.gut.cont[minute]>f.gut.cont[minute],ifelse(f.nest.duration>m.nest.duration,ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"
                },{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }),ifelse(m.gut.cont[minute]<=m.gut.min,f.behaviour[minute]<-"nest",ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"
                },{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }))
                ),ifelse(m.gut.cont[minute]==f.gut.cont[minute] & f.nest.duration>m.nest.duration,ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"
                },{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }),ifelse(f.nest.duration>m.nest.duration,ifelse(chick.food>=chick.meal.max,{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"parent.only"
                },{f.behaviour[minute]<-"out.flight"
                f.trip.type<-"both"
                }),f.behaviour[minute]<-"nest")
                )
                )
                  
                }
                
              }                 #CLOSE OF IF PRIORITY ==0 LOOP
              
            }                 #CLOSE OF IF MALE AT NEST LOOP
          }                  #CLOSE OF AT NEST LOOP
          
          
          
          ################################################################################
          ##FEMALE TRIP TYPE== BOTH
          ################################################################################                  
          
          if(f.trip.type=="both")
          {  
            if(f.behaviour[minute-1]=="out.flight")      
            {
              ifelse(f.outflight.duration<f.flight.duration, f.behaviour[minute]<-"out.flight", f.behaviour[minute]<-"initial")
            }                         
            
            if(f.behaviour[minute-1]=="in.flight" )     
            {
              ifelse(f.inflight.duration<f.flight.duration, f.behaviour[minute]<-"in.flight", f.behaviour[minute]<-"nest")
            }                         
            
            
            if(f.behaviour[minute-1]=="forage" )      
            {
              ifelse(f.gut.cont[minute] < f.gut.max,
                     ifelse(f.dive.duration >= f.forage.max, {f.behaviour[minute]<-"loaf"
                     f.loaf.time<-f.inter.loaf}, f.behaviour[minute]<-"forage")    ,{f.behaviour[minute]<-"loaf"
                     f.loaf.time<-f.inter.loaf})            
            }
            
            if(f.behaviour[minute-1]=="chick.prov" )       
            {    
              ifelse(f.dive.duration >= f.forage.max, {f.behaviour[minute]<-"loaf"
              f.loaf.time<-f.end.loaf}, f.behaviour[minute]<-"chick.prov")           
            }
            
            if(f.behaviour[minute-1]=="initial")
            {
              ifelse(f.before.loaf > f.initloaf.duration, f.behaviour[minute]<-"initial",f.behaviour[minute]<-"forage") 
            }
            
            if(f.behaviour[minute-1]=="loaf")        
            {
              
              if(f.gut.cont[minute]<f.gut.min & female.res[pair,day]<f.hungry.reserve) f.meal.size<-0
              ifelse(f.meal.size>=f.gut.int|f.chk.provisioned==1,
                     ifelse(f.intbout.duration>=f.loaf.time & female.res[pair,day]>f.hungry.reserve, ifelse(f.chk.provisioned==0, f.behaviour[minute]<-"chick.prov",f.behaviour[minute]<-"in.flight" ),{f.behaviour[minute]<-"loaf"
                     f.intbout.duration<-f.intbout.duration+1}),
                     ifelse(f.intbout.duration<f.loaf.time, {f.behaviour[minute]<-"loaf" 
                     f.intbout.duration<-f.intbout.duration+1}, f.behaviour[minute]<-"forage")) 
              
            } 
            
            
            
          }            #CLOSE OF FEMALE TRIP TYPE BOTH LOOP
          
          ################################################################################
          ##FEMALE TRIP TYPE == CHICK ONLY
          ################################################################################                   
          
          if(f.trip.type=="chick.only")
          {
            if(f.behaviour[minute-1]=="out.flight")      
            {
              ifelse(f.outflight.duration<f.flight.duration, f.behaviour[minute]<-"out.flight", f.behaviour[minute]<-"initial")
            }                         
            
            if(f.behaviour[minute-1]=="in.flight" )     
            {
              ifelse(f.inflight.duration<f.flight.duration, f.behaviour[minute]<-"in.flight", f.behaviour[minute]<-"nest")
            }                         
            
            if(f.behaviour[minute-1]=="initial")
            {
              ifelse(f.before.loaf > f.initloaf.duration, f.behaviour[minute]<-"initial",f.behaviour[minute]<-"chick.prov") 
            }
            
            if(f.behaviour[minute-1]=="chick.prov" )       
            {    
              ifelse(f.dive.duration >= f.forage.max, {f.behaviour[minute]<-"loaf"
              f.loaf.time<-f.end.loaf}, f.behaviour[minute]<-"chick.prov")             
            }
            
            if(f.behaviour[minute-1]=="loaf")        
            { 
              if(f.gut.cont[minute]<f.gut.min & female.res[pair,day]<f.hungry.reserve) f.meal.size<-0
              ifelse(f.intbout.duration>=f.loaf.time, f.behaviour[minute]<-"in.flight",{f.behaviour[minute]<-"loaf"
              f.intbout.duration<-f.intbout.duration+1})   
            }
            
          }                 #CLOSE OF CHICK ONLY TRIP TYPE LOOP
          
          ################################################################################
          ##FEMALE TRIP TYPE == PARENT ONLY
          ################################################################################                        
          
          if(f.trip.type=="parent.only")
          {   
            
            if(f.behaviour[minute-1]=="out.flight")      
            {
              ifelse(f.outflight.duration<f.flight.duration, f.behaviour[minute]<-"out.flight", f.behaviour[minute]<-"initial")
            }                         
            
            if(f.behaviour[minute-1]=="in.flight" )     
            {
              ifelse(f.inflight.duration<f.flight.duration, f.behaviour[minute]<-"in.flight", f.behaviour[minute]<-"nest")
            }                         
            
            if(f.behaviour[minute-1]=="forage" )       
            {
              ifelse(f.gut.cont[minute] < f.gut.max,
                     ifelse(f.dive.duration >= f.forage.max, {f.behaviour[minute]<-"loaf"
                     f.loaf.time<-f.inter.loaf}, f.behaviour[minute]<-"forage")    ,{f.behaviour[minute]<-"loaf"
                     f.loaf.time<-f.inter.loaf})           
            }
            
            
            if(f.behaviour[minute-1]=="initial")
            {
              ifelse(f.before.loaf > f.initloaf.duration, f.behaviour[minute]<-"initial",f.behaviour[minute]<-"forage") 
            }
            
            
            if(f.behaviour[minute-1]=="loaf")        
            {
              if(f.gut.cont[minute]<f.gut.min & female.res[pair,day]<f.hungry.reserve) f.meal.size<-0
              ifelse(f.meal.size>=f.gut.int,
                     ifelse(f.intbout.duration>=f.loaf.time & female.res[pair,day]>f.hungry.reserve, f.behaviour[minute]<-"in.flight",{f.behaviour[minute]<-"loaf"
                     f.loaf.time<-f.end.loaf
                     f.intbout.duration<-f.intbout.duration+1}),
                     ifelse(f.intbout.duration<f.loaf.time, {f.behaviour[minute]<-"loaf"
                     f.loaf.time<-f.inter.loaf 
                     f.intbout.duration<-f.intbout.duration+1}, f.behaviour[minute]<-"forage"))
              
            } 
          }             #CLOSE OF FEMALE TRIP == PARENT ONLY LOOP
          
          
          
          
          ##############################################################   
          
        }     #CLOSE OF DAY LIGHT LOOP
        
        ################################################################################
        ##BEHAVIOUR DECISIONS OVERNIGHT
        ################################################################################    
        
        
        if(minute>1200) #enter loop if nightitme -- LAST 4 HOURS IN THE DAY
        {
          
          ################################################################################
          ##MALE NIGHTTIME BEHAVIOUR
          ################################################################################                            
          
          if (m.behaviour[minute-1]== "nest") m.behaviour[minute]<-"nest"   #if male was at nest last minute he will stay there
          
          if (m.behaviour[minute-1]== "in.flight")                          #if male was in flying towards nest in the previous minute will stay in flight until has reached destiation
          {
            ifelse(m.inflight.duration<m.flight.duration, m.behaviour[minute]<-"in.flight", m.behaviour[minute]<-"nest")
          }
          
          if(m.behaviour[minute-1]=="out.flight")      ##if female was in flight towards a foraging site, he stays in flight unless has reached destiation then start resting on the surface
          {
            ifelse(m.outflight.duration<m.flight.duration, m.behaviour[minute]<-"out.flight", m.behaviour[minute]<-"loaf")
          }
          
          if(m.behaviour[minute-1]=="loaf" | m.behaviour[minute-1]== "forage"|m.behaviour[minute-1]=="chick.prov") m.behaviour[minute]<-"loaf"  #if was resting on the surface or foraging at nightfall he will loaf all night
          
          if(m.behaviour[minute-1]=="initial") m.behaviour[minute]<-"loaf"   #if he was resting after just arriving at a foraging site he will continue resting
          
          ################################################################################
          ##FEMALE NIGHTTIME BEHAVIOUR
          ################################################################################    
          
          #same as above but for the female bird.
          if (f.behaviour[minute-1]== "nest") f.behaviour[minute]<-"nest"
          if (f.behaviour[minute-1]== "in.flight")
          {
            ifelse(f.inflight.duration<f.flight.duration, f.behaviour[minute]<-"in.flight", f.behaviour[minute]<-"nest")
          }
          if(f.behaviour[minute-1]=="out.flight")      
          {
            ifelse(f.outflight.duration<f.flight.duration, f.behaviour[minute]<-"out.flight", f.behaviour[minute]<-"loaf")
          }
          if(f.behaviour[minute-1]=="loaf" | f.behaviour[minute-1]== "forage" | f.behaviour[minute-1]=="chick.prov") f.behaviour[minute]<-"loaf"  
          if(f.behaviour[minute-1]=="initial") f.behaviour[minute]<-"loaf"
          
        }          #CLOSE OF OVER NIGHT LOOP
        ################################################################################
        ##DEAD
        ################################################################################   
        
        if(male.res[pair,day]<=m.dead.mass|m.collision==1)    m.behaviour[minute]<-"dead"  #if the reserve mass is below the level for the definition of a dead bird (or the bird has collded with a devices) then the behaviour is defined as dead
        if (female.res[pair,day]<=f.dead.mass|f.collision==1) f.behaviour[minute]<-"dead"    
        
        if(chick.cond=="dead") {m.behaviour[minute]<-"failed"
        f.behaviour[minute]<-"failed"}     #if the chick is dead the parents behaviour is defined as failed.
        
        
        ################################################################################
        ##UPDATING THE BEHAVIOUR COUNTERS
        ################################################################################
        
        ################################################################################
        ##MALE
        ################################################################################
        
        if (m.behaviour[minute]=="dead")     #if the male is dead then all the counters turn to zero
        {
          m.T.nest <- 0
          m.nest.duration <-0 
          m.loaf.duration<-0
          m.outflight.duration<-0
          m.inflight.duration<-0
          m.forage.duration<-0 
          m.dive.duration<-0 
          m.initloaf.duration<-0 
          m.trip.duration<-0
          
        }
        
        
        if (m.behaviour[minute]=="nest") 
        {                                         #if male is spending this minute at the nest the counter for the total time spent on nest that day increases by one
          m.T.nest<-(m.T.nest+1)
          m.nest.duration <-(m.nest.duration+1)      #the length of time have currently been on the nest is also increased by one
          m.loaf.duration<-0
          m.outflight.duration<-0                    #other duration counters reset to zero.
          m.inflight.duration<-0
          m.forage.duration<-0 
          m.dive.duration<-0  
          m.initloaf.duration<-0 
          m.trip.duration<-0
          m.e.expend[minute]<-m.Rnest*(male.res[pair, day]/1000)       #inputs the relevalent energy expended into the vector (in J)
          m.trip.type<-"none"                                     #resets the trip type
        }
        
        if (m.behaviour[minute]=="nest" & m.behaviour[minute-1]=="in.flight")  {m.bout<-m.bout+1   ##if just returned to nest add one to bout counter
        chick.food<-chick.food+m.beak.cont
        m.chk.provisioned<-0
        ifelse(m.beak.cont>0,no.chick.feeds<-no.chick.feeds+1,no.chick.feeds<-no.chick.feeds)
        m.beak.cont<-0}                       #when return to nest add food to chick and remove from beak. counter for number of chick feeds increases if the bird was carrying food
        
        if (m.behaviour[minute]=="loaf")      #if male is spending this minute loafing
        {
          m.T.loaf <- (m.T.loaf+1)             #increase the counter of total time that day spent loafing and the duration of current loafing session
          m.loaf.duration<-(m.loaf.duration+1)
          m.dive.duration<-0                     #reset the length of time have been spending in a dive bout to zero.
          m.e.expend[minute]<-m.Rloaf*(male.res[pair, day]/1000)       #inputs the relevalent energy expended into the vector (in J)
        }
        
        if (m.behaviour[minute]=="initial") 
        {                                                    #if is resting after just arriving at the foragng site
          m.T.loaf <- (m.T.loaf+1)                              #total time loafing that day increases by 1
          m.initloaf.duration<-(m.initloaf.duration+1)          #total time spent in current initial ressting period increases by 1
          m.dive.duration<-0                                #reset the length of time has been engaged in diving to zero
          m.e.expend[minute]<-f.Rloaf*(male.res[pair,day]/1000)
        }                                               #inputs the relevalent energy expended into the vector (in J)
        
        
        if (m.behaviour[minute]=="in.flight")            #if bird is spending this minute flying to nest
        {
          m.meal.size<-0                                         #reset the male meal size to 0
          m.T.flight <- (m.T.flight+1)                          #increase the counter for total time spent in flight by 1
          m.inflight.duration<-(m.inflight.duration+1)        #increases the counters for current flight duration by 1.
          m.e.expend[minute]<-(flight.cost(body.mass=(male.res[pair,day]/1000), speed=(m.speed/60),food.carried=((m.gut.cont[minute]+m.beak.cont)/1000),bmr.w=((m.BMR/60)*(male.res[pair,day]/1000)))*60)       #calculates the energy expended using the flight cost function (in watts) then inputs into the energy expended vector in J
          m.nest.duration<-0                              #resets the duration of time currently spent on nest to zero
        }
        
        if (m.behaviour[minute]=="out.flight")           #if bird is spending this minute flying to foraging site
        {
          m.T.flight <- (m.T.flight+1)                    #increases the counters for total flight duration by 1
          m.outflight.duration<-(m.outflight.duration+1)    #increases the counter for duration of current flight by 1
          m.e.expend[minute]<-(flight.cost(body.mass=(male.res[pair,day]/1000), speed=(m.speed/60),food.carried=((m.gut.cont[minute]+m.beak.cont)/1000),bmr.w=((m.BMR/60)*(male.res[pair,day]/1000)))*60)       #calculates the energy expended using the flight cost function (in watts) then inputs into the energy expended vector in J 
        } 
        
        if (m.behaviour[minute]=="loaf" & m.behaviour[minute-1]=="forage") m.intbout.duration<-1  #if is now resting on the water, but was foraging in the previous minute the counter for current resting period is restarted at 1
        
        if (m.behaviour[minute]!="loaf" & m.behaviour[minute-1]=="loaf") m.intbout.duration<-0     #if currently not resting on the water, but was the previous minute the counter for current resting is reset to 0
        
        if (m.behaviour[minute]!="nest" & minute <= 1200) m.trip.duration<-m.trip.duration+1  #if it is during the day and the bird is not at nest the trip duration counter is increased by 1
        
        if(m.behaviour[minute]=="forage") m.T.parent.only<-m.T.parent.only+1  #if this minute the male is foraging for its self the total time spent self provisioning is increased by 1
        
        if (m.behaviour[minute]=="forage"|m.behaviour[minute]=="chick.prov")                #if male is spending this minute foraging  for either his self or the chick
        {
          
          m.T.forage <- (m.T.forage+1)                     #add one to the counts for total time that day spent foraging
          m.forage.duration<-(m.forage.duration+1)          #time spent this trips foraging increases by 1
          m.dive.duration<-m.dive.duration+1                                  #duration of current diving bout increases by 1.
          m.e.expend[minute]<-m.Rforage*(male.res[pair, day]/1000)       #add in energy expended  into the vector in J
          m.intbout.duration<-0                         #counter for current period of rest reset to zero
          ifelse(rbinom(1,1,COLL.RATE)==1,m.collision<-1,m.collision<-m.collision)     #use binomial trial to determine if adult has died due to a collision.
        }
        
        if(minute==1200)                               #counts the minutes during daylight the male spends in different behaviours
        {m.Day.nest<-m.T.nest
        m.Day.loaf<-m.T.loaf
        m.Day.forage<-m.T.forage
        m.Day.flight<-m.T.flight
        }
        ################################################################################
        #CALCULATING GUT CONTENT AND CHICK MEAL SIZE
        ################################################################################
        
        ifelse(m.behaviour[minute]=="forage",
               {m.gut.cont[minute+1] <- (m.gut.cont[minute]+m.cpue-m.dig.rate)
               m.meal.size<-m.meal.size+m.cpue}, m.gut.cont[minute+1]<-m.gut.cont[minute]-m.dig.rate)
        #if adult was foraging for itsself this minute the gut content next minute is the food he caught minus digestion rate
        
        if(m.gut.cont[minute+1]<0) m.gut.cont[minute+1]<-0      #if digestion rate causes the gut content to be minus, then it is set to zero
        if(m.behaviour[minute]=="loaf" & m.behaviour[minute-1]=="chick.prov") {
          m.chk.provisioned<-1
          ifelse(rbinom(1,1,success)==1,m.beak.cont<-chick.prey.size,m.beak.cont<-0)}  ##if adult is now resting on the surface after just stopping chick provisioning and he has food in his beak (based on single binomial trial),  it is the mass of the chicks prey and has provisioned the chick
        
        ################################################################################
        ##FOR FEMALE
        ################################################################################
        
        ##as above but for the female bird
        if (f.behaviour[minute]=="dead") 
        {
          f.T.nest <- 0
          f.nest.duration <-0 
          f.loaf.duration<-0
          f.outflight.duration<-0
          f.inflight.duration<-0
          f.forage.duration<-0 
          f.dive.duration<-0 
          f.initloaf.duration<-0 
          f.trip.duration<-0
          
        }
        
        
        if (f.behaviour[minute]=="nest") 
        {
          f.T.nest <- (f.T.nest+1)
          f.nest.duration <-(f.nest.duration+1) 
          f.loaf.duration<-0
          f.outflight.duration<-0
          f.inflight.duration<-0
          f.forage.duration<-0 
          f.dive.duration<-0 
          f.initloaf.duration<-0 
          f.trip.duration<-0
          f.e.expend[minute]<-f.Rnest*(female.res[pair, day]/1000)         
        }
        
        if (f.behaviour[minute]=="nest" & f.behaviour[minute-1]=="in.flight")  {f.bout<-f.bout+1
        chick.food<-chick.food+f.beak.cont
        f.chk.provisioned<-0
        ifelse(f.beak.cont>0,no.chick.feeds<-no.chick.feeds+1,no.chick.feeds<-no.chick.feeds)
        f.beak.cont<-0}   
        
        if (f.behaviour[minute]=="loaf") 
        {
          f.T.loaf <- (f.T.loaf+1)
          f.loaf.duration<-(f.loaf.duration+1)
          f.dive.duration<-0
          f.e.expend[minute]<-f.Rloaf*(female.res[pair,day]/1000)
        }
        
        if (f.behaviour[minute]=="initial") 
        {
          f.T.loaf <- (f.T.loaf+1)
          f.initloaf.duration<-(f.initloaf.duration+1)
          f.dive.duration<-0
          f.e.expend[minute]<-f.Rloaf*(female.res[pair,day]/1000)
        }
        
        if (f.behaviour[minute]=="in.flight") 
        {
          f.T.flight <- (f.T.flight+1)
          f.inflight.duration<-(f.inflight.duration+1)
          f.e.expend[minute]<-(flight.cost(body.mass=(female.res[pair,day]/1000), speed=(f.speed/60),food.carried=((f.gut.cont[minute]+f.beak.cont)/1000),bmr.w=((f.BMR/60)*(female.res[pair,day]/1000)))*60)
          f.meal.size<-0                                                              
          f.nest.duration<-0
        }
        
        
        if (f.behaviour[minute]=="out.flight") 
        {
          f.T.flight <- (f.T.flight+1)
          f.outflight.duration<-(f.outflight.duration+1)
          f.e.expend[minute]<-(flight.cost(body.mass=(female.res[pair,day]/1000), speed=(f.speed/60),food.carried=((f.gut.cont[minute]+f.beak.cont)/1000),bmr.w=((f.BMR/60)*(female.res[pair,day]/1000)))*60)
        } 
        
        if (f.behaviour[minute]=="loaf" & f.behaviour[minute-1]=="forage") f.intbout.duration<-1    
        if (f.behaviour[minute]!="loaf" & f.behaviour[minute-1]=="loaf") f.intbout.duration<-0      
        if (f.behaviour[minute]!="nest" & minute <= 1200) f.trip.duration<-f.trip.duration+1         
        
        if(f.behaviour[minute]=="forage") f.T.parent.only<-f.T.parent.only+1
        
        if (f.behaviour[minute]=="forage"|f.behaviour[minute]=="chick.prov") 
        {
          f.T.forage <- (f.T.forage+1)
          f.forage.duration<-(f.forage.duration+1)
          f.dive.duration<-f.dive.duration+1
          f.e.expend[minute]<-f.Rforage*(female.res[pair, day]/1000)
          ifelse(rbinom(1,1,COLL.RATE)==1,f.collision<-1,f.collision<-f.collision)
        }
        
        ifelse(f.behaviour[minute]=="forage",
               {f.gut.cont[minute+1] <- (f.gut.cont[minute]+f.cpue-f.dig.rate)
               f.meal.size<-f.meal.size+f.cpue}, f.gut.cont[minute+1]<-f.gut.cont[minute]-f.dig.rate)
        
        if(f.gut.cont[minute+1]<0) f.gut.cont[minute+1]<-0  
        
        if(f.behaviour[minute]=="loaf" & f.behaviour[minute-1]=="chick.prov") {ifelse(rbinom(1,1,success)==1,f.beak.cont<-chick.prey.size,f.beak.cont<-0)   
          f.chk.provisioned<-1}
        
        if(minute==1200)                               #counts the minutes during daylight the male spends in different behaviours
        {f.Day.nest<-f.T.nest
        f.Day.loaf<-f.T.loaf
        f.Day.forage<-f.T.forage
        f.Day.flight<-f.T.flight
        }
        
        ##CHICK
        
        if(f.behaviour[minute]!="nest" & m.behaviour[minute]!="nest") {chick.alone<-chick.alone+1  
        
        ifelse(rbinom(1,1,PRED.RATE)==1,chick.cond<-"dead",chick.cond<-chick.cond)}   #if  neither parent is at the nest then the counter for the number of minutes the chick is alone increases by 1. IF NEITHER PARENT IS AT THE NEST THEN THE CHICK MIGHT GET PREDATED AND ITS CONDITION BECOMES DEAD
        
        ################################################################################ 
        
      }          #CLOSE OF MINUTE LOOP
      
      ################################################################################ 
      
      ################################################################################
      #DEFINE NIGHT TIME BEHAVIOUR
      ################################################################################ 
      
      f.night.behaviour<-f.behaviour[1440]          #sets f.night.behaviour as the value of f.behaviour in the last minute used in nest loop to determine behaviour in minute 1
      m.night.behaviour<-m.behaviour[1440]          #as above but for the male
      
      f.night.gc<-f.gut.cont[1441]             #extracts the gut content in the last minute+1 to input into the start of the next day
      m.night.gc<-m.gut.cont[1441]             #as above but for male
      
      ################################################################################
      ##CALCULATE ENERGY BALANCES
      ################################################################################
      
      ##FEMALE
      ifelse(f.night.behaviour!="dead",{ifelse(f.night.behaviour!="failed",{
        
        f.energy.expended<-sum(f.e.expend)       #the sum energy expended by the female over the day in Joules
        
        
        f.energy.gain<-f.a*(f.cpue*f.T.parent.only*prey.energy.density) #calculate the energy assimilated by female during the day  in joules  
        
        female.res[pair,(day+1)] <- female.res[pair,day] + ((f.energy.gain-f.energy.expended)/f.g)},{   #calculates the mass of the adult in the next day difference in energy gain and expended (j) divided by bird energy density of tisse J/g. gives new weight in g  
          f.energy.expended<-NA
          f.energy.gain<-NA
          female.res[pair,(day+1)]<-female.res[pair,day]}) 
        #IF THE CHICK HAS DIED THE MASS OF THE FEMALE STAYS THE SAME AND ENERGY EXPENDITURE AND GAIN ARE NA
      },{
        f.energy.expended<-NA
        f.energy.gain<-NA
        female.res[pair,(day+1)]<-0})   
      #IF THE FEMALE IS STILL ALIVE THEN THE ENERGY GAIN, EXPENDED ARE CALCULATED OVER THE DAY, OTHERWISE ARE SET TO NA. BODY MASS GOES TO ZERO IF BIRD DIES
      female.dee[pair,day]<-(f.energy.expended/1000) ##PUTS THE DAILY ENERGY EXPENDITURE (SUM OF ALL THE MINUTES) IN kJ INTO THE MATRIX
      female.gain[pair,day]<-(f.energy.gain/1000) ##PUTS THE DAILY ENERGY GAIN (ASSIMILATED ENERGY) IN kJ INTO THE MATRIX
      female.food[pair,day]<-(f.cpue*f.T.parent.only) ##PUT THE g OF FISH CONSUMED BY THE FEMALE INTO THE MATRIX
      female.bouts[pair,day]<-f.bout           #PUTS THE NUMBER OF TIMES THE FEMALE RETURNS TO THE NEST EACH DAY IN THE MATRIX
      female.per.nest[pair,day]<-(f.Day.nest/1200)*100         #PUTS THE PERCENTAGE DAYLIGHT TIME SPENT IN DIFFERENT ACTIVITIES IN THE RELEVANT MATRICES 
      female.per.rest[pair,day]<-(f.Day.loaf/1200)*100
      female.per.dive[pair,day]<-(f.Day.forage/1200)*100
      female.per.fly[pair,day]<-(f.Day.flight/1200)*100
      
      ##MALE  
      #as above but for males 
      ifelse(m.night.behaviour!="dead",{ifelse(m.night.behaviour!="failed",{
        
        m.energy.expended<-sum(m.e.expend)       #the sume energy expended by the male over the day in Joules
        
        
        m.energy.gain<-m.a*(m.cpue*m.T.parent.only*prey.energy.density) #calculate the energy assimilated by female during the day the foraging rate g/min * time spent foraging min * energy density (J/g)==answer in joules  
        
        male.res[pair,(day+1)] <- male.res[pair,day] + ((m.energy.gain-m.energy.expended)/m.g)},{   #claculates the mass of the adult in the next day difference in energy gain and expended (j) divided by bird energy density of tisse J/g. gives new weight in g  
          m.energy.expended<-NA
          m.energy.gain<-NA
          male.res[pair,(day+1)]<-male.res[pair,day]}) 
        #IF THE CHICK HAS DIED THE MASS OF THE FEMALE STAYS THE SAME AND ENERGY EXPENDITURE AND GAIN ARE NA
      },{
        m.energy.expended<-NA
        m.energy.gain<-NA
        male.res[pair,(day+1)]<-0})  
      male.dee[pair,day]<-(m.energy.expended/1000)
      male.gain[pair,day]<-(m.energy.gain/1000) 
      male.food[pair,day]<-(m.cpue*m.T.parent.only)
      male.bouts[pair,day]<-m.bout 
      male.per.nest[pair,day]<-(m.Day.nest/1200)*100
      male.per.rest[pair,day]<-(m.Day.loaf/1200)*100
      male.per.dive[pair,day]<-(m.Day.forage/1200)*100
      male.per.fly[pair,day]<-(m.Day.flight/1200)*100 
      
      ##CHICK 
      chick.energy<-(chick.prey.energy.dens/1000)*chick.food    #calculates the amount of energy in prey fed to chick in kJ
      
      ifelse(chick.cond!="dead",chick.mass[pair, day+1]<-chick.growth(prev.mass=chick.mass[pair,day], food=chick.energy),chick.mass[pair,day+1]<-0)  ##calculates chick mass using function in source file
      chick.feeds[pair,day]<-no.chick.feeds               #adds the number of times the chick was fed to the matrix
      chick.intake[pair,day]<-chick.food                  #adds the mass of food fed to the chick (g) to the matrix
      ifelse(chick.cond!="dead",chick.ab[pair,day]<-(chick.alone/1440)*100,chick.ab[pair,day]<-NA)          #adds the % time the chick is alone at the colony for the whole 24 hours
      ck.condition[pair,day]<-chick.cond
      
      fem.b[,day,pair]<-f.behaviour                 #assigns the vector of behaviours for this day to the relevant place in the array given the day number and the pair
      mal.b[,day,pair]<-m.behaviour
      fem.gc[,day,pair]<-f.gut.cont[1:1440]                 #assigns the vector of gut content for this day to the relvant place in the array given the day number and pair
      mal.gc[,day,pair]<-m.gut.cont[1:1440]
      
      ################################################################################ 
    }        #CLOSE OF DAY LOOP
    ################################################################################ 
  }        #CLOSE OF PAIR LOOP
  ################################################################################
  
  output<-list(female.res,
               male.res,
               chick.mass,
               female.dee,
               male.dee, 
               female.gain,
               male.gain,
               female.food, 
               male.food, 
               female.bouts, 
               male.bouts, 
               chick.feeds,
               chick.intake,
               female.per.nest, 
               female.per.rest, 
               female.per.dive, 
               female.per.fly,
               male.per.nest,
               male.per.rest, 
               male.per.dive, 
               male.per.fly, 
               chick.ab, 
               ck.condition, 
               fem.b, 
               mal.b,
               fem.gc,
               mal.gc)
  
  names(output)<-c("Female.Mass", "Male.Mass", "Chick.Mass", "Female.DEE", "Male.DEE","Female.EnGain", "Male.EnGain", "Female.Food", "Male.Food","Female.Bouts", "Male.Bouts","Chick.Feeds","Chick.Intake", "Female.Col", "Female.Loaf", "Female.For", "Female.Fly", "Male.Col", "Male.Loaf", "Male.For", "Male.Fly", "Chick.Alone","Chick.Condition", "F.Behaviour", "M.Behaviour", "F.GutCont", "M.GutCont")
  
  # ## Cleanup and rearrange
  # output <-  lapply(output, function(x) gather(as.data.frame(x)))
  # output <-  lapply(output, function(x) gather(as.data.frame(x)))
  # output <- lapply(output, function(x) cbind(x, ind_num = rep(1:num.pairs, length = nrow(x))))
  # output <- do.call(rbind, output)
  # output$stat <- gsub(("[[:digit:]]+"), "", rownames(output))
  # row.names(output) <- NULL
  # names(output) <- c("day", "value", "ind_num", "stat")
  # output <- output %>% 
  #   select(stat, day, ind_num, value) %>% 
  #   arrange(stat, ind_num, day) %>% 
  #   mutate(value = as.numeric(value),
  #          day = as.numeric(day))
  
  output<-output  
  
  
  
  #output is a list containing the matrices for the variables for all the pairs and the days (column numbers are days). Female.Mass is the mass of the female at the start of each day (g) calculated based on energy expenditure and gain on the previous day; Male.Mass is the mass of the male at the start of each day (g) calculated based on energy expenditure and gain on the previous day; Chick.Mass is the mass of the chick at the start of each day (g) calculated based on energy expenditure and gain on the previous day; Female.DEE is the total daily energy expenditure of the female adult (kJ);   Male.DEE is the total daily energy expenditure of the male adult (kJ);  Female.EnGain is the total energy (kJ) assimilated by a the adult during the day; Male.EnGain is the total energy (kJ) assimilated by a the adult during the day; Female.Food is the mass of food consumed by the adult female (g) during a day; Male.Food is the mass of food consumed by the adult male (g) during a day; Female.Bouts is the number of bots (times the female returned to the nest) during each day; Male.Bouts is the number of bots (times the male returned to the nest) during each day;  Chick.Feeds is the number of times the chick is fed, (sum of both parents) during each day; Chick.Intake   the total mass(g) of food fed to the chick each day (by both parents); Female.Col the % of daylight hours (20 hours of daylight) the female spends at the colony each day;  Male.Col the % of daylight hours (20 hours of daylight) the male spends at the colony each day; Female.Loaf the % of daylight hours (20 hours of daylight) the female spends resting on the water;  Male.Loaf the % of daylight hours (20 hours of daylight) the male spends resting on the water; Female.For the % daylight hours (20 hours of daylight) the female spends foraging - this includes both dives and dive pauses between dives in the same diving bout; Male.For the % daylight hours (20 hours of daylight) the male spends foraging - this includes both dives and dive pauses between dives in the same diving bout; Female.Fly the % of daylight hours (20 hours of daylight) that the female spends flying, Male.Fly the % of daylight hours (20 hours of daylight) that the male spends flying, Chick.Alone is the % time of the whole day that the chick is alone at the colony; Chick.Condition is the condition of the chick -ok, critical or dead, F.Behaviour is behaviour for each minute of each day for all pairs, M.Behaviour is the behaviour for each minute of each day for the males of al pairs, F.Gut.Cont is the gut content each minute of each day for all the females (g), M.Gut.Cont is the gut content of all the males each minute of each day for all the pairs (g).
  
  
  ################################################################################
}                    ##CLOSE OF FUNCTION
################################################################################



################################################################################
################################################################################
################################################################################
##HOW TO USE THE FUNCTION AND EXTRACT RESULTS
################################################################################
################################################################################
################################################################################


#When using the function it needs to be assigned an object name so the results can be called for in R

#set.seed(2712)
num.pairs <- 2
sim.length.days <- 22
G1<-guillemot(DAYS=sim.length.days,
              PAIRS=num.pairs) #this runs the function guillemot for 22 day time steps and 2 pairs 

## Cleanup and rearrange
G1.t <-  lapply(G1, function(x) gather(as.data.frame(x)))
G1.t <-  lapply(G1.t, function(x) gather(as.data.frame(x)))
G1.t <- lapply(G1.t, function(x) cbind(x, ind_num = rep(1:num.pairs, length = nrow(x))))
g.df <- do.call(rbind, G1.t)
g.df$stat <- gsub(("[[:digit:]]+"), "", rownames(g.df))
row.names(g.df) <- NULL
names(g.df) <- c("day", "value", "ind_num", "stat")
g.df <- g.df %>% 
  select(stat, day, ind_num, value) %>% 
  arrange(stat, ind_num, day) %>% 
  mutate(value = as.numeric(value),
         day = as.numeric(day))

## Visualize some stuff
ggplot(g.df[g.df$stat %in% c("Chick.Mass.", "Male.Mass.", "Female.Mass."),]) +
  stat_smooth(aes(y = value, x = day, color = stat))
