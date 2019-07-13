#####################################################################
#####################################################################

## Crude pseudo-code outline for Manxie ABM

## Function start
{
  ## Define the environmnent (distance to short/long waters, productivity)
  
  ## Define pairs (number)
  
  ## For each pair
  {
    
    ## Set mass of adults
    
    ## Set chick mass
    
    ## For each day
    {
      
      ## For each individual
      {
        
        ## If above mass threshold, go on a short trip: 
        
        ## If on short trip gain/lose x amount. Amount gained/lost will depend on distance/productivity/flight cost. Based on the new mass, either forage another day or provision
        
        ## If on a 'long trip' - mass gain/loss will vary by day. It is not immediately clear to me what the best way to account for this is when the time steps are whole days.
        ## Individuals will stay on a long trip until they cross some upper threshold.
        
      }
      
      ## At the 'end' of each day, each individual will be either foraging or provisioning. 
      ## Which parent provisions first will be random. The amount each individual provisions will be based on their success while foraging
      
      ## The mass of each individual will be updated and the 
      
    }
    
  }
  
}