#> Total Length Simulator
#> Paul Rago
#> Last updated 4/6/2025
#>
#> THIS SCRIPT USES SUBROUTINES TO ADDRESS THE VARIOUS SCENARIOS
#> 1. MIN SIZE LIMIT ONLY
#> 2  MIN SIZE LIMIT PLUS BAG LIMIT
#> 3  TOTAL LENGTH LIMIT WITH OVERAGE ALLOWANCE
#> 4  TOTAL LENGTH LIMIT WITH DISCARD OF LAST FISH
#>
#>
#> LOOKS AT THE TOTAL LENGTH LIMIT + DISCARDS LAST FISH
#>
#> THis program simulates the effects of a recreational fishery controlled by
#> total length restrictions on catch per trip. The basic idea is to contol
#> bag limit on the basis of a total length limit (ie cumulative length of all
#> fish caught).  This contrasts with typical measures based on a minimum size
#> limit and a fixed  bag limit.  Under this alternative the number of fish is
#> a random process in which fishing continues until either the total allotted
#> for the trip is exceeded, or the total length limit exceded.
#> An essential feature of this alternative is that no discarding is allowed.
#> Discards may or may not be allowed depending on how the stopping rule is
#> implemented.
#>
#> Data for this exercise are based on information assembled by Lou Carr-Harris,
#> NEFSC for implementation of the Recreational Demand Model.  The size structure
#> of the population is based on outputs from the summer flounder assessment model
#> developed by Mark Terceiro.  LW parameters are taken from Wigley et al 2003.
#>
#> Results are for illustration purposes only. Theyshould be considered indicative
#> rather than definitive.
#>
#> Key model outputs include:
#>   1. Total catch in numbers and weight
#>   2. Trip Duration initial and truncated
#>   3. Realized % SPR given the scenario
#>   4. Average size of remaining spawning fish
#>   5. Discard numbers and mortality (and dead biomass)
#>   6. Total number of trips before biomass quota is reached.
#>   7. Compensating variation relative to a no-harvest scenario (CV)
#>
#>  Input/control parameters
#>  1. LW parameters
#>  2  Size frequency distribution for summer flounder
#>  3. 10,000 observations of trip duration and total catch
#>  4. Min Size limit
#>  5. Bag limit when min size limit is used
#>  6. Total length limit
#>  7. Discard mortality rate for undersized fish, or last fish in total length Limit
#>  8. Scenario type: 1=Min size,bag limit, 2= total length limit(discard last fish),
#>      3=total length limit(keep last fish even if over TLL), 4=total length limit, but
#>      the last fish is discarded.
#>  9. Preference parameters from discrete choice experiment survey -
#>     used to compute CV
#>  10. Trip costs (merged to draws of catch and trip duration)
#>
#>

# Initialize vectors

#pop.len=rep(0,N.lengths)
#pop.freq=rep(0, N.lengths)

# Read in the size freq data
wd=getwd()
ifile=paste0(wd,"/PopLenFreq.csv")
db1=read.csv(ifile,header=TRUE)

N.lengths=length(db1$Len)
pop.len=rep(0,N.lengths)
pop.freq=rep(0, N.lengths)
pop.freq.init=rep(0, N.lengths)
pop.freq.final=rep(0, N.lengths)
pop.prob=rep(0, N.lengths)
land.len=rep(0, N.lengths)
disc.len=rep(0, N.lengths)

pop.len=db1$Len          # load the lengths (inches)
pop.freq=db1$Freq        #load the estimated numbers at length
pop.freq.init=pop.freq   # save the initial distribution for comparison
pop.prob=pop.freq/sum(pop.freq)  # compute probabilities for sample selection
pop.index=seq(from=1, to=N.lengths)  # set the pointers for each length class

init.pop.size=sum(pop.freq)

# Read in the trip duration and catch data
wd=getwd()
ifile=paste0(wd,"/TripData_new.csv")
db2=read.csv(ifile,header=TRUE)

N.trips=length(db2$TripID)  # number of random draws of trips from MRIP data
trip.hrs=rep(0,N.trips)
trip.catch=rep(0,N.trips)

trip.hrs=db2$HrsFished
trip.catch=db2$Catch

# -------------------------------------------------------------------
# Welfare inputs for logsum CV (Fish vs Opt-out)
# Requires a file named 'trip_costs.xlsx' in the working directory with a column 'cost'
# Draws trip-specific preference parameters to pair with simulated trips.
# -------------------------------------------------------------------
ifile_costs <- paste0(wd, "/trip_costs.xlsx")
if(!file.exists(ifile_costs)){
  stop(paste0("Missing trip costs file: ", ifile_costs, ". Please place 'trip_costs.xlsx' in the working directory."))
}
if(!requireNamespace("readxl", quietly = TRUE)){
  stop("Package 'readxl' is required to read trip_costs.xlsx. Please install it (install.packages('readxl')).")
}

db3 <- readxl::read_excel(ifile_costs)

if(!("cost" %in% names(db3))){
  stop("trip_costs.xlsx must contain a column named 'cost'.")
}

# Add preference parameter draws (DCE estimates) aligned with each cost draw
db3$beta_sqrt_sf_keep    <- stats::rnorm(nrow(db3), mean = 0.827, sd = 1.267)
db3$beta_sqrt_sf_release <- stats::rnorm(nrow(db3), mean = 0.065, sd = 0.325)
db3$beta_opt_out         <- stats::rnorm(nrow(db3), mean = -2.091, sd = 1.983)
db3$beta_opt_out_age         <- 0.010
db3$beta_opt_out_avidity        <- -0.010

db3$beta_cost            <- -0.012

# convenience vector (optional)
trip.costs <- db3$cost
trip.ages <- db3$age
trip.avidity <- db3$total_trips_12


#  Maturation parameters
#a.mat
#b.mat
len.mat = 15   # min size (inches) at maturity.  Assume knife edge formulation.
len.mat.index=18   # index of size at maturity.

# LW parameters from Wigley et al 2003 for combined sexes, fall survey
#   where length is in cm and wgt is in kg.
a.LW=exp(-12.2841)
b.LW= 3.2156

# compute initial biomass
WAL=a.LW*(pop.len*2.54)^b.LW   # compute weight (kg) at length (g) (vector)
BAL.init=pop.freq *WAL   # compute initial biomass (kg) at length  (vector)
Btot.init=sum(BAL.init)  # initial total biomass before fishing
SSB.init=sum(BAL.init[pop.len>len.mat])  # initial SSB prior to fishing
#                            Ave weight of mature fish before fishing
Ave.wt.mat.init=sum(BAL.init[pop.len>len.mat])/sum(pop.freq[pop.len>len.mat])

# Set up the fishery control parameters
bag.limit=  5  # number of legal fish allowed per trip (not yet used in this version)
min.size=  12  # inches
TLL =      60  #   40  #60  # set equal to number of fish allowed for bag limit of min size fish
disc.mort= 0.3 # probability of post capture mortality for discarded fish

N.sim=  5000000  # number of simulated trips
#trip.dur=rep(0,N.sim)
#> Create the cdf of numbers at length based on  population structure
#>
trip.dur=rep(0,N.sim)
land.tot=rep(0,N.lengths)  # vector to accumulate numbers of landed fish by length
disc.tot=rep(0,N.lengths)  # vector to accumulate numbers of discarded fish by length
#
pop.freq
sum(pop.freq)

# load the data and parameter lists


# ----------------------------
# (Functions defined below; main simulation run moved to end)
# ----------------------------

scen.TLL.w.onediscard=function(sim.data, sim.pars){
  # load data
  trip.hrs=sim.data[[1]]
  trip.catch=sim.data[[2]]
  pop.len=sim.data[[3]]
  pop.freq=sim.data[[4]]
  WAL=sim.data[[5]]

  db3=sim.data[[6]]
  N.costdraws=nrow(db3)
  # load parameters
  len.mat.index=sim.pars[[1]]
  min.size=sim.pars[[2]]
  bag.limit=sim.pars[[3]]
  TLL=sim.pars[[4]]
  disc.mort=sim.pars[[5]]
  N.sim=sim.pars[[6]]
  SimType=sim.pars[[7]]

  # initial conditions
  pop.freq.init=pop.freq
  trip.dur=rep(0,N.sim)
  N.lengths=length(pop.len)
  land.len=rep(0, N.lengths)
  disc.len=rep(0, N.lengths)

  trip.ge.baglimit=0
  trip.gt.TLL=0      # set counter for number of trips with total length greater than TLL
  sum_dCS <- 0
  n_dCS   <- 0
  for (i in 1:N.sim){
    #> Select trip from  MRIP sample list
    j=min(round(runif(1)*N.trips)+1,N.trips)        # select a random trip index
    j.catch=trip.catch[j]           # select the catch associated with this trip
    trip.dur[i]=trip.hrs[j]  #set trip duration=actual. Decrement below if TLL is exceeded


    # Draw preference parameters + cost for this simulated trip
    pdraw <- sample.int(N.costdraws, 1L)
    cost_i <- db3$cost[pdraw]
    age_i <- db3$age[pdraw]
    avidity_i <- db3$total_trips_12[pdraw]

    b_keep <- db3$beta_sqrt_sf_keep[pdraw]
    b_rel  <- db3$beta_sqrt_sf_release[pdraw]
    b_cost <- db3$beta_cost[pdraw]
    b_oo   <- db3$beta_opt_out[pdraw]
    b_oo_age   <- db3$beta_opt_out_age[pdraw]
    b_oo_avidity   <- db3$beta_opt_out_avidity[pdraw]

    # trip-level kept/released counts under policy (total released, regardless of discard mortality)
    keep1 <- 0L
    rel1  <- 0L
    # select the lengths associated with this catch and decrement pop
    if(j.catch>0){      #if catch=zero, then this loop is bypassed
      # select set of random indices for lengths corresponding to total number caught
      trip.len=sample(pop.index,j.catch, replace=TRUE, prob=pop.prob)

      # Check if total length of catch exceeds TLL
      if( sum(pop.len[trip.len])<=TLL){     # all catch is assigned to landings
        ###
        for (k in 1:j.catch){
          pop.freq[trip.len[k]]= pop.freq[trip.len[k]] -1    # remove the fish from population
          land.tot[trip.len[k]]= land.tot[trip.len[k]] +1    # add to landings
          keep1 <- keep1 + 1L
          trip.dur[i]=trip.hrs[j]    # trip duration is unaffected
        }  # end of loop over fish caught in this random trip

      }   # end of processing for trips with total length of catch<TLL

      ###   Consider trips where the TLL is binding
      if( sum(pop.len[trip.len])>TLL){     #Last  catch is assigned to discards
        trip.gt.TLL=trip.gt.TLL+1   # increment the counter for trips with catches>TLL
        cum.len=0
        for (k in 1:j.catch){
          if(cum.len<=TLL){                    #process loop until TLL is exceeded
            pop.freq[trip.len[k]] = pop.freq[trip.len[k]] -1    # remove the fish from population
            land.tot[trip.len[k]] = land.tot[trip.len[k]] +1  # add to landings
            keep1 <- keep1 + 1L
            cum.len=cum.len+pop.len[trip.len[k]]  # add length to total
            # check if TLL is exceeded
            if(cum.len>TLL) {   # truncate the trip with this fish.  Record TLC=tot len caught
              trip.dur[i] =trip.hrs[j] * k/j.catch    # assumes equal time between events
              # Discard the last fish and determine if it survives
              index.last.fish=trip.len[k]
              # the last fish will be subtracted fro landings and possibly added to discards
              land.tot[index.last.fish]=land.tot[index.last.fish] -1   #
              keep1 <- keep1 - 1L
              rel1  <- rel1 + 1L

              x=runif(1)            # get uniform random number for disc mort test
              if (x<disc.mort){             # determine if discarded fish survives
                disc.tot[index.last.fish]=disc.tot[index.last.fish] +1  # if it dies, add to discards
              }  # end of discard test
              if(x>=disc.mort){        # if fish survives, add it back into the population
                pop.freq[index.last.fish]=pop.freq[index.last.fish] +1}  # add back alive

            }  # end of loop to deal with fate of last fish
          }  # end of truncated loop processing
        }  # end of loop over fish caught in this random trip  with tot len>TLL

      }   # end of processing for trips with total length of catch<TLL


      # Sample with replacement and weight by frequency (use prob integral transform)
      # for each length, compare to min size and discard if less.  Let discard mortality
      # be a random variable.  IF alive, add fish back to the population.  Otherwise OK
      #
      # Decrement the population for each length group removed and
      # update the sample probabilities for removals.  This is important because
      #  sampling is done without replacement.
      # -------------------------
      # Logsum CV vs baseline "no harvest" (Fish vs Opt-out)
      # Baseline: keep0=0, rel0=j.catch (total released)
      # -------------------------
      keep0 <- 0L
      rel0  <- j.catch

      vA  <- b_keep * sqrt(keep1) + b_rel * sqrt(rel1) + b_cost * cost_i
      v0  <- b_keep * sqrt(keep0) + b_rel * sqrt(rel0) + b_cost * cost_i
      vOO <- b_oo + b_oo_age * age_i +b_oo_avidity * avidity_i

      exp_vA<-exp(vA)
      exp_v0<-exp(v0)
      exp_vOO<-exp(vOO)

      logsum_alt  <- log(exp_vA+exp_vOO)
      logsum_base <- log(exp_v0+exp_vOO)

      CS_alt  <- logsum_alt/(-b_cost)
      CS_base <- logsum_base/(-b_cost)

      dCS <- CS_alt - CS_base
      sum_dCS <- sum_dCS + dCS
      n_dCS   <- n_dCS + 1L

      pop.prob=pop.freq/sum(pop.freq)  # update probabilities for sample selection
    }   # end of the loop for examining trips with non zero catches
  }   # end of the simulation loop from 1 to N.sim
  pop.freq.final=pop.freq
  mean.trip.dur=mean(trip.dur)  # this is the realized trip durations with truncations
  mean_dCS <- sum_dCS / n_dCS

  # create return list of outputs
  sim.outputs=list(land.tot,disc.tot,pop.freq.init, pop.freq.final,
                   mean.trip.dur,trip.gt.TLL,trip.ge.baglimit, mean_dCS)

  return(sim.outputs)

}  # end of subroutine  =scen.TLL.w.onediscard


scen.summary=function(SimType.output, sim.pars, sim.name, WAL,pop.len){

  # Model output
  land.tot=SimType.output[[1]]
  disc.tot=SimType.output[[2]]
  pop.freq.init=SimType.output[[3]]
  pop.freq.final=SimType.output[[4]]
  mean.trip.dur=SimType.output[[5]]
  trip.gt.TLL=SimType.output[[6]]
  trip.ge.baglimit=SimType.output[[7]]
  mean_dCS <- if(length(SimType.output) >= 8) SimType.output[[8]] else NA_real_
  # model parameters
  len.mat.index=sim.pars[[1]]
  min.size=sim.pars[[2]]
  bag.limit=sim.pars[[3]]
  TLL=sim.pars[[4]]
  disc.mort=sim.pars[[5]]
  N.sim=sim.pars[[6]]
  SimType=sim.pars[[7]]

  print("################## START OF SIMULATION OUTPUT #############")
  print(sim.name)
  print("Total Length Limit (inches)")
  print(TLL)
  print("Min Size Limit")
  print(min.size)
  print("Bag limit")
  print(bag.limit)

  print("landings (number) by length ")
  print(land.tot)
  print("total landings (number) =")
  print(sum(land.tot))
  print("total weight of landings")
  print(sum(land.tot*WAL))    # total weight of landings

  print("discard (number) by length ")
  print(disc.tot)
  print("total discard (number) =")
  print(sum(disc.tot))
  print("total weight of discard")
  print(sum(disc.tot*WAL))    # total weight of landings

  print("Ratio of landings weight to total weight caught")
  print(sum(land.tot*WAL)/(sum(land.tot*WAL)+sum(disc.tot*WAL)))

  print("Initial Pop Size Distribution")
  print(pop.freq.init)
  print("Sum of Population initial (pre fishery)")
  print (sum(pop.freq.init))
  print("Final Pop Size Distribution")
  print(pop.freq.final)
  print("Sum of population final (post fishery)")
  print(sum(pop.freq.final))

  sum(pop.freq.final)+ sum(land.tot)+sum(disc.tot)
  init.pop.size
  print ("Sum check for mass balance.  Value should =0")
  print(sum(pop.freq.final)+ sum(land.tot)+sum(disc.tot) - init.pop.size)
  print("Ratio of catch in numbers to inital pop size")
  print( (sum(land.tot)+sum(disc.tot))/(sum(pop.freq.final)+ sum(land.tot)+sum(disc.tot)))
  print(" Ratio of final population size to initial size (numbers)")
  print( sum(pop.freq.final)/sum(pop.freq.init))

  print("Biomass at Length")
  print(pop.freq.init *WAL )
  BAL.fin=pop.freq.final *WAL   # compute total biomass at length  (vector)
  print("Total Biomass initial--before fishing")
  BAL.init=pop.freq.init *WAL
  print(BAL.init)
  print("Total biomass at end of fishing")
  print(sum(BAL.fin))
  Btot.init=sum(BAL.init)
  Btot.fin=sum(BAL.fin)
  print("Ratio of final to initial Biomass")
  print(Btot.fin/Btot.init)   # fraction of initial biomass remaining

  print("SSB statistics")
  SSB.init=sum(BAL.init[pop.len>len.mat])  # SSB before fishery
  SSB.fin=sum(BAL.fin[pop.len>len.mat])  # SSB following fishery
  print("Ratio of final to initial SSB")
  print(SSB.fin/SSB.init)
  print("Ave wt of mature fish before fishery")
  #                            Ave weight of mature fish before fishing
  Ave.wt.mat.init=sum(BAL.init[pop.len>len.mat])/sum(pop.freq.init[pop.len>len.mat])

  print(Ave.wt.mat.init)   # ave wt of mature fish before fishery


  Ave.wt.mat.fin=sum(BAL.fin[pop.len>len.mat])/sum(pop.freq.final[pop.len>len.mat]) #ave wt after fishery
  # Ave.wt.mat.fin
  print("ave wt of mature fish after fishery")
  print(Ave.wt.mat.fin)   # ave wt of mature fish after fishery


  print("Total Number of Trips")
  print(N.sim)
  print(" number of trips with total length greater than TLL")
  print(trip.gt.TLL)  # number of trips with total length greater than TLL
  print("fraction of trips truncated")
  print(trip.gt.TLL/N.sim)
  print("mean trip duration in MRIP")
  print( mean(trip.hrs)) # mean trip duration in MRIP
  print("Mean trip duration under this scenario")
  print( mean.trip.dur )#realized  mean trip duration given truncation due to TLL

  print("Number of trips that reached bag limit")
  print("Mean change in consumer surplus (logsum CV), baseline=no harvest (Fish vs Opt-out)")
  print(mean_dCS)

  print(trip.ge.baglimit)

  #  return a list of variables for construction of output database

  SSB.init=sum(BAL.init[pop.len>len.mat])  # SSB before fishery
  SSB.fin=sum(BAL.fin[pop.len>len.mat])  # SSB following fishery
  #                            Ave weight of mature fish before fishing
  Ave.wt.mat.init=sum(BAL.init[pop.len>len.mat])/sum(pop.freq.init[pop.len>len.mat])
                               #ave wt  of mature fish after fishery
  Ave.wt.mat.fin=sum(BAL.fin[pop.len>len.mat])/sum(pop.freq.final[pop.len>len.mat])

  # Ave.wt.mat.fin
  sim.stats=list(
    sim.name ,
    TLL,
    min.size,
    bag.limit,
    sum(land.tot),
    sum(land.tot*WAL,    # total weight of landings
    sum(disc.tot),  #total discard (number)
    sum(disc.tot*WAL,    # total weight of landings
    sum(land.tot*WAL)/(sum(land.tot*WAL)+sum(disc.tot*WAL))), #Ratio of landings weight to total weight caught
    sum(pop.freq.init), #Sum of Population initial (pre fishery)
    sum(pop.freq.final), #Sum of population final (post fishery)
    sum(land.tot)+sum(disc.tot))/(sum(pop.freq.init)), #Ratio of catch in numbers to inital pop size
    sum(pop.freq.final)/sum(pop.freq.init), #Ratio of final population size to initial size (numbers)
    sum(pop.freq.final *WAL ),     #Total biomass at end of fishing
    sum(pop.freq.init *WAL )  ,    #"Total Biomass initial--before fishing
    sum(pop.freq.final *WAL )/sum(pop.freq.init *WAL ), #"Ratio of final to initial Biomass"
    SSB.fin/SSB.init,  #Ratio of final to initial SSB
    Ave.wt.mat.init,   # ave wt of mature fish before fishery
    Ave.wt.mat.fin,   # ave wt of mature fish after fishery
    N.sim, #Total Number of Trips")
    trip.gt.TLL,  # number of trips with total length greater than TLL
    trip.gt.TLL/N.sim, #fraction of trips truncated
    mean(trip.hrs), # mean trip duration in MRIP
    mean.trip.dur,  #realized  mean trip duration given truncation due to TLL
    trip.ge.baglimit, #Number of trips that reached bag limit
    mean_dCS #Mean change in CS (logsum CV)
    )
  return(sim.stats)

}   # end of summary function

scen.stats=function(SimType.output, sim.pars, sim.name, WAL,pop.len){

    # Model output
    land.tot=SimType.output[[1]]
    disc.tot=SimType.output[[2]]
    pop.freq.init=SimType.output[[3]]
    pop.freq.final=SimType.output[[4]]
    mean.trip.dur=SimType.output[[5]]
    trip.gt.TLL=SimType.output[[6]]
    trip.ge.baglimit=SimType.output[[7]]

  mean_dCS <- if(length(SimType.output) >= 8) SimType.output[[8]] else NA_real_
    # model parameters
    len.mat.index=sim.pars[[1]]
    min.size=sim.pars[[2]]
    bag.limit=sim.pars[[3]]
    TLL=sim.pars[[4]]
    disc.mort=sim.pars[[5]]
    N.sim=sim.pars[[6]]
    SimType=sim.pars[[7]]


    BAL.fin=pop.freq.final *WAL   # compute total biomass at length  (vector)
    BAL.init=pop.freq.init *WAL #"Total Biomass initial--before fishing"

  SSB.init=sum(BAL.init[pop.len>len.mat])  # SSB before fishery
  SSB.fin=sum(BAL.fin[pop.len>len.mat])  # SSB following fishery
  #                            Ave weight of mature fish before fishing
  Ave.wt.mat.init=sum(BAL.init[pop.len>len.mat])/sum(pop.freq.init[pop.len>len.mat])
  #ave wt  of mature fish after fishery
  Ave.wt.mat.fin=sum(BAL.fin[pop.len>len.mat])/sum(pop.freq.final[pop.len>len.mat])

  # Ave.wt.mat.fin
  # Setup a data frame and return
  #

  ss= data.frame(
    type = sim.name,
    TLL= TLL,
    min.size=min.size,
    bag.limit=bag.limit,
    land.num=sum(land.tot),
    land.wt= sum(land.tot*WAL),    # total weight of landings
    disc.num=sum(disc.tot),  #total discard (number)
    disc.wt=sum(disc.tot*WAL),    # total weight of landings
    land.catch.ratio.wt=sum(land.tot*WAL)/(sum(land.tot*WAL)+sum(disc.tot*WAL)), #Ratio of landings weight to total weight caught
    pop.init.num=sum(pop.freq.init), #Sum of Population initial (pre fishery)
    pop.final.num=sum(pop.freq.final), #Sum of population final (post fishery)
    catch.pop.ratio.num=(sum(land.tot)+sum(disc.tot))/(sum(pop.freq.init)), #Ratio of catch in numbers to inital pop size
    pop.ratio.num=sum(pop.freq.final)/sum(pop.freq.init), #Ratio of final population size to initial size (numbers)
    pop.init.wt=sum(pop.freq.init *WAL ),    #"Total Biomass initial--before fishing
    pop.final.wt=sum(pop.freq.final *WAL ),     #Total biomass at end of fishing
    pop.ratio.wt=sum(pop.freq.final *WAL)/sum(pop.freq.init *WAL), #"Ratio of final to initial Biomass"
    SSB.init=SSB.init,       # pre fishery SSB
    SSB.fin=SSB.fin,         # post fishery SSB
    SSB.ratio=SSB.fin/SSB.init,  #Ratio of final to initial SSB
    AveWtMat.init=Ave.wt.mat.init,   # ave wt of mature fish before fishery
    AveWtMat.fin=Ave.wt.mat.fin,   # ave wt of mature fish after fishery
    N.sim=N.sim, #Total Number of Trips")
    trip.gt.TLL=trip.gt.TLL,  # number of trips with total length greater than TLL
    trip.trunc.TLL=trip.gt.TLL/N.sim, #fraction of trips truncated
    aveTripHrs=mean(trip.hrs), # mean trip duration in MRIP
    aveTrip.trunc= mean.trip.dur,  #realized  mean trip duration given truncation due to TLL
    Trip.trunc.bag=trip.ge.baglimit, #Number of trips that reached bag limit
    mean_dCS #Mean change in CS (logsum CV)
  )
  return(ss)
  }  # end of sim stats

scen.plot=function(SimType.output, sim.pars, sim.name, WAL, pop.len ){
  # Model output
  land.tot=SimType.output[[1]]
  disc.tot=SimType.output[[2]]
  pop.freq.init=SimType.output[[3]]
  pop.freq.final=SimType.output[[4]]
  mean.trip.dur=SimType.output[[5]]
  trip.gt.TLL=SimType.output[[6]]
  trip.ge.baglimit=SimType.output[[7]]

  # model parameters
  len.mat.index=sim.pars[[1]]
  min.size=sim.pars[[2]]
  bag.limit=sim.pars[[3]]
  TLL=sim.pars[[4]]
  disc.mort=sim.pars[[5]]
  N.sim=sim.pars[[6]]
  SimType=sim.pars[[7]]

  if(SimType==1) {sublabel=paste("Min Size=",min.size, " Bag Lim=",bag.limit, " TLL= NA ")}
  if(SimType==2) {sublabel=paste("Min Size=",min.size, " Bag Lim= NA"," TLL= NA ")}
  if(SimType==3) {sublabel=paste("Min Size= NA", " Bag Lim=NA"," TLL= ", TLL ," TLL+1=FALSE")}
  if(SimType==4) {sublabel=paste("Min Size= NA", " Bag Lim=NA"," TLL= ", TLL ," TLL+1=TRUE")}
  print(sublabel)

  plot(pop.len, (land.tot+disc.tot)/pop.freq.init,ylim=c(0,1),
       ylab="Catch/Abundance", xlab="Length (inch)",cex.main=0.8,
       main=paste("Type=",sim.name, ": Catch/Init Abundance vs length"),
      sub=sublabel)
  abline(h=mean((disc.tot+land.tot)/pop.freq.init), col="red")

  plot(pop.len, (land.tot)/pop.freq.init,ylim=c(0,1),
       ylab="Landings/Abundance", xlab="Length (inch)",cex.main=0.8,
       main=paste("Type=",sim.name,": Landings/Init Abundance vs length"),
       sub=sublabel)  # compare len freq
  abline(h=mean((land.tot)/pop.freq.init), col="red")

  plot(pop.len, (disc.tot)/pop.freq.init,ylim=c(0,1),cex.main=0.8,
       ylab="Discards/Abundance", xlab="Length (inch)",
       main=paste("Type=",sim.name, ": Discards/Init Abundance vs length"),
       sub=sublabel)  # compare len freq
  abline(h=mean((disc.tot)/pop.freq.init), col="red")

  plot(pop.len, pop.freq.init,ylab="Abundance", xlab="Length (inch)", cex.main=0.8,
       main=paste("Type=",sim.name,":Init Abund vs Len(dot),Final(grn),land(blu),disc(red) "),
       sub=sublabel)
  lines(pop.len, pop.freq.final, col="green")
  lines(pop.len, land.tot, col="blue")
  lines(pop.len,disc.tot, col="red")

  plot(pop.len, pop.freq.init*WAL*2.54,ylab="Biomass", xlab="Length (inch)",cex.main=0.8,
       main=paste("Type=",sim.name,": Init Bio vs Len(dot),Final(grn),land(blu),disc(red) "),
       sub=sublabel)
  lines(pop.len, pop.freq.final*WAL*2.54, col="green")
  lines(pop.len, land.tot*WAL*2.54, col="blue")
  lines(pop.len, disc.tot*WAL*2.54, col="red")
  abline(h=0,col="black")

  #plot(pop.len, pop.freq.final/pop.freq.init,ylim=c(0,1.05))
 # abline(h=mean(pop.freq.final/pop.freq.init), col="red")

  plot(pop.len, pop.freq.final/pop.freq.init, ylim=c(0,1.05),
       ylab="Ratio Abundance final/init", xlab="Length (inch)",
       main=paste("Type=",sim.name,": Abund Final/Init vs Len "),
       sub=sublabel,cex.main=0.8)
  abline(h=mean(pop.freq.final/pop.freq.init), col="red")


  plot(pop.len, (pop.freq.final*WAL*2.54)/(pop.freq.init*WAL*2.54),ylim=c(0,1.05),
       ylab="Ratio Biomass final/init", xlab="Length (inch)",
       main=paste("Type=",sim.name,": Biomass Final/Init vs Len "),
       sub=sublabel,cex.main=0.8)
  abline(h=mean(pop.freq.final*WAL*2.54)/mean(pop.freq.init*WAL*2.54), col="red")


}  # end of plot subroutine

#>______________________________________
#>______________________________________
scen.TLL.wo.discards=function(sim.data, sim.pars){
  # in this scenario, no discards are allowed, but last fish may result in TLL violation
  # load data
  trip.hrs=sim.data[[1]]
  trip.catch=sim.data[[2]]
  pop.len=sim.data[[3]]
  pop.freq=sim.data[[4]]
  WAL=sim.data[[5]]

  db3=sim.data[[6]]
  N.costdraws=nrow(db3)
  # load parameters
  len.mat.index=sim.pars[[1]]
  min.size=sim.pars[[2]]
  bag.limit=sim.pars[[3]]
  TLL=sim.pars[[4]]
  disc.mort=sim.pars[[5]]
  N.sim=sim.pars[[6]]
  SimType=sim.pars[[7]]

  # initial conditions
  pop.freq.init=pop.freq
  trip.dur=rep(0,N.sim)
  N.lengths=length(pop.len)
  land.len=rep(0, N.lengths)
  disc.len=rep(0, N.lengths)

  # code from V2_TLL.R
  #
  trip.ge.baglimit=0
  trip.gt.TLL=0      # set counter for number of trips with total length greater than TLL
  sum_dCS <- 0
  n_dCS   <- 0
  for (i in 1:N.sim){
    #> Select trip from  MRIP sample list
    j=min(round(runif(1)*N.trips)+1,N.trips)        # select a random trip index
    j.catch=trip.catch[j]           # select the catch associated with this trip
    trip.dur[i]=trip.hrs[j]  #set trip duration=actual. Decrement below if TLL is exceded


    pdraw <- sample.int(N.costdraws, 1L)
    cost_i <- db3$cost[pdraw]
    age_i <- db3$age[pdraw]
    avidity_i <- db3$total_trips_12[pdraw]

    b_keep <- db3$beta_sqrt_sf_keep[pdraw]
    b_rel  <- db3$beta_sqrt_sf_release[pdraw]
    b_cost <- db3$beta_cost[pdraw]
    b_oo   <- db3$beta_opt_out[pdraw]
    b_oo_age   <- db3$beta_opt_out_age[pdraw]
    b_oo_avidity   <- db3$beta_opt_out_avidity[pdraw]

    keep1 <- 0L
    rel1  <- 0L
    # select the lengths associated with this catch and decrement pop
    if(j.catch>0){      #if catch=zero, then this loop is bypassed
      # select set of random indices for lengths corresponding to total number caught
      trip.len=sample(pop.index,j.catch, replace=TRUE, prob=pop.prob)

      # Check if total length of catch exceeds TLL
      if( sum(pop.len[trip.len])<=TLL){     # all catch is assigned to landings
        ###
        for (k in 1:j.catch){
          pop.freq[trip.len[k]]=pop.freq[trip.len[k]]-1    # remove the fish from population
          land.tot[trip.len[k]]= land.tot[trip.len[k]] +1  # add to landings
          keep1 <- keep1 + 1L
          trip.dur[i]=trip.hrs[j]    # trip duration is unaffected
        }  # end of loop over fish caught in this random trip

      }   # end of processing for trips with total length of catch<TLL

      ###   Consider trips where the TLL is binding
      if( sum(pop.len[trip.len])>TLL){     # all catch is assigned to landings
        trip.gt.TLL=trip.gt.TLL+1   # increment the counter for trips with catches>TLL
        cum.len=0
        for (k in 1:j.catch){
          if(cum.len<=TLL){   #process loop until TLL is exceeded
            pop.freq[trip.len[k]]=pop.freq[trip.len[k]]-1    # remove the fish from population
            land.tot[trip.len[k]]= land.tot[trip.len[k]] +1  # add to landings
          keep1 <- keep1 + 1L
            cum.len=cum.len+pop.len[trip.len[k]]  # add length to total
            # check if TLL is exceeded
            if(cum.len>TLL) {   # truncate the trip with this fish.  Record TLC=tot len caught
              trip.dur[i] =trip.hrs[j] * k/j.catch    # assumes equal time between events
            }
          }  # end of truncated loop processing
        }  # end of loop over fish caught in this random trip  with tot len>TLL

      }   # end of processing for trips with total length of catch<TLL


      # Sample with replacement and weight by frequency (use prob integral transform)
      # for each length, compare to min size and discard if less.  Let discard mortality
      # be a random variable.  IF alive, add fish back to the population.  Otherwise OK
      #
      # Decrement the population for each length group removed
      #
      # update the sample probabilities for removals.  This is important because
      #  sampling is done without replacement.
      # Logsum CV vs baseline "no harvest" (Fish vs Opt-out)
      keep0 <- 0L
      rel0  <- j.catch

      vA  <- b_keep * sqrt(keep1) + b_rel * sqrt(rel1) + b_cost * cost_i
      v0  <- b_keep * sqrt(keep0) + b_rel * sqrt(rel0) + b_cost * cost_i
      vOO <- b_oo + b_oo_age * age_i +b_oo_avidity * avidity_i

      exp_vA<-exp(vA)
      exp_v0<-exp(v0)
      exp_vOO<-exp(vOO)

      logsum_alt  <- log(exp_vA+exp_vOO)
      logsum_base <- log(exp_v0+exp_vOO)

      CS_alt  <- logsum_alt/(-b_cost)
      CS_base <- logsum_base/(-b_cost)

      dCS <- CS_alt - CS_base
      sum_dCS <- sum_dCS + dCS
      n_dCS   <- n_dCS + 1L

      pop.prob=pop.freq/sum(pop.freq)  # update probabilities for sample selection
    }   # end of the loop for examining trips with non zero catches
  }   # end of the simulation loop from 1 to N.sim

  pop.freq.final=pop.freq
  mean.trip.dur=mean(trip.dur)  # this is the realized trip durations with truncations
  mean_dCS <- sum_dCS / n_dCS

  # create return list of outputs
  sim.outputs=list(land.tot,disc.tot,pop.freq.init, pop.freq.final,
                   mean.trip.dur,trip.gt.TLL,trip.ge.baglimit, mean_dCS)

  return(sim.outputs)

}  # end of subroutine
#>
#>________________________________________
#>|start of min size subroutine           |
#>|_______________________________________|
scen.MinSize=function(sim.data, sim.pars){
# load code start
  # in this scenario, the minimum size limit operates but there is no bag limit
  # load data
  trip.hrs=sim.data[[1]]
  trip.catch=sim.data[[2]]
  pop.len=sim.data[[3]]
  pop.freq=sim.data[[4]]
  WAL=sim.data[[5]]

  db3=sim.data[[6]]
  N.costdraws=nrow(db3)
  # load parameters
  len.mat.index=sim.pars[[1]]
  min.size=sim.pars[[2]]
  bag.limit=sim.pars[[3]]
  TLL=sim.pars[[4]]
  disc.mort=sim.pars[[5]]
  N.sim=sim.pars[[6]]
  SimType=sim.pars[[7]]

  # initial conditions
  pop.freq.init=pop.freq
  trip.dur=rep(0,N.sim)
  N.lengths=length(pop.len)
  land.len=rep(0, N.lengths)
  disc.len=rep(0, N.lengths)
    # load code end

  # code from V1.R
  trip.ge.baglimit=0
  trip.gt.TLL=0      # set counter for number of trips with total length greater than TLL
  sum_dCS <- 0
  n_dCS   <- 0
  for (i in 1:N.sim){
    #> Select trip from  MRIP sample list
    j=min(round(runif(1)*N.trips)+1,N.trips)        # select a random trip index
    j.catch=trip.catch[j]           # select the catch associated with this trip
    trip.dur[i]=trip.hrs[j]    # trip duration is unaffected for all trips


    pdraw <- sample.int(N.costdraws, 1L)
    cost_i <- db3$cost[pdraw]
    age_i <- db3$age[pdraw]
    avidity_i <- db3$total_trips_12[pdraw]

    b_keep <- db3$beta_sqrt_sf_keep[pdraw]
    b_rel  <- db3$beta_sqrt_sf_release[pdraw]
    b_cost <- db3$beta_cost[pdraw]
    b_oo   <- db3$beta_opt_out[pdraw]
    b_oo_age   <- db3$beta_opt_out_age[pdraw]
    b_oo_avidity   <- db3$beta_opt_out_avidity[pdraw]

    keep1 <- 0L
    rel1  <- 0L
    # select the lengths associated with this catch and decrement pop
    if(j.catch>0){      #if catch=zero, then this loop is bypassed
      # select set of random indices for lengths corresponding to total number caught
      trip.len=sample(pop.index,j.catch, replace=TRUE, prob=pop.prob)
      # increment counter if total lengths on this trip exceeds TLL
      # if( sum(pop.len[trip.len])>TLL){trip.gt.TLL=trip.gt.TLL+1}  #

      # Sample with replacement and weight by frequency (use prob integral transform)
      # for each length, compare to min size and discard if less.  Let discard mortality
      # be a random variable.  IF alive, add fish back to the population.  Otherwise OK
      #
      # Decrement the population for each length group removed
      #

      for (k in 1:j.catch){
        pop.freq[trip.len[k]]=pop.freq[trip.len[k]]-1    # remove the fish from population
        if(pop.len[trip.len[k]]>=min.size){                # check if legal size

          land.tot[trip.len[k]]= land.tot[trip.len[k]] +1  # if legal, add to landings
          keep1 <- keep1 + 1L
        }  # end of test for min size
        else                                # If fish<legal size, discard
        {
        rel1 <- rel1 + 1L
        x=runif(1)
        if (x<disc.mort){                  # determine if discarded fish survives
          disc.tot[trip.len[k]]=disc.tot[trip.len[k]]+1  # if it dies, add to discards
        }  # end of discard test
        if(x>=disc.mort){        # if fish survives, add it back into the population
          pop.freq[trip.len[k]]=pop.freq[trip.len[k]] +1}  # add back alive
        }  # end of else clause for undersized fish
      }  # end of loop over fish caught in this random trip
      # update the sample probabilities for removals.  This is important because
      #  sampling is done without replacement.
      # Logsum CV vs baseline "no harvest" (Fish vs Opt-out)
      keep0 <- 0L
      rel0  <- j.catch

      vA  <- b_keep * sqrt(keep1) + b_rel * sqrt(rel1) + b_cost * cost_i
      v0  <- b_keep * sqrt(keep0) + b_rel * sqrt(rel0) + b_cost * cost_i
      vOO <- b_oo + b_oo_age * age_i +b_oo_avidity * avidity_i

      exp_vA<-exp(vA)
      exp_v0<-exp(v0)
      exp_vOO<-exp(vOO)

      logsum_alt  <- log(exp_vA+exp_vOO)
      logsum_base <- log(exp_v0+exp_vOO)

      CS_alt  <- logsum_alt/(-b_cost)
      CS_base <- logsum_base/(-b_cost)

      dCS <- CS_alt - CS_base
      sum_dCS <- sum_dCS + dCS
      n_dCS   <- n_dCS + 1L

      pop.prob=pop.freq/sum(pop.freq)  # update probabilities for sample selection
    }   # end of the loop for examining trips with non zero catches
  }   # end of the simulation loop from 1 to N.sim

  #
  #   wrap up code
  pop.freq.final=pop.freq
  mean.trip.dur=mean(trip.dur)  # this is the realized trip durations with truncations
  mean_dCS <- sum_dCS / n_dCS

  # create return list of outputs
  sim.outputs=list(land.tot,disc.tot,pop.freq.init, pop.freq.final,
                   mean.trip.dur,trip.gt.TLL,trip.ge.baglimit, mean_dCS)

  return(sim.outputs)
}  # end of subroutine

#>
#>________________________________________
#>|  end of min size subroutine           |
#>|_______________________________________|
#>

#> __________________________________________
#>|start of min size + bag limit subroutine  |
#>|__________________________________________|
#>
scen.MinSize.Bag=function(sim.data, sim.pars){
  # this routine needs a counter for number of trips in which bag limit constained
  #   catch.

  # load code start
  # in this scenario, the minimum size limit operates AND there is a bag limit
  # There is however, no truncation of trip duration

  # load data
  trip.hrs=sim.data[[1]]
  trip.catch=sim.data[[2]]
  pop.len=sim.data[[3]]
  pop.freq=sim.data[[4]]
  WAL=sim.data[[5]]

  db3=sim.data[[6]]
  N.costdraws=nrow(db3)
  # load parameters
  len.mat.index=sim.pars[[1]]
  min.size=sim.pars[[2]]
  bag.limit=sim.pars[[3]]
  TLL=sim.pars[[4]]
  disc.mort=sim.pars[[5]]
  N.sim=sim.pars[[6]]
  SimType=sim.pars[[7]]

  # initial conditions
  pop.freq.init=pop.freq
  trip.dur=rep(0,N.sim)
  N.lengths=length(pop.len)
  land.len=rep(0, N.lengths)
  disc.len=rep(0, N.lengths)
  # load code end

  # code from V1.R  + additional code for bag limit
  #  in this code it is assumed that bag limit operates by having all additional catches
  #  discarded.
  trip.ge.baglimit=0
  trip.gt.TLL=0      # set counter for number of trips with total length greater than TLL
  sum_dCS <- 0
  n_dCS   <- 0
  for (i in 1:N.sim){
    #> Select trip from  MRIP sample list
    j=min(round(runif(1)*N.trips)+1,N.trips)        # select a random trip index
    j.catch=trip.catch[j]           # select the catch associated with this trip
    trip.dur[i]=trip.hrs[j]    # trip duration is unaffected for all trips


    pdraw <- sample.int(N.costdraws, 1L)
    cost_i <- db3$cost[pdraw]
    age_i <- db3$age[pdraw]
    avidity_i <- db3$total_trips_12[pdraw]

    b_keep <- db3$beta_sqrt_sf_keep[pdraw]
    b_rel  <- db3$beta_sqrt_sf_release[pdraw]
    b_cost <- db3$beta_cost[pdraw]
    b_oo   <- db3$beta_opt_out[pdraw]
    b_oo_age   <- db3$beta_opt_out_age[pdraw]
    b_oo_avidity   <- db3$beta_opt_out_avidity[pdraw]

    keep1 <- 0L
    rel1  <- 0L
    # select the lengths associated with this catch and decrement pop
    if(j.catch>0){      #if catch=zero, then this loop is bypassed
      # select set of random indices for lengths corresponding to total number caught
      trip.len=sample(pop.index,j.catch, replace=TRUE, prob=pop.prob)
      # increment counter if total lengths on this trip exceeds TLL
      #if( sum(pop.len[trip.len])>TLL){trip.gt.TLL=trip.gt.TLL+1}  #

      # Sample with replacement and weight by frequency (use prob integral transform)
      # for each length, compare to min size and discard if less.  Let discard mortality
      # be a random variable.  IF alive, add fish back to the population.  Otherwise OK
      #
      # Decrement the population for each length group removed
      #
      #  Process this trip and compute effects of bag limit
      legal.catch.num=0
      for (k in 1:j.catch){
        pop.freq[trip.len[k]]=pop.freq[trip.len[k]]-1    # remove the fish from population

         if(pop.len[trip.len[k]]>=min.size){                # check if legal size
          legal.catch.num=legal.catch.num+1
          if(legal.catch.num<=bag.limit){
           land.tot[trip.len[k]]= land.tot[trip.len[k]] +1  # if legal and bag lim is not
           keep1 <- keep1 + 1L
           #
           #                                                  constraining, add to landings
          } # end of test for bag limit

        }  # end of test for min size
       # else                      # If fish<legal size OR fish exeeds bag limit, discard
       if((pop.len[trip.len[k]]< min.size) |(legal.catch.num>bag.limit) ){
         rel1 <- rel1 + 1L
         x=runif(1)
        if (x<disc.mort){                  # determine if discarded fish survives
          disc.tot[trip.len[k]]=disc.tot[trip.len[k]]+1  # if it dies, add to discards
        }  # end of discard test
        if(x>=disc.mort){        # if fish survives, add it back into the population
          pop.freq[trip.len[k]]=pop.freq[trip.len[k]] +1}  # add back alive
        }  # end of else clause for undersized fish
      }  # end of loop over fish caught in this random trip
      if(legal.catch.num>=bag.limit){trip.ge.baglimit=trip.ge.baglimit+1}
      # update the sample probabilities for removals.  This is important because
      #  sampling is done without replacement.
      # Logsum CV vs baseline "no harvest" (Fish vs Opt-out)
      keep0 <- 0L
      rel0  <- j.catch

      vA  <- b_keep * sqrt(keep1) + b_rel * sqrt(rel1) + b_cost * cost_i
      v0  <- b_keep * sqrt(keep0) + b_rel * sqrt(rel0) + b_cost * cost_i
      vOO <- b_oo + b_oo_age * age_i +b_oo_avidity * avidity_i

      exp_vA<-exp(vA)
      exp_v0<-exp(v0)
      exp_vOO<-exp(vOO)

      logsum_alt  <- log(exp_vA+exp_vOO)
      logsum_base <- log(exp_v0+exp_vOO)

      CS_alt  <- logsum_alt/(-b_cost)
      CS_base <- logsum_base/(-b_cost)


      dCS <- CS_alt - CS_base
      sum_dCS <- sum_dCS + dCS
      n_dCS   <- n_dCS + 1L

      pop.prob=pop.freq/sum(pop.freq)  # update probabilities for sample selection
    }   # end of the loop for examining trips with non zero catches
  }   # end of the simulation loop from 1 to N.sim

  #
  #   wrap up code
  pop.freq.final=pop.freq
  mean.trip.dur=mean(trip.dur)  # this is the realized trip durations with truncations
  mean_dCS <- sum_dCS / n_dCS

  # create return list of outputs
  sim.outputs=list(land.tot,disc.tot,pop.freq.init, pop.freq.final,
                   mean.trip.dur,trip.gt.TLL,trip.ge.baglimit, mean_dCS)

  return(sim.outputs)


}  # end of min size + bag limit routine
#>__________________________________________
#>|  end of min size + bag limit subroutine |
#>|_________________________________________|
#>

# ==========================================================
# MAIN: run scenarios (moved to end so functions are defined)
# ==========================================================

sim.data=list(trip.hrs,trip.catch,pop.len,pop.freq,WAL, db3)
#sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType)

#sim.outputs=list(land.tot,disc.tot,BAL.init,BAL.fin,pop.freq.init, pop.freq.final,
 #                mean.trip.dur,ave.len.init,ave.len.fin, ave.B.init,ave.B.final,t,
   #              ave.len.ma.init, ave.len.mat.fin)

#sim.output.stats=list(mean.trip.dur,ave.len.init,ave.len.fin, ave.B.init,ave.B.final,
 #                    ave.len.mat.init, ave.len.mat.fin)
## to add to stats, Number of shortened trips, number of maxed out bag limit trips
###  total catch in wt, total discards in wt

SimType=4  ##TOTAL LENGTH LIMIT WITH DISCARD OF LAST FISH
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType)
SimType.4.output= scen.TLL.w.onediscard(sim.data, sim.pars)
sim.name.4="TLL w.1disc"
scen.summary(SimType.4.output, sim.pars,sim.name.4,WAL,pop.len)
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
scen.plot(SimType.4.output, sim.pars, sim.name.4, WAL, pop.len )


SimType=3  ##TOTAL LENGTH LIMIT WITH  NO DISCARD OF LAST FISH
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType)
SimType.3.output= scen.TLL.wo.discards(sim.data, sim.pars)
sim.name.3="TLL wo.disc"
scen.summary(SimType.3.output, sim.pars,sim.name.3,WAL,pop.len)
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
scen.plot(SimType.3.output, sim.pars, sim.name.3, WAL, pop.len )


SimType=2  ##MIN SIZE RESTRICTION BUT NO BAG LIMIT
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType)
SimType.2.output= scen.MinSize(sim.data, sim.pars)
sim.name.2="min size only"
scen.summary(SimType.2.output, sim.pars,sim.name.2,WAL,pop.len)
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
scen.plot(SimType.2.output, sim.pars, sim.name.2, WAL, pop.len )

SimType=1  ##MIN SIZE RESTRICTION AND BAG LIMIT
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType)
SimType.1.output= scen.MinSize.Bag(sim.data, sim.pars)
sim.name.1="minSize+BagLim"
scen.summary(SimType.1.output, sim.pars,sim.name.1,WAL,pop.len)
sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
scen.plot(SimType.1.output, sim.pars, sim.name.1, WAL, pop.len )
s.s=scen.stats(SimType.1.output, sim.pars, sim.name.1, WAL,pop.len)

# test  loop over multiple bag limits'
bag.limit.set=rep(0,10)
bag.limit.set=c(3,4,5,6,7,8,9,10,20,50)

min.size.set=rep(0,6)
min.size.set=c(12,15,17,19,21,23)
i.loop=0
for(i.b in 1:10){
  bag.limit=bag.limit.set[i.b]  # set the bag limit for this run

# test loop for multiple min size limits
for (i.s in 1:6){
    min.size= min.size.set[i.s]  # set the min size for this run
    TLL=NA
    SimType=1
    sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
    SimType=1 ##MIN SIZE RESTRICTION AND BAG LIMIT
    SimType.1a.output= scen.MinSize.Bag(sim.data, sim.pars)
    sim.name.1=paste0("minSize+BagLim:",min.size.set[i.s],"_",bag.limit.set[i.b])
    scen.plot(SimType.1a.output, sim.pars, sim.name.1, WAL, pop.len )
    s.s=scen.stats(SimType.1a.output, sim.pars, sim.name.1, WAL,pop.len)
    i.loop=i.loop+1
    if(i.loop==1){s.s.all=s.s}
    if(i.loop>1){s.s.all=rbind.data.frame(s.s.all,s.s)}

    }  # end of loop over min size increase
  #
}   # end of loop over bag limit

# test loop for multiple total Length limits with 1 discard
TLL.set=rep(0,7)
TLL.set=c(30,45,60,75,100,150,200)
for (i.s in 1:7){
  TLL= TLL.set[i.s]
  min.size=NA
  bag.limit=NA
  SimType=4  ##TOTAL LENGTH LIMIT WITH DISCARD OF LAST FISH
  sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
  SimType.4a.output= scen.TLL.w.onediscard(sim.data, sim.pars)
  sim.name.4= paste0("TLL w.1disc_" ,TLL.set[i.s])
 # scen.summary(SimType.4a.output, sim.pars,sim.name.4,WAL,pop.len)
  sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
  scen.plot(SimType.4a.output, sim.pars, sim.name.4, WAL, pop.len )
  s.s=scen.stats(SimType.4a.output, sim.pars, sim.name.4, WAL,pop.len)
 # if(i.s==1){s.s.all=s.s}
  if(i.s>0){s.s.all=rbind.data.frame(s.s.all,s.s)}

}  # end of loop over various TLL with 1 discard allowed


# Test loop for TLL with no discards
TLL.set=rep(0,7)
TLL.set=c(30,45,60,75,100,150,200)
for (i.s in 1:7){
  TLL= TLL.set[i.s]
  min.size=NA
  bag.limit=NA
  SimType=3  ##TOTAL LENGTH LIMIT WITH  NO DISCARD OF LAST FISH
  sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
  SimType.3a.output= scen.TLL.wo.discards(sim.data, sim.pars)
  sim.name.3= paste0("TLL wo.disc_" ,TLL.set[i.s])
  # scen.summary(SimType.4a.output, sim.pars,sim.name.4,WAL,pop.len)
  sim.pars=list(len.mat.index,min.size,bag.limit,TLL, disc.mort,N.sim,SimType) # update pars
  scen.plot(SimType.3a.output, sim.pars, sim.name.3, WAL, pop.len )
  s.s=scen.stats(SimType.3a.output, sim.pars, sim.name.3, WAL,pop.len)
  # if(i.s==1){s.s.all=s.s}
  if(i.s>0){s.s.all=rbind.data.frame(s.s.all,s.s)}

}  # end of loop over various TLL without discards


###_________________________________________
###_________________________________________

# end of simulation experiments for TLL with no discards

# Write out the results of the simulation experiments
wd=getwd()
outfile=paste0(wd,"/Model.experiments_4.csv")
#write.csv(s.s.all,outfile,header=TRUE)
write.csv(s.s.all,outfile)
db.out=read.csv(outfile,header=TRUE)  # read in saved simulation file


# wd=getwd()
# outfile=paste0(wd,"/Model.experiments_3.csv")
# db.out=read.csv(outfile,header=TRUE)  # read in saved simulation file

#Compute derived variables from simulation outputs

db.out$catch.wt=db.out$land.wt+db.out$disc.wt
db.out$catch.num=db.out$land.num+db.out$disc.num

# number landed, discarded and caught per hr fished
db.out$num.land.phr=db.out$land.num/(db.out$aveTripHrs* db.out$N.sim)
db.out$num.disc.phr=db.out$disc.num/(db.out$aveTripHrs* db.out$N.sim)
db.out$num.catch.phr=db.out$catch.num/(db.out$aveTripHrs* db.out$N.sim)

# weight landed, discarded and caught per hr fished
db.out$wt.land.phr=db.out$land.wt/(db.out$aveTripHrs* db.out$N.sim)
db.out$wt.disc.phr=db.out$disc.wt/(db.out$aveTripHrs* db.out$N.sim)
db.out$wt.catch.phr=db.out$catch.wt/(db.out$aveTripHrs* db.out$N.sim)


# SUMMARY PLOTS FOR THE SIMULATION EXPERIMENTS

#EFFECTS OF TLL ON LANDINGS, DISCARD AND CATCH

plot(db.out$TLL,db.out$land.wt,
     main="Landings(dot) and Disc(line) in Wt vs TLL",
     ylab="Total landigs (kg)", xlab="Total Length Limit (inches)",
     ylim=c(0,max(db.out$land.wt)), pch=db.out$TLL.disc)
lines(db.out$TLL[db.out$TLL.disc==1],db.out$disc.wt[db.out$TLL.disc==1], col="red",lty=1)
lines(db.out$TLL[db.out$TLL.disc==0],db.out$disc.wt[db.out$TLL.disc==0], col="blue",lty=2)


plot(db.out$TLL[db.out$TLL.disc==1],db.out$catch.pop.ratio.num[db.out$TLL.disc==1],
     main="Exploit. Ratio(#) = Catch/Init Pop Size", col="red",
     ylab="Catch/Pop (number)", xlab="Total Length Limit")
points(db.out$TLL[db.out$TLL.disc==0],db.out$catch.pop.ratio.num[db.out$TLL.disc==0], col="blue")


plot(db.out$min.size,db.out$catch.pop.ratio.num,
     main="Expl.Ratio(#)=Catch/InitPop v minSize (dots); TLL=200[r],60[b]",
     ylab="Catch/Pop (number)", xlab="Min Size Limit(inches)")
abline(h=db.out$catch.pop.ratio.num[db.out$TLL==200], col="red")
abline(h=db.out$catch.pop.ratio.num[db.out$TLL==60], col="blue")

# COMPARISON OF TLL PLOTS WITH MIN SIZE LIMIT AND BAG LIMITS

plot(db.out$min.size,db.out$land.wt, xlab="Min Size Limit (inches)",
     ylab="Landings (wt)", main="Landings vs MinSize(dots), TLL=200[r],100[g],60[b](lines)")
abline(h=db.out$land.wt[db.out$TLL==200], col="red")
abline(h=db.out$land.wt[db.out$TLL==100], col="green")
abline(h=db.out$land.wt[db.out$TLL==60], col="blue")

plot(db.out$min.size,db.out$land.wt/db.out$land.num,
     main=" mean wt landed fish vs min size limit; TLL=lines ",
     ylab="Mean Wt (kg) of landed fish", xlab="Min Sixe Limit (inches)")
abline(h=db.out$land.wt[db.out$TLL==200]/db.out$land.num[db.out$TLL==200], col="red")
abline(h=db.out$land.wt[db.out$TLL==100]/db.out$land.num[db.out$TLL==100], col="green")
abline(h=db.out$land.wt[db.out$TLL==60]/db.out$land.num[db.out$TLL==60], col="blue")

plot(db.out$min.size, db.out$disc.wt/db.out$disc.num,
     main="Ave wt disc v minSize (dots), TLL=200[r],100[g],60[b](lines)",
     ylab="Ave Wt Disc (kg)", xlab="Min Size Limit (inches)")
abline(h=db.out$disc.wt[db.out$TLL==200&db.out$TLL.disc==1]/
         db.out$disc.num[db.out$TLL==200&db.out$TLL.disc==1], col="red")
abline(h=db.out$disc.wt[db.out$TLL==100&db.out$TLL.disc==1]/
         db.out$disc.num[db.out$TLL==100&db.out$TLL.disc==1], col="green")
abline(h=db.out$disc.wt[db.out$TLL==60&db.out$TLL.disc==1]/
         db.out$disc.num[db.out$TLL==60&db.out$TLL.disc==1], col="blue")


# FISHERY PERFORMANCE PLOTS--CATCH RATES PER HOUR
# compute landings per hour fished, disc/hr,  catch/hr
plot(db.out$min.size,db.out$land.wt/(db.out$aveTripHrs* db.out$N.sim),
     main=" Ave weight landed fish/hr vs min size limit",
     ylab="Ave Wt (kg) landed/hr", xlab="Min Size Limit (inches)")


plot(db.out$min.size,db.out$land.num/(db.out$aveTripHrs* db.out$N.sim),
     main="# landed/hr vs min size limit; TLL=200[r],100[g],30[b](lines)",
     ylab="Number landed per hr fished", xlab="Min Size Limit (inches)")
abline(h=db.out$num.land.phr[db.out$TLL.disc==1&db.out$TLL==200], col="red")
abline(h=db.out$num.land.phr[db.out$TLL.disc==1&db.out$TLL==100], col="green")
abline(h=db.out$num.land.phr[db.out$TLL.disc==1&db.out$TLL==30], col="blue")

plot(db.out$min.size,db.out$disc.num/(db.out$aveTripHrs* db.out$N.sim),
     main="# disc/hr vs min size limit; TLL=200[r],100[g],30[b](lines)",
     ylab="Number disc per hr fished", xlab="Min Size Limit (inches)")
abline(h=db.out$num.disc.phr[db.out$TLL.disc==1&db.out$TLL==200], col="red")
abline(h=db.out$num.disc.phr[db.out$TLL.disc==1&db.out$TLL==100], col="green")
abline(h=db.out$num.disc.phr[db.out$TLL.disc==1&db.out$TLL==30], col="blue")

plot(db.out$min.size,db.out$catch.wt/(db.out$aveTripHrs* db.out$N.sim),
     main=" weight catch fish/hr vs min size limit",
     ylab="Catch wt/hr fish (kg/hr)", xlab="Min Size Limit (in)")
abline(h=db.out$landed.wt.phr.fished[db.out$TLL==200], col="blue")
abline(h=db.out$landed.wt.phr.fished[db.out$TLL==60], col="red")

# TRADEOFF PLOTS LANDINGS VS DISCARDS
xmax=max(db.out$land.wt)
plot(db.out$land.wt[is.na(db.out$TLL)], db.out$disc.wt[is.na(db.out$TLL)],
     main=("Tradeoff disc v land for minlen and baglim; TLL+1=red"),
     xlab="Total Landings (kg)", ylab="Total Discards (kg)",
     ylim=c(0,max( db.out$disc.wt[is.na(db.out$TLL)])),
     xlim=c(0,xmax))
# points(db.out$land.wt[!is.na(db.out$TLL)], db.out$disc.wt[!is.na(db.out$TLL)],col="red")
points(db.out$land.wt[db.out$TLL.disc==1], db.out$disc.wt[db.out$TLL.disc==1],
       pch=18, col="red")

plot(db.out$min.size[db.out$bag.limit==50],db.out$land.catch.ratio.wt[db.out$bag.limit==50],
     ylim=c(0,1.05), main="Landings/Catch (wt) vs Min Size by bag lim", ylab="Landings/Catch (wt)")
lines(db.out$min.size[db.out$bag.limit==20],db.out$land.catch.ratio.wt[db.out$bag.limit==20],
      pch=19, col="red", type="b")
lines(db.out$min.size[db.out$bag.limit==3],db.out$land.catch.ratio.wt[db.out$bag.limit==3],
      pch=19, col="blue", type="b")
abline(h=db.out$land.catch.ratio.wt[db.out$TLL==100], col="green")
abline(h=db.out$land.catch.ratio.wt[db.out$TLL==60], col="purple")

# EFFECTS OF REGULATIONS ON SSB AND AVERAGE SIZE OF MATURE FISH

plot(db.out$min.size,db.out$SSB.ratio, ylim=c(0,1.05),
     main="SSB ratio vs Min Size(dots); TLL=200[r],100[g],60[b](lines)",
     xlab="Min Size Limit (inches)", yla="SSB Ratio (final/init)")
abline(h=db.out$SSB.ratio[db.out$TLL==200], col="red")
abline(h=db.out$SSB.ratio[db.out$TLL==100], col="green")
abline(h=db.out$SSB.ratio[db.out$TLL==60], col="blue")

plot(db.out$TLL[db.out$TLL.disc==1],
     db.out$AveWtMat.fin[db.out$TLL.disc==1]/db.out$AveWtMat.init[db.out$TLL.disc==1],
     main="Ratio Ave Wt Mat Final/AveWtMat Init", col="red",ylim=c(0.95,1.05),
     ylab="Ratio AveWtMat final/init", xlab="Total Length Limit")
points(db.out$TLL[db.out$TLL.disc==0],
       db.out$AveWtMat.fin[db.out$TLL.disc==0]/db.out$AveWtMat.init[db.out$TLL.disc==0], col="blue")
abline(h=1)

plot(db.out$min.size,
     db.out$AveWtMat.fin/db.out$AveWtMat.init,
     main="AveWtMat Final/Init vs Min Size(dots); TLL=200[r],30[b] (line)",
     col="red",ylim=c(0.90,1.1),
     ylab="Ratio AveWtMat final/init", xlab="Min Size Limit (inches)")
abline(h= db.out$AveWtMat.fin[db.out$TLL.disc==0]/db.out$AveWtMat.init[db.out$TLL.disc==0])
abline(h= db.out$AveWtMat.fin[db.out$TLL.disc==1&db.out$TLL==200]/
         db.out$AveWtMat.init[db.out$TLL.disc==1&db.out$TLL==200],
       col="red")
abline(h= db.out$AveWtMat.fin[db.out$TLL.disc==1&db.out$TLL==30]/
         db.out$AveWtMat.init[db.out$TLL.disc==1&db.out$TLL==30],
       col="blue")



######################################################################
# <<<<<MISC PLOTS >>>>>
######################################################################
# measures of trip limit caps
plot(db.out$TLL, db.out$aveTrip.trunc)
plot(db.out$bag.limit, db.out$Trip.trunc.bag)
plot(db.out$TLL, db.out$trip.gt.TLL)
plot(db.out$TLL, db.out$trip.trunc.TLL)

plot(db.out$TLL,db.out$disc.wt,main="Discard in Wt vs TLL")
points(db.out$TLL[db.out$TLL.disc==1],db.out$disc.wt[db.out$TLL.disc==1], col="blue",pch=1)
points(db.out$TLL[db.out$TLL.disc==0],db.out$disc.wt[db.out$TLL.disc==0], col="red",pch=2)
abline(h=0, col="green")


plot(db.out$TLL,db.out$SSB.ratio,main="SSB ratio (final/init) vs TLL",
     ylab="SSB ratio (final/init)", xlab="Total Length Limit (inches)")
plot(db.out$min.size,db.out$SSB.ratio, main="SSB ratio (final/init) vs Min Size",
     ylab="SSB ratio (final/init)", xlab="Min Size Limit (inches)")


plot(db.out$min.size,db.out$AveWtMat.fin/db.out$AveWtMat.init,
     main="Ave Wt Mature (final/init) vs min Size (dots); TLL=100[r],60[b] ",
     ylab="Ave Wt Mature (final/init)", xlab="Min Size Limit (inches)")
abline(h=db.out$AveWtMat.init[db.out$TLL==100]/db.out$AveWtMat.fin[db.out$TLL==100], col="red")
abline(h=db.out$AveWtMat.init[db.out$TLL==60]/db.out$AveWtMat.fin[db.out$TLL==60], col="blue")



plot(db.out$TLL,db.out$land.catch.ratio.wt)


plot(db.out$min.size,db.out$disc.wt/(db.out$aveTripHrs* db.out$N.sim),
     main=" weight disc fish/hr vs min size limit")
plot(db.out$min.size,db.out$disc.num/(db.out$aveTripHrs* db.out$N.sim),
     main=" number disc fish/hr vs min size limit")

plot(db.out$min.size,db.out$disc.wt/(db.out$aveTripHrs* db.out$N.sim),
     main=" weight DISC fish/hr vs min size limit",
     ylab="DISC wt/hr fish (kg/hr)", xlab="Min Size Limit (in)")
abline(h=db.out$disc.wt.phr.fished[db.out$TLL==200], col="blue")
abline(h=db.out$disc.wt.phr.fished[db.out$TLL==60], col="red")


abline(h=db.out$catch.wt[db.out$TLL==200]/(
         db.out$aveTrip.trunc[db.out$TLL==200]*db.out$N.sim[db.out$TLL==200])
         , col="red")
abline(h=db.out$catch.wt[db.out$TLL==100]/(
         db.out$aveTrip.trunc[db.out$TLL==100]*db.out$N.sim[db.out$TLL==100])
       , col="green")
abline(h=db.out$catch.wt[db.out$TLL==60]/ (
         db.out$aveTrip.trunc[db.out$TLL==60]*db.out$N.sim[db.out$TLL==60])
       , col="blue")

plot(db.out$min.size,db.out$catch.num/(db.out$aveTripHrs* db.out$N.sim),
     main=" number catch fish/hr vs min size limit")

plot(db.out$min.size,db.out$disc.wt)
abline(h=db.out$disc.wt[db.out$TLL==200], col="red")
abline(h=db.out$disc.wt[db.out$TLL==100], col="green")
abline(h=db.out$disc.wt[db.out$TLL==60], col="blue")



plot(db.out$min.size,db.out$catch.pop.ratio.num )
plot(db.out$land.wt[!is.na(db.out$TLL)], db.out$disc.wt[!is.na(db.out$TLL)],
       main=("Tradeoff between discards v landings for TLL and TLL+1"))

##############################################################################
#     SUBROUTINES
# ############################################################################


