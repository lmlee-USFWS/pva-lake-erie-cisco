#==============================================================================================================================
#=CUSTOM BETA DISTRIBUTION=====================================================================================================
#==============================================================================================================================
#Function to estimate beta distribution parameters and return random value from distribution
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  sim.beta <- rbeta(1,alpha,beta)
    return(params = list(alpha = alpha, beta = beta, sim.beta = sim.beta))
}

#==============================================================================================================================
#=END CUSTOM BETA DISTRIBUTION=================================================================================================
#==============================================================================================================================

#==============================================================================================================================
#=GENERATE LIFE HISTORY PARAMETERS=============================================================================================
#==============================================================================================================================
lifehistory.sims <- function(n.simulations,age,vonB,n.vonB,lw,n.lw,fecund,n.fecund,
                             earlyS,fishery.use,F.partial,customS.use,S.custom)
{


#-Create Objects for Storing Results-------------------------------------------------------------------------------------------
sim.bio <- data.frame(matrix(nrow=n.simulations,ncol=11))
colnames(sim.bio) <- c("Linf","K","t0","lw.a","lw.b","f.a","f.b","f.type","eggsurvival","frysurvival","age0survival")

sim.length <- data.frame()
sim.weight <- data.frame()
sim.natmort <- data.frame()
sim.survival <- data.frame()
sim.fecundity <- data.frame()
sim.eggS <- data.frame()
sim.fryS <- data.frame()
sim.age0S <- data.frame()


#-Get Life History Parameters for Simulations----------------------------------------------------------------------------------
set.seed(8675309)
for (j in 1:n.simulations) {

	# Set von Bertalanffy age-length parameters
  pick.vonB <- sample(1:n.vonB,1,replace=FALSE)
  Linf <- as.numeric(vonB[pick.vonB,1])
  K <- as.numeric(vonB[pick.vonB,2])
  t0 <- as.numeric(vonB[pick.vonB,3])
#  Linf <- 403.71934
#  K <- 0.22703
#  t0 <- -2.45121

	# Set length-weight parameters
  pick.lw <- sample(1:n.lw,1,replace=FALSE)
  lw.a <- as.numeric(lw[pick.lw,1])
  lw.b <- as.numeric(lw[pick.lw,2])
#  lw.a <- 0.000001276696
#  lw.b <- 3.348761


	# Set fecundity-weight parameters
	pick.fecund <- sample(1:n.fecund,1,replace=FALSE)
	f.a <- as.numeric(fecund[pick.fecund,1])
	f.b <- as.numeric(fecund[pick.fecund,2])
	f.type <- fecund[pick.fecund,3]
#  f.a <- -440.35
#  f.b <- 44.48
#  f.type <- "weight"

	
  # early life survival
	eggS.mu <- as.numeric(earlyS[1,2])
	eggS.var <- as.numeric(earlyS[1,3])
	fryS.mu <- as.numeric(earlyS[2,2])
	fryS.var <- as.numeric(earlyS[2,3])
	age0S.mu <- as.numeric(earlyS[3,2])
	age0S.var <- as.numeric(earlyS[3,3])

	# Set egg survival
	eggS <- estBetaParams(eggS.mu,eggS.var)$sim.beta
#  eggS <- 0.0865
	
	# Set fry survival
  fryS <- estBetaParams(fryS.mu,fryS.var)$sim.beta
#  fryS <- 0.0611
	
	# Set age-0 survival
	age0S <- estBetaParams(age0S.mu,age0S.var)$sim.beta
#  age0S <- 0.0556
	

	# Compute Growth, Fecundity, Mortality, and Survival
	length <- Linf * (1 - exp(-K * (age - t0)))
	weight <- lw.a * length ^ lw.b


	if (f.type == "weight") {
		fecundity <- f.a + (f.b * weight)
	} else {
		fecundity <- 10^((f.b * (length/10)) + f.a)
	}

	fecundity <- abs(fecundity)
	
	
	natmort <- 3 * (weight^-0.288)
	
	Mchange <- runif(1, -0.30, 0.30)
	natmort <- as.double(natmort + (natmort * Mchange))


  if (fishery.use == "Yes") {
    Z.mort <- natmort + F.partial
  } else {
    Z.mort <- natmort
  }
	
	if (customS.use == "Default") {
	  survival <- exp(-Z.mort)
	} else {
	  survival <- S.custom
	}

	# Store Life History Parameters
	sim.bio[j,1] <- Linf
	sim.bio[j,2] <- K
	sim.bio[j,3] <- t0
	sim.bio[j,4] <- lw.a
	sim.bio[j,5] <- lw.b
	sim.bio[j,6] <- f.a
  sim.bio[j,7] <- f.b
	sim.bio[j,8] <- f.type
  sim.bio[j,9] <- eggS
  sim.bio[j,10] <- fryS
  sim.bio[j,11] <- age0S

	# Store Derived Parameters
	sim.length <- rbind(sim.length,length)
  sim.weight <- rbind(sim.weight,weight)
	sim.natmort <- rbind(sim.natmort,natmort)
	sim.survival <- rbind(sim.survival,survival)
	sim.fecundity <- rbind(sim.fecundity,fecundity)
	sim.eggS <- rbind(sim.eggS,eggS)
	sim.fryS <- rbind(sim.fryS,fryS)
	sim.age0S <- rbind(sim.age0S,age0S)

}


#-Change Column Names for Stored Derived Parameters----------------------------------------------------------------------------
colnames(sim.length) <- sprintf("Age%02d",1:max(age))
colnames(sim.weight) <- sprintf("Age%02d",1:max(age))
colnames(sim.natmort) <- sprintf("Age%02d",1:max(age))
colnames(sim.survival) <- sprintf("Age%02d",1:max(age))
colnames(sim.fecundity) <- sprintf("Age%02d",1:max(age))


#-Save Results-----------------------------------------------------------------------------------------------------------------
return(list(sim.bio = sim.bio, sim.length = sim.length, sim.weight = sim.weight,
			sim.natmort = sim.natmort, sim.survival = sim.survival,
			sim.fecundity = sim.fecundity, sim.eggS = sim.eggS, sim.fryS = sim.fryS,
			sim.age0S = sim.age0S))
}


#==============================================================================================================================
#=END GENERATE LIFE HISTORY PARAMETERS=========================================================================================
#==============================================================================================================================

#==============================================================================================================================
#=RUN SIMULATIONS==============================================================================================================
#==============================================================================================================================
sims.run <- function(n.projections,n.simulations,age,sexratio,maturity,sim.bio,sim.survival,sim.weight,sim.fecundity,
                     sim.eggS,sim.fryS,sim.age0S,stocking.pro,area.lake,thresh.allee,boom.use,int.boom,boom.inc)

{

#-Create Objects for Storing Results-------------------------------------------------------------------------------------------
sim.pop <- matrix(nrow=n.projections,ncol=n.simulations)
sim.popwt <- matrix(nrow=n.projections,ncol=n.simulations)
sim.wild <- matrix(nrow=n.projections,ncol=n.simulations)
sim.stocked <- matrix(nrow=n.projections,ncol=n.simulations)
sim.hatchery <- matrix(nrow=n.projections,ncol=n.simulations)
sim.recruits <- data.frame()
sim.wildage1 <- data.frame()

sim.S1 <- matrix(nrow=n.projections,ncol=n.simulations)

age.mature.pop <- matrix(nrow=max(age)+1,ncol=n.projections)
age.mature.hatchery <- matrix(nrow=max(age)+1,ncol=n.projections)

sim.mature.pop <- data.frame()
sim.mature.hatchery <- data.frame()

sim.other <- matrix(nrow=n.projections,ncol=n.simulations)
sim.mature.other <- matrix(nrow=n.projections,ncol=n.simulations)

lambda.wild <- data.frame()
changes.all <- data.frame()


#-Initiate Progress Bar--------------------------------------------------------------------------------------------------------
pb <- txtProgressBar(min = 0,      			# Minimum value of the progress bar
                     max = n.simulations,   	# Maximum value of the progress bar
                     style = 3,    			# Progress bar style (also available style = 1 and style = 2)
                     width = 50,   			# Progress bar width. Defaults to getOption("width")
                     char = "=")   			# Character used to create the bar


#-Simulations------------------------------------------------------------------------------------------------------------------
set.seed(8675309)
for (j in 1:n.simulations) {

  incProgress(1/n.simulations, detail = paste("Doing simulation", j))
  Sys.sleep(0.1)

	# Mx for Wild Fish Leslie Matrix
#	mx <- sexratio * maturity * as.double(sim.fecundity[j,]) * sim.bio[j,9] * sim.bio[j,10] * sim.bio[j,11]
  fecundity <- as.double(sim.fecundity[j,])
  
	# Mx for Stocked Fish Leslie Matrix
	mx.stocked <- rep(0,max(age))


	# Survival of Adult Wild Fish
  Sw <- as.numeric(sim.survival[j,])


	# Weight at Age
	waa <- sim.weight[j,]

	# Survival for Eggs
	Segg <- sim.eggS[j,]

	# Survival for Fry
	Sfry <- sim.fryS[j,]

	# Survival for Age 0
	Sage0 <- sim.age0S[j,]


	# Run Population Projections
	p <- pop.stockpro(n.projections, sexratio, maturity, fecundity, mx.stocked, Sw, waa, area.lake, thresh.allee,
	                  Segg, Sfry, Sage0, stocking.pro, boom.use, int.boom, boom.inc)

	# Store Projection Results
	sim.pop[,j] <- p$pop.sizes
	sim.popwt[,j] <- p$pop.weight
	sim.wild[,j] <- p$stage.wild
	sim.stocked[,j] <- p$stage.stocked
	sim.hatchery[,j] <- p$stage.hatchery

	sim.recruits <- rbind(sim.recruits,p$pop.recruits)
	sim.wildage1 <- rbind(sim.wildage1,p$pop.wildage1)

	sim.S1[,j] <- p$S1

	age.mature.pop <- (p$stage.vectors * maturity)
	age.mature.hatchery <- (p$hatchery.vectors * maturity)

	lambda.wild <- rbind(lambda.wild,p$lambda.Awild)
	changes.all <- rbind(changes.all,p$pop.changes)

	# Determine Number Spawners
	sim.mature.pop <- rbind(sim.mature.pop,colSums(age.mature.pop))
	sim.mature.hatchery <- rbind(sim.mature.hatchery,colSums(age.mature.hatchery))


	# Progress Bar
	setTxtProgressBar(pb, j)

	
}

close(pb)


sim.popwt <- sim.popwt/1000000	#==>convert to metric tons

sim.other <- sim.pop - sim.hatchery
sim.mature.other <- sim.mature.pop - sim.mature.hatchery


#-Save Results-----------------------------------------------------------------------------------------------------------------
return(list(sim.pop = sim.pop, sim.popwt = sim.popwt, sim.wild = sim.wild, sim.stocked = sim.stocked,
			sim.hatchery = sim.hatchery, sim.recruits = sim.recruits, sim.wildage1 = sim.wildage1,
			sim.S1 = sim.S1, sim.mature.pop = sim.mature.pop,
			sim.mature.hatchery = sim.mature.hatchery,lambda.wild = lambda.wild,
			sim.other = sim.other, sim.mature.other = sim.mature.other, changes.all = changes.all))
}


#==============================================================================================================================
#=END RUN SIMULATIONS==========================================================================================================
#==============================================================================================================================

#==============================================================================================================================
#=LESLIE MATRIX POPULATION PROJECTIONS=========================================================================================
#==============================================================================================================================
pop.stockpro <- function(n.projections, sexratio, maturity, fecundity, mx.stocked, Sw, waa, area.lake, thresh.allee,
                         Segg, Sfry, Sage0, stocking.pro, boom.use, int.boom, boom.inc )
{

	n.age <- length(maturity)
	n.stockpro <- ncol(stocking.pro) - 2
	x <- length(Sw)
	t <- n.projections

  n.wild <- matrix(numeric(x * t), nrow = x)
  n.stocked.juv <- numeric(t)
  n.stocked <- matrix(numeric(x * t), nrow = x)
	n.ss <- matrix(numeric(x * t), nrow = x)
	n.hatchery <- matrix(numeric(x * t), nrow = x)
	n.mature <- matrix(numeric(x * t), nrow = x)
	n <- matrix(numeric(x * t), nrow = x)
	wt <- matrix(numeric(x * t), nrow = x)

	S1 <- numeric(t)
	stage <- matrix(numeric(x * t), nrow = x)
	pop <- numeric(t)
	pop.wt <- numeric(t)
	pop.wild <- numeric(t)
  pop.stocked <- numeric(t)
	pop.ss <- numeric(t)
	pop.hatchery <- numeric(t)
	pop.recruits <- numeric(t)
	pop.wildage1 <- numeric(t)
	pop.mature <- numeric(t)

	lambda.Awild <- numeric(t)
	change <- numeric(t - 1)

	pop.thresh <- area.lake * thresh.allee

	# process stocked early life stages
	if (any(stocking.pro$age < 1)) {
	  for (i in 1:t) {
	    if (i <= n.stockpro) {
	      if (i == 1) {
	        n.stocked.juv[i] <- 0

	      } else {

	        stocking.juv <- subset(stocking.pro,stocking.pro$age < 1)
	        stocking.juv <- stocking.juv[,c(1,2,i+2)]

	        colnames(stocking.juv) <- c("age","month","nums")

	        stocking.juv$nums <- stocking.juv$nums * 0.5        # assume half are female

	        # monthly survival for wild egg, fry, and age 0
	        eggS.wild.month <- Segg
	        fryS.wild.month <- Sfry^(1/5)
	        age0S.wild.month <- Sage0^(1/6)

	        # wild stage survival
	        stocking.juv <- stocking.juv %>% mutate(S.wild = case_when(
	          age == -2 ~ eggS.wild.month ^ 1,
	          age == -1 ~ fryS.wild.month ^ (7-month),
	          age == 0 ~ age0S.wild.month ^ (12-month)))

	        stocking.juv$S.hatch <- stocking.juv$S.wild / 2       #half wild survival

	        # age-1 equivalents
	        stocking.juv <- stocking.juv %>% mutate(age1.eq = case_when(
	          age == -2 ~ Sfry * Sage0 * nums * S.hatch,
	          age == -1 ~ Sage0 * nums * S.hatch,
	          age == 0 ~ nums * S.hatch))	        

	        age1.eq.tot <- sum(stocking.juv$age1.eq)

	        n.stocked.juv[i] <- age1.eq.tot
	        n.stocked.juv[i] <- n.stocked.juv[i] + (n.stocked.juv[i] * runif(1, -0.25, 0.25))

	      }
	    } else {
	      n.stocked.juv[i] <- 0
	    }
	  }
	} else {
	  n.stocked.juv[1:t] <- 0
	}


	for (i in 1:t) {

	  if (i <= n.stockpro && any(stocking.pro$age > 0)) {
	    stocking.adult <- subset(stocking.pro,stocking.pro$age > 0)
      n.var <- runif(n.stockpro, -0.25, 0.25)
      stocking.adult[-1:-2] <- stocking.adult[-1:-2] + (stocking.adult[-1:-2] * n.var)
	  } else {
	    stocking.adult <- data.frame(matrix(nrow = 1, ncol = n.stockpro + 2))
	    stocking.adult[1,1] <- 1
	    stocking.adult[1,2] <- 1
	    stocking.adult[1,3:length(stocking.adult)] <- 0
	    colnames(stocking.adult)[1] <- "age"
	    colnames(stocking.adult)[2] <- "month"

	    yrnum <- sprintf("yr%d",seq(1:n.stockpro))
	    colnames(stocking.adult)[3:length(stocking.adult)] <- yrnum
	  }

    df.age <- as.data.frame(1:n.age)
    colnames(df.age) <-"age"
    stocking.adult <- merge(df.age,stocking.adult,by="age",all=TRUE)
    stocking.adult <- stocking.adult %>% replace(is.na(.), 0)

    
    # boom dynamics
    p.boom <- 1/int.boom
    fryS.inc <- boom.inc/100

    rnd.boom <- runif(1)
    if (boom.use == "Yes") {
      if (rnd.boom < p.boom) {
        Sfry.boom <- Sfry * (fryS.inc + 1)
      } else {
        Sfry.boom <- Sfry
      }
    } else {
      Sfry.boom <- Sfry
    }

    
    mx <- sexratio * maturity * fecundity * Segg * Sfry.boom * Sage0
    
   # Schange <- runif(1, -0.30, 0.30)
	 # lx <- as.double(Sw + (Sw * Schange))
    lx <- Sw
		mx.new <- mx * lx
		A.wild <- make_leslie_mpm(survival = lx, fecundity = mx.new, n_stages = n.age, split = FALSE)
		#A.wild[n.age,n.age] <- 0	#==>comment out if using a plus group
		lambda.Awild[i] <- lambda(A.wild)

#    Sh <- runif(1, 0, 0.5)		#survival immediately after stocking

		lx.month <- lx ^ (1/12)
		n.month.remain <- 12 - stocking.adult$month - 1
		lx.partial <- lx.month ^ n.month.remain
		lx.stocked <- lx.partial / 2
		A.stocked <- make_leslie_mpm(survival = lx.stocked, fecundity = mx.stocked, n_stages = n.age, split = FALSE)
		#A.stocked[n.age,n.age] <- 0	#==>comment out if using a plus group

		# stocked survivors
		A.ss <- make_leslie_mpm(survival = lx, fecundity = mx.stocked, n_stages = n.age, split = FALSE)
		#A.ss[n.age,n.age] <- 0		#==>comment out if using a plus group

		# recruits of stocked fish
		A.sr <- make_leslie_mpm(survival = 0, fecundity = mx.new, n_stages = n.age, split = FALSE)
		#A.sr[n.age,n.age] <- 0		#==>comment out if using a plus group

		if (i == 1) {
			n.ss[,i] <- 0
			n.wild[,i] <- 0
		} else {
			n.ss[,i] <- (A.stocked %*% n.stocked[,i-1]) + (A.ss %*% n.ss[,i-1])
			n.wild[,i] <- (A.wild %*% n.wild[,i-1]) + (A.sr %*% n.ss[,i-1])			
			
			n.ss[1,i] <- n.ss[1,i] + n.stocked.juv[i]
		}


		if (i <= n.stockpro + 1) {
			if (i == n.stockpro + 1) {
				n.stocked[,i] <- 0
			} else {
#				n.stocked[,i] <- stocking.pro[,i+1] * Sh
				n.stocked[,i] <- stocking.adult[,i+2] * 0.5     # assume half are female
				
			}
		} else {
			n.stocked[,i] <- 0
		}

		# allee effect
		if(i > 1) {
			if (pop.mature[i-1] <= pop.thresh) {
				n.wild[1,i] <- 0
			} else {
				n.wild[1,i] <- n.wild[1,i]
			}
		}

		
		n.hatchery[,i] <- n.ss[,i] + n.stocked[,i]
		n[,i] <- n.wild[,i] + n.hatchery[,i]
		wt[,i] <- n[,i] * as.double(waa)
		stage[,i] <- n[,i]
		stage <- stage %>% replace(is.na(.), 0)
		S1[i] <- lx[1]
		pop.recruits[i] <- n[1,i]
		pop.wildage1[i] <- n.wild[1,i]
		n.mature[,i] <- stage[,i] * maturity
		
		pop.mature[i] <- sum(n.mature[,i])
		
	}

	pop <- colSums(stage)
	pop.wild <- colSums(n.wild)

	for (i in 1:t) {
      	if (i > 1) {
      		change[i - 1] <- pop.wild[i]/pop.wild[i - 1]
      	}
	}

	
	pop.stocked <- colSums(n.stocked)
	pop.ss <- colSums(n.ss)
	pop.hatchery <- colSums(n.hatchery)
	pop.wt <- colSums(wt)

	w <- stage[, t]


	pop.proj <- list(lambda = pop[t]/pop[t - 1], lambda.Awild = lambda.Awild, stable.stage = w/sum(w),
        			stage.vectors = stage, pop.sizes = pop, pop.changes = change,
				      stage.wild = pop.wild, stage.stocked = pop.stocked,
				      stage.hatchery = pop.hatchery, hatchery.vectors = n.hatchery,
				      pop.weight = pop.wt, S1 = S1, pop.recruits = pop.recruits,
				      pop.wildage1 = pop.wildage1,
				      Segg = Segg,Sfry=Sfry,Sage0=Sage0)
}


#==============================================================================================================================
#=END LESLIE MATRIX POPULATION PROJECTIONS=====================================================================================
#==============================================================================================================================

#==============================================================================================================================
#=COMPUTE SUMMARY STATISTICS===================================================================================================
#==============================================================================================================================
stats.calc <- function(n.projections,n.simulations,n.stockpro,lambda.wild,changes.all,sim.pop,sim.mature.pop,sim.hatchery,sim.other,
				sim.mature.hatchery,sim.mature.other,sim.popwt)

{


#------------------------------------------------------------------------------------------------------------------------------
#-COMPUTE POST-STOCKING LAMBDA FOR EACH SIMULATION (WILD FISH ONLY)------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#-Pull Out Lambdas for Post-Stocking Years-------------------------------------------------------------------------------------
changes.ps <- changes.all[,-1:-(n.stockpro-1)]

#-Replace 0 Values with NA-----------------------------------------------------------------------------------------------------
#changes.ps[changes.ps==0] <- NA

#-Calculate Overall Lambda for Each Sim as Geometric Mean----------------------------------------------------------------------
lambdasim.ps <- geometricmeanRow(changes.ps)

#-For calculation of Number of Runs where Lambda Equal to or Exceeding 1-------------------------------------------------------
copy.lambdasim.ps <- as.data.frame(lambdasim.ps)
colnames(copy.lambdasim.ps) <- c("lambda")

copy.lambdasim.ps$exceed <- ifelse(copy.lambdasim.ps$lambda >= 1, 1, 0)
n.exceed <- sum(copy.lambdasim.ps$exceed)
percent.exceed <- round((n.exceed/n.simulations)*100,digits=1)


#-Calculate Summary Statistics for Post-Stocking Lambda------------------------------------------------------------------------
lambda.stats <- quantile(lambdasim.ps,probs=c(0.05,0.25,0.50,0.75,0.95),na.rm=TRUE)
tlambda.stats <- t(lambda.stats)
tlambda.stats <- as.data.frame(tlambda.stats)
colnames(tlambda.stats) <- c("P05","P25","Median","P75","P95")


#------------------------------------------------------------------------------------------------------------------------------
#-COMPUTE POPULATION STATISTICS FOR EACH SIMULATION----------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#-Median Population Size by Year Over All Simulations--------------------------------------------------------------------------
sim.pop <- as.data.frame(sim.pop)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(sim.pop) <- simnum

pop.quantile <- apply(sim.pop, 1, quantile, na.rm=TRUE)
tpop.quantile <- t(pop.quantile)
tpop.quantile <- as.data.frame(tpop.quantile)
tpop.quantile$year <- 1:n.projections
colnames(tpop.quantile) <- c("P0","P25","Median","P75","P100","Year")

#-same as above for specialized plotting
pop.allquantile <- apply(sim.pop, 1, quantile, probs=c(0.05,0.15,0.25,0.35,0.45,0.50,0.55,0.65,0.75,0.85,0.95), na.rm=TRUE)
tpop.allquantile <- t(pop.allquantile)
tpop.allquantile <- as.data.frame(tpop.allquantile)
tpop.allquantile$year <- 1:n.projections
colnames(tpop.allquantile) <- c("P05","P15","P25","P35","P45","Median","P55","P65","P75","P85","P95","Year")


#-Terminal Year Population Size Over All Simulations---------------------------------------------------------------------------
end.pop <- sim.pop[n.projections,]
end.stats <- quantile(end.pop,probs=c(0.05,0.25,0.50,0.75,0.95),na.rm=TRUE)
tend.stats <- t(end.stats)
tend.stats <- as.data.frame(tend.stats)
colnames(tend.stats) <- c("P05","P25","Median","P75","P95")


#-Median Mature Population Size by Year Over All Simulations-------------------------------------------------------------------
sim.mature.pop <- as.data.frame(sim.mature.pop)
tsim.mature.pop <- t(sim.mature.pop)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(tsim.mature.pop) <- simnum

mature.quantile <- apply(tsim.mature.pop, 1, quantile, na.rm=TRUE)
tmature.quantile <- t(mature.quantile)
tmature.quantile <- as.data.frame(tmature.quantile)
tmature.quantile$year <- 1:n.projections
colnames(tmature.quantile) <- c("P0","P25","Median","P75","P100","Year")


#-Terminal Year Mature Population Size Over All Simulations--------------------------------------------------------------------
endmat.pop <- tsim.mature.pop[n.projections,]
endmat.stats <- quantile(endmat.pop,probs=c(0.05,0.25,0.50,0.75,0.95),na.rm=TRUE)
tendmat.stats <- t(endmat.stats)
tendmat.stats <- as.data.frame(tendmat.stats)
colnames(tendmat.stats) <- c("P05","P25","Median","P75","P95")


#-Median Population Size of Each Origin by Year Over All Simulations-----------------------------------------------------------
sim.hatchery <- as.data.frame(sim.hatchery)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(sim.hatchery) <- simnum

hatchery.quantile <- apply(sim.hatchery, 1, quantile, na.rm=TRUE)
thatchery.quantile <- t(hatchery.quantile)
thatchery.quantile <- as.data.frame(thatchery.quantile)
thatchery.quantile$year <- 1:n.projections
colnames(thatchery.quantile) <- c("P0","P25","Median","P75","P100","Year")
thatchery.quantile$Origin <- "Hatchery"

sim.other <- as.data.frame(sim.other)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(sim.other) <- simnum

other.quantile <- apply(sim.other, 1, quantile, na.rm=TRUE)
tother.quantile <- t(other.quantile)
tother.quantile <- as.data.frame(tother.quantile)
tother.quantile$year <- 1:n.projections
colnames(tother.quantile) <- c("P0","P25","Median","P75","P100","Year")
tother.quantile$Origin <- "Wild"

pop.origin.quantile <- rbind(thatchery.quantile,tother.quantile)


#-Median Mature Population Size of Each Origin by Year Over All Simulations----------------------------------------------------
sim.mature.hatchery <- as.data.frame(sim.mature.hatchery)
tsim.mature.hatchery <- t(sim.mature.hatchery)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(tsim.mature.hatchery) <- simnum

mature.hatchery.quantile <- apply(tsim.mature.hatchery, 1, quantile, na.rm=TRUE)
tmature.hatchery.quantile <- t(mature.hatchery.quantile)
tmature.hatchery.quantile <- as.data.frame(tmature.hatchery.quantile)
tmature.hatchery.quantile$year <- 1:n.projections
colnames(tmature.hatchery.quantile) <- c("P0","P25","Median","P75","P100","Year")
tmature.hatchery.quantile$Origin <- "Hatchery"

sim.mature.other <- as.data.frame(sim.mature.other)
tsim.mature.other <- t(sim.mature.other)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(tsim.mature.other) <- simnum

mature.other.quantile <- apply(tsim.mature.other, 1, quantile, na.rm=TRUE)
tmature.other.quantile <- t(mature.other.quantile)
tmature.other.quantile <- as.data.frame(tmature.other.quantile)
tmature.other.quantile$year <- 1:n.projections
colnames(tmature.other.quantile) <- c("P0","P25","Median","P75","P100","Year")
tmature.other.quantile$Origin <- "Wild"

mature.origin.quantile <- rbind(tmature.hatchery.quantile,tmature.other.quantile)


#-Terminal Year Mature Population Size by Origin Over All Simulations----------------------------------------------------------
endmat.hatchery <- tsim.mature.hatchery[n.projections,]
endmat.hatchery.stats <- quantile(endmat.hatchery,probs=c(0.05,0.25,0.50,0.75,0.95),na.rm=TRUE)
tendmat.hatchery.stats <- t(endmat.hatchery.stats)
tendmat.hatchery.stats <- as.data.frame(tendmat.hatchery.stats)
colnames(tendmat.hatchery.stats) <- c("P05","P25","Median","P75","P95")
tendmat.hatchery.stats$Origin <- "Hatchery"

endmat.other <- tsim.mature.other[n.projections,]
endmat.other.stats <- quantile(endmat.other,probs=c(0.05,0.25,0.50,0.75,0.95),na.rm=TRUE)
tendmat.other.stats <- t(endmat.other.stats)
tendmat.other.stats <- as.data.frame(tendmat.other.stats)
colnames(tendmat.other.stats) <- c("P05","P25","Median","P75","P95")
tendmat.other.stats$Origin <- "Wild"

end.origin.stats <- rbind(tendmat.hatchery.stats,tendmat.other.stats)


#-Median Population Weight (mt) by Year Over All Simulations-------------------------------------------------------------------
sim.popwt <- as.data.frame(sim.popwt)
simnum <- sprintf("sim%d",seq(1:n.simulations))
colnames(sim.popwt) <- simnum

popwt.quantile <- apply(sim.popwt, 1, quantile, na.rm=TRUE)
tpopwt.quantile <- t(popwt.quantile)
tpopwt.quantile <- as.data.frame(tpopwt.quantile)
tpopwt.quantile$year <- 1:n.projections
colnames(tpopwt.quantile) <- c("P0","P25","Median","P75","P100","Year")


#------------------------------------------------------------------------------------------------------------------------------
#-COMPUTE EXTINCTION PROBABILITY-----------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
pop.final <- tail(sim.pop, n=1)
tpop.final <- t(pop.final)
colnames(tpop.final) <- c("PopSize")
tpop.final <- as.data.frame(tpop.final)

tpop.final$change <- ifelse(tpop.final$PopSize < 1, 0, 1)
persist <- sum(tpop.final$change)

extinct <- n.simulations - persist

p.extinct <- extinct / n.simulations


#-Save Results-----------------------------------------------------------------------------------------------------------------
return(list(lambdasim.ps = lambdasim.ps, tlambda.stats = tlambda.stats, tend.stats = tend.stats, tendmat.stats = tendmat.stats,
            extinct = extinct, p.extinct = p.extinct, percent.exceed = percent.exceed,
            tpop.quantile = tpop.quantile, tpop.allquantile = tpop.allquantile, pop.origin.quantile = pop.origin.quantile,
            tmature.quantile = tmature.quantile, mature.origin.quantile = mature.origin.quantile,
            tpopwt.quantile = tpopwt.quantile, end.origin.stats = end.origin.stats))
}


#==============================================================================================================================
#=END COMPUTE SUMMARY STATISTICS===============================================================================================
#==============================================================================================================================
