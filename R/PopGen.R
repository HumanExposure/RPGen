# Population Generator for EPA's RPGen
# Designed and written by WGG at ICF, July 18, 2019
# Updated by AE of ORAU, 2020.

# Randomization and Sorting:

#' Splits a character into a vector of character values of length two.
#' 
#' @param   str a character
#' @return  a character vector with length two

splitpairs = function(str) {
  n <- str_length(str)/2
  x <- substr(str,1,2)
  if (n>1) { 
    for (i in 2:n) {
      x <- c(x,substr(str,2*i-1,2*i))
    }
  }
  return(x)
}

#' Removes commas from a character.
#' 
#' @param   x a character
#' @return  a numeric vector without commas. 

string.values = function(x){
  
  y <- str_replace_all(x,","," ")
  w <- lapply(str_split(y," "),as.numeric)[[1]]
  v <- w[!is.na(w)]
  return(v)
}

#' Returns a vector of cumulative probability 
#' 
#' Prints error code if probability is less than 0.
#' 
#' @param   x a vector of numerics
#' @return  a numeric vector with the cumulative probability. 

cumul.prob = function(x) {
  if (min(x)<0) cat("Negative values in probability vector")
  y <- cumsum(x)
  return(y/max(y))
}

#' Creates random values from seed. 
#' 
#' Takes in a dataframe and selects n number of random values
#' as specified by seed and a list of guiding
#' variables (varlist). Contains a flag for changing seeds in a sensitivity analysis; flag
#' is not called in general use of RPGen. 
#' 
#' @param   var      name of dataframe
#' @param   n        number of individuals to be selected
#' @param   seeds    seeds used to drive random number selection
#' @param   varlist  parameters to narrow selection criteria in PUMS
#' @param   flag     Used to hold certain seeds constant
#' @return  a numeric vector with the cumulative probabily

get.randoms = function(var,n,seeds,varlist,flag) {
  if(flag==0) off <- 0 else off <- 1
  b <- 2*match(var,varlist)-1+off
  if(off==0) off<- -1
  if(is.na(b)) stop(var," not found")
  set.seed(seeds[b:(b-off)],"Marsaglia-Multicarry")
  return(runif(n))
}

#' Creates multiple seeds to be used in random number generation 
#' 
#' The generation of multiple seeds allows for more explicit control  
#' of random number generation for sensitivity analysis. 
#' 
#' @param   init.seed  The initial seed.
#' @param   num        The number of seeds to create, based on data inputs. 
#' @return  two seeds for each dataset input.  

get.seeds = function(init.seed,num) {
  n     <- 2*num
  base  <- as.integer64(2147483647)
  mult  <- as.integer64(397204094)
  seeds <- vector("integer",n)
  x     <- as.integer64(init.seed)
  for (i in 1:n) {
    x <- (x * mult) %% base
    seeds[i] <- as.numeric(x)
  }
  return(seeds)
}

#' Selects random users from PUMS input
#' 
#' Finds the cumulative probability using PUMS' pwgtp weighting variable.
#' Selects random users using the cumulative sum interval using get.randoms. 
#' This is done for the 
#' @param   pums       The pums dataframe
#' @param   num        The number of seeds to create, based on data inputs. 
#' @return  a dataframe with n number of observations, randomly selected,
#'          but by weighted probability and with a set seed.   

gen.pop = function(pums) {
  u.pop   <- get.randoms("pums",g$num.persons,g$seeds,g$var.list,0)
  cp.pop  <- cumul.prob(pums$pwgtp)
  return( 1+findInterval(u.pop,cp.pop,rightmost.closed=TRUE))
}

#' Finds the interval between two numeric vectors
#' 
#' @param   p a numeric vector
#' @param   q a numeric vector
#' @return  a numeric vector with the interval between p and q.   

sampleq = function(p,q){
  cp.x <- cumul.prob(p)
  sel.x <- 1+findInterval(q,cp.x)
  return(sel.x)
}

#' Generates two sets of random numbers that are bivariate-normal correlated taking a vector of quantiles as inputs    
#' This is used to generate heights and weights
#' 
#' @param   q a dataframe
#' @param   px a vector with values 0 and 1
#' @param   py a vector with values 0 and 1
#' @param   rho zero, unless changed by sigma and px,py
#' @param   means the first observation in the vector is px, and second py
#' @param   sigma used to create variation in equation below
#' @return  a vector of cumulative densities

bi_norm_cor = function(q,px=c(0,1),py=c(0,1),rho=0,means=c(0,0),sigma=NULL) {
  if (!is.null(sigma)) {
    px[1] <- means[1]
    py[1] <- means[2]
    px[2] <- sigma[1,1]^0.5
    py[2] <- sigma[2,2]^0.5
    rho   <- sigma[1,2]/(px[2]*py[2])
  }
  x   <- qnorm(q[,1],mean=px[1],sd=px[2])
  y   <- qnorm(q[,2],mean=py[1]+rho*py[2]*(x-px[1])/px[2],sd=py[2]*(1-rho^2)^0.5)
  return(cbind(x,y))
}


#' Adjusts the weight of physiological parameters per weight adjustment from httk.
#' 
#' @param   y a dataframe with physiological parameters 
#' @return  a dataframe with physiological parameters adjusted in accordance with weight's adjustment. 

adjust_weight = function(y) {
  adj <- y$weight / y$weight_adj
  y[, `:=`(Blood_mass, Blood_mass*adj)]
  y[, `:=`(Brain_mass, Brain_mass*adj)]
  y[, `:=`(Gonads_mass, Gonads_mass*adj)]
  y[, `:=`(Heart_mass, Heart_mass*adj)]
  y[, `:=`(Kidneys_mass, Kidneys_mass*adj)]
  y[, `:=`(Large_intestine_mass, Large_intestine_mass*adj)]
  y[, `:=`(Liver_mass, Liver_mass*adj)]
  y[, `:=`(Lung_mass, Lung_mass*adj)]
  y[, `:=`(Muscle_mass, Muscle_mass*adj)]
  y[, `:=`(Pancreas_mass, Pancreas_mass*adj)]
  y[, `:=`(Skeleton_mass, Skeleton_mass*adj)]
  y[, `:=`(Skin_mass, Skin_mass*adj)]
  y[, `:=`(Small_intestine_mass, Small_intestine_mass*adj)]
  y[, `:=`(Spleen_mass, Spleen_mass*adj)]
  y[, `:=`(Stomach_mass, Stomach_mass*adj)]
  y[, `:=`(Adipose_mass, Adipose_mass*adj)]
  y[, `:=`(Other_mass, Other_mass*adj)]
  y[, `:=`(Adipose_flow, Adipose_flow*adj)]
  y[, `:=`(Brain_flow, Brain_flow*adj)]
  y[, `:=`(Gonads_flow, Gonads_flow*adj)]
  y[, `:=`(Heart_flow, Heart_flow*adj)]
  y[, `:=`(Kidneys_flow, Kidneys_flow*adj)]
  y[, `:=`(Large_intestine_flow, Large_intestine_flow*adj)]
  y[, `:=`(Liver_flow, Liver_flow*adj)]
  y[, `:=`(Lung_flow, Lung_flow*adj)]
  y[, `:=`(Muscle_flow, Muscle_flow*adj)]
  y[, `:=`(Pancreas_flow, Pancreas_flow*adj)]
  y[, `:=`(Skeleton_flow, Skeleton_flow*adj)]
  y[, `:=`(Skin_flow, Skin_flow*adj)]
  y[, `:=`(Small_intestine_flow, Small_intestine_flow*adj)]
  y[, `:=`(Spleen_flow, Spleen_flow*adj)]
  y[, `:=`(Stomach_flow, Stomach_flow*adj)]
  return(y)
}


# File reading:

#' Reads and stores user input
#' 
#' Displays error codes when entries outside of the bounds are entered.
#' 
#' @return  specs, a list containing the specifcations of the user to create the population. 

read.console = function() {
  if(exists("specs")) rm(specs,inherits=TRUE)
  run.name <- ""
  while (run.name=="") {
    run.name    <- readline("Name for this run: ")
    if (run.name=="") cat("\n Error: run name is blank ")
  }  
  num.persons <- 0
  while (is.na(num.persons) | num.persons<=0) {
    num.persons <- as.numeric(readline("Number of simulated persons: "))
    if (is.na(num.persons)) cat("\n Error: number of persons missing")
    else if (num.persons<=0) cat("\n Error: number of persons not > 0")
  }     
  ok <- 0
  while (ok==0) {
    min.age <- as.numeric(readline("Minimum age in this run (years): "))
    max.age <- as.numeric(readline("Maximum age in this run (years): "))
    if (min.age<0)  cat("\n Error: Minimum age must be 0-99 ")
    if (min.age>99) cat("\n Error: Maximum age must be 0-99 ")
    if (min.age>max.age) cat("\n Error: Minimum cannot exceed maximum: ")
    if (min.age>=0 & max.age<=99 & min.age <=max.age) ok<-1 else cat("\n Redo age limits")
  }
  ok <- 0
  while (ok==0) {
    yn <- readline("Include males (y/n): ")
    if (substr(tolower(yn),1,1)=="n") males<-"" else males<-"M"
    yn <- readline("Include females (y/n): ")
    if (substr(tolower(yn),1,1)=="n") females<-"" else females<-"F"
    if (males=="M" | females=="F") ok<-1 else cat ("\n Pick males, females, or both")
  }
  gender <- paste0(females,males)
  ok <- 0
  while (ok==0) {
    yn <- readline("Include non-Hispanic (y/n): ")
    if (substr(tolower(yn),1,1)=="n") nonh<-"" else nonh<-"N"
    yn <- readline("Include Mexican-American (y/n): ")
    if (substr(tolower(yn),1,1)=="n") mex<-"" else mex<-"M"
    yn <- readline("Include Other Hispanic (y/n): ")
    if (substr(tolower(yn),1,1)=="n") otherh<-"" else otherh<-"O"
    if (nonh=="N" | mex=="M" | otherh=="O") ok<-1 else cat ("\n Pick at least one ethnicity")
  }   
  ethnicity <- paste0(nonh,mex,otherh)
  ok <- 0
  while (ok==0) {
    yn <- readline("Include White persons (y/n): ")
    if (substr(tolower(yn),1,1)=="n") white<-"" else white<-"W"
    yn <- readline("Include African-Americans (y/n): ")
    if (substr(tolower(yn),1,1)=="n") black<-"" else black<-"B"
    yn <- readline("Include Native Americans (y/n): ")
    if (substr(tolower(yn),1,1)=="n") native<-"" else native<-"N"
    yn <- readline("Include Asian Americans (y/n): ")
    if (substr(tolower(yn),1,1)=="n") asian<-"" else asian<-"A"
    yn <- readline("Include Pacific Islanders (y/n): ")
    if (substr(tolower(yn),1,1)=="n") pacific<-"" else pacific<-"P"
    yn <- readline("Include multiple/other races (y/n): ")
    if (substr(tolower(yn),1,1)=="n") multrace<-"" else multrace<-"O"
    if (white=="W" | black=="B" | native=="N" | asian=="A" | pacific=="P" | multrace=="O") ok<-1 else 
      cat ("Pick at least one race")
  }  
  race <- paste0(white,black,native,asian,pacific,multrace)
  run.seed <- 0
  while (is.na(run.seed) | run.seed<=0 | run.seed > 2147483646 ) {
    run.seed <- as.numeric(readline("Initial random number seed (1-2147483646): "))
    if (is.na(run.seed)) cat("\n Error: Random number seed missing")
    else if (run.seed<=0) cat("\n Error: Random number seed not > 0")
    else if(run.seed > 2147483646) cat("\n Error: Run seed over maximum of 2147483646")
  }  
  
  allstates <- fread("./data/states.txt",colClasses = "character") 
  
  rg  <- readline("Enter list of region codes:")
  rg2 <- str_replace_all(rg, "[ .,]", "")
  rg3 <- string.values(rg2)
  st  <- readline("Enter list of state FIPS codes:") 
  st2 <- str_replace_all(st, "[ .,]", "")
  st3 <- string.values(st2)
  #if (rg3=="" & st3[1]=="") st3 <- c(allstates$FIPS) 
  if (length(rg3) == 0 ){
    st3 <- c(st3,allstates$FIPS)
  } else {
    if (str_detect(rg3,"1"))  st3 <- c(st3,allstates$FIPS[allstates$region=="1"])
    if (str_detect(rg3,"2"))  st3 <- c(st3,allstates$FIPS[allstates$region=="2"])
    if (str_detect(rg3,"3"))  st3 <- c(st3,allstates$FIPS[allstates$region=="3"])    
    if (str_detect(rg3,"4"))  st3 <- c(st3,allstates$FIPS[allstates$region=="4"])
  }
  st4     <- unique(st3[order(st3)])
  states  <- st4[st4!=""]
  allregs <- unique(allstates[allstates$FIPS %in% states]$region)
  
  specs <- list(
    run.name       = run.name,
    num.persons    = num.persons,
    min.age        = min.age,
    max.age        = max.age,
    gender         = gender,
    ethnicity      = ethnicity,
    race           = race,
    run.seed       = run.seed,
    states         = states,
    regions        = rg3,
    allregs        = allregs)
  return(specs)
}


#' Reads and stores input from popfile.txt file 
#' 
#' Creates a list to apply user specifications to the datasets. 
#' @param   popfile the .txt input file
#' @return  specs, a list containing the specifications of the user to create the population. 

read.popfile = function(popfile){
  
  run.name       <- "test"
  num.persons    <- 10
  min.age        <- 3 
  max.age        <- 5
  gender         <- c("M","F")
  ethnicity      <- c("N","M","O")
  race           <- c("W","B","N","A","P","O")
  run.seed       <- 876144637
  filename       <- paste0(files$inpath,files$states)
  allstates <- fread("./data/states.txt",colClasses = "character")
  
  a <- read.table(paste0("./input/",popfile),skip=1,sep="=",
                  stringsAsFactors=FALSE,strip.white=TRUE)
  a$V1 <- tolower(a$V1)
  a$V2 <- gsub(" ","",a$V2)
  for (i in 1:nrow(a)) {
    if (str_trim(a$V1[i])=="run.name")       run.name      <- str_trim(a$V2[i])
    if (str_trim(a$V1[i])=="num.persons")    num.persons   <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="min.age")        min.age       <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="max.age")        max.age       <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="gender")         gender        <- c(str_trim(a$V2[i]))
    if (str_trim(a$V1[i])=="ethnicity")      ethnicity     <- c(str_trim(a$V2[i]))
    if (str_trim(a$V1[i])=="race")           race          <- c(str_trim(a$V2[i]))
    if (str_trim(a$V1[i])=="run.seed")       run.seed      <- as.integer(a$V2[i])
    if (str_trim(a$V1[i])=="regions")        regions       <- str_trim(a$V2[i])
    if (str_trim(a$V1[i])=="states")         states        <- splitpairs(a$V2[i])
  }
  
  dir  <- paste0("output/",run.name)
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
  gender <- toupper(gender)
  b    <- ""
  if (str_detect(gender,"M"))  b <- c(b,"M")
  if (str_detect(gender,"F"))  b <- c(b,"F")
  gender.list <- b[2:length(b)]
  ethnicity <- toupper(ethnicity)
  b       <- ""
  if (str_detect(ethnicity,"N"))  b <- c(b,"N")
  if (str_detect(ethnicity,"M"))  b <- c(b,"M")
  if (str_detect(ethnicity,"O"))  b <- c(b,"O")
  ethnicity.list <- b[2:length(b)]
  race <- toupper(race)
  b <- ""
  if (str_detect(race,"W"))  b <- c(b,"W")
  if (str_detect(race,"B"))  b <- c(b,"B")
  if (str_detect(race,"N"))  b <- c(b,"N")
  if (str_detect(race,"A"))  b <- c(b,"A")
  if (str_detect(race,"P"))  b <- c(b,"P")
  if (str_detect(race,"O"))  b <- c(b,"O")
  race.list <- b[2:length(b)]
  if (regions[1]=="" & states[1]=="") states = c(allstates$FIPS)
  if (str_detect(regions,"1")) states <- c(states,allstates$FIPS[allstates$region=="1"])
  if (str_detect(regions,"2")) states <- c(states,allstates$FIPS[allstates$region=="2"])
  if (str_detect(regions,"3")) states <- c(states,allstates$FIPS[allstates$region=="3"])    
  if (str_detect(regions,"4")) states <- c(states,allstates$FIPS[allstates$region=="4"])
  states  <- unique(states[order(states)])
  states  <- states[states!=""]
  allregs <- unique(allstates[allstates$FIPS %in% states]$region)
  
  if (num.persons<=0)          stop("\n No persons to model \n")
  if (is.null(gender.list))    stop("\n No gender selected \n")
  if (is.null(ethnicity.list)) stop("\n No ethnicity selected \n")
  if (is.null(race.list))      stop("\n No race selected \n")
  if (max.age<min.age)         stop("\n Max age < min age \n")
  
  
  cat("\n")
  cat("run.name      =",run.name,"\n")
  cat("num.persons   =",num.persons,"\n")
  cat("min.age       =",min.age,"\n")
  cat("max.age       =",max.age,"\n")
  cat("gender        =",gender,"\n")
  cat("ethnicity     =",ethnicity,"\n")
  cat("race          =",race,"\n")
  cat("run.seed      =",run.seed,"\n")
  cat("states        =",states,"\n")
  cat("regions       =",regions,"\n")
  
  specs <- list(
    run.name       = run.name,
    num.persons    = num.persons,
    min.age        = min.age,
    max.age        = max.age,
    gender         = gender,
    ethnicity      = ethnicity,
    race           = race,
    run.seed       = run.seed,
    states         = states,
    regions        = regions, 
    allregs        = allregs)
  return(specs)
  
}


#' Reads in PUMS data
#'
#' Uses filename from files in Control.R's Run.RPGen. Only loads in region files
#' necessary for specified run. 
#'
#' @return  PUMS dataframe 

read.pums = function() {
  filename <- files$pums
  kept <- 0
  for (i in 1:4) {
    if (i %in% g$allregs) {
      x <- get(load(paste0("./data/",str_c(filename," Region ", i,".rda"))))
      setnames(x,1,"pool")
      setnames(x,length(names(x)),"vehicles")
      x$compid <- x$compid + 10000000   # to force leading zero on state FIPS
      x$state  <- substr(x$compid,2,3)
      x1 <- x[(x$state %in% g$states),]
      x2 <- x1[(x1$age>=g$min.age & x1$age<=g$max.age),]
      if (kept==0) y <- x2
      if (kept>0)  y <- rbind(y,x2)
      kept <- 1
    }
  }
  pums <- y[(str_detect(g$gender,y$gender) & str_detect(g$race,y$race) & str_detect(g$ethnic,y$ethnicity)),]
  if (nrow(pums)==0) stop("No suitable persons found in PUMS database")
  return(pums)
}  

# httk :

#' Renames inputs in a datatable of input demographics.  
#'
#' @param  p a datatable with ethnicity, age, and gender
#' @return a datatable of demograhpics


httkvars = function(p) {
  reths <- c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other")
  q <- data.table::copy(p)
  nreth <- rep(5,nrow(q))
  nreth[q$ethnicity=="M"] <- 1
  nreth[p$ethnicity=="O"] <- 2
  nreth[p$ethnicity=="N" & p$race=="W"] <- 3
  nreth[p$ethnicity=="N" & p$race=="B"] <- 4
  q$reth <- as.factor(reths[nreth])
  q.months <- get.randoms("months",nrow(q),g$seeds,g$var.list,0)
  q$age_months <- 12*q$age+floor(12*q.months)
  q$temp_age_months <- 12*q$age+floor(12*q.months)
  q$age_years <- q$age
  q$gender[q$gender=="M"] <- "Male"
  q$gender[q$gender=="F"] <- "Female"
  q$num <- 1:nrow(q)
  return(q)
}

#' Generates height and weight for individuals given ethnicity age, and gender. 
#'
#' Adapted from httk, contains get.randoms() to add variabiltiy. 
#' @citation Pearce, R., Setzer, R., Strope, C., Sipes, N., & Wambaugh, J. (2017). 
#'           R Package for High-Throughput Toxicokinetics. Journal of Statistical Software, 
#'           79(4), 1 - 26. doi:http://dx.doi.org/10.18637/jss.v079.i04 
#' 
#' @param  hbw_dt dataframe with ethnicity, age, and gender
#' @param  specs  user limits on ethnicity, age, and gender
#' @return dataframe with height and bodyweight generated

random_gen_height_weight = function(hbw_dt,specs) {
  mean_logh <- g <- gender <- r <- reth <- height_spline <- NULL
  age_months <- mean_logbw <- weight_spline <- hw_kde <- nkde <- NULL
  id <- weight <- NULL
  logbw_resid <- height <- logh_resid <- NULL
  hbw_dt <- data.table::copy(hbw_dt)
  hbw_dt[, `:=`(age, age_years)]
  hbw_dt[, `:=`(age_years, pmin(age_years,99))]
  hbw_dt[, `:=`(age_months,pmin(age_months,99*12))]    # cap age at 99 years
  hbw_dt[, `:=`(mean_logh,  predict(spline_heightweight[g == gender & r == reth, height_spline][[1]], x = age_months)$y), by = list(gender, reth)]
  hbw_dt[, `:=`(mean_logbw, predict(spline_heightweight[g == gender & r == reth, weight_spline][[1]], x = age_months)$y), by = list(gender, reth)]
  spline_kde <- spline_heightweight[, list(g, r, hw_kde, nkde)]
  setnames(spline_kde, c("g", "r"), c("gender", "reth"))
  hbw_dt[, `:=`(id, 1:nrow(hbw_dt))]
  hbw_dt <- merge(hbw_dt, spline_kde, by = c("gender", "reth"))
  hbw_dt[, `:=`(q.nkde, get.randoms("nkde",nrow(hbw_dt),specs$seeds,specs$var.list,0))] 
  hbw_dt[, `:=`(q.hw1,  get.randoms("hw1", nrow(hbw_dt),specs$seeds,specs$var.list,0))] 
  hbw_dt[, `:=`(q.hw2,  get.randoms("hw2", nrow(hbw_dt),specs$seeds,specs$var.list,0))]
  hbw_dt[, `:=`(c("logbw_resid", "logh_resid"), as.data.frame(hw_kde[[1]]$x[sampleq(unique(nkde), hw_kde[[1]]$w, q.nkde), ] 
                                                              + bi_norm_cor(cbind(q.hw1,q.hw2), mean=c(0, 0), sigma=hw_kde[[1]]$H))), by = list(gender, reth)]
  hbw_dt[, `:=`(weight, pmin(exp(mean_logbw + logbw_resid),160))]   # cap at 160 kg
  hbw_dt[, `:=`(height, pmin(exp(mean_logh + logh_resid),225))]     # cap at 225 cm
  hbw_dt[, `:=`(id, NULL)]
  hbw_dt[, `:=`(hw_kde, NULL)]
  hbw_dt[, `:=`(nkde, NULL)]
  hbw_dt[, `:=`(q.nkde,NULL)]
  hbw_dt[, `:=`(q.hw1,NULL)]
  hbw_dt[, `:=`(q.hw2,NULL)]
  return(hbw_dt)
}


#' Generates toxicokinetic parameters for body tissues using ethnicity, age, gender, weight, and height as inputs 
#'
#' Adapted from httk, contains get.randoms() to add variabiltiy. 
#' @citation Pearce, R., Setzer, R., Strope, C., Sipes, N., & Wambaugh, J. (2017). 
#'           R Package for High-Throughput Toxicokinetics. Journal of Statistical Software, 
#'           79(4), 1 - 26. doi:http://dx.doi.org/10.18637/jss.v079.i04 
#' 
#' @param  tmf_dt dataframe with ethnicity, age, gender, weight and height
#' @return dataframe with toxicokinetic parameters generated

random_tissue_masses_flows = function (tmf_dt) {
  id <- mass_mean <- height_ref <- height <- mass_ref <- tissue <- NULL
  gender <- age_years <- age_months <- weight <- bonemass_mean <- NULL
  BSA <- mass_dist <- mass <- mass_cv <- flow_mean <- flow_ref <- NULL
  flow_frac <- flow <- flow_cv <- CO <- Adipose <- Bone <- NULL
  org_mass_sum <- Blood <- Other_mass <- Adipose_mass <- NULL
  org_flow_check <- CO_flow <- weight_adj <- BSA_adj <- NULL
  million.cells.per.gliver <- NULL
  tmf_dt <- copy(tmf_dt)
  tmf_dt[, `:=`(id, 1:nrow(tmf_dt))]
  tmp_dt <- merge(tmf_dt, mcnally_dt, by = "gender", allow.cartesian = TRUE)
  tmp_dt[, `:=`(mass_mean, tissue_scale(height_ref = height_ref, height_indiv = height, tissue_mean_ref = mass_ref))]
  tmp_dt[tissue == "Brain", `:=`(mass_mean, brain_mass(gender = gender, age_years = age_years))]
  tmp_dt[tissue == "Bone", `:=`(mass_mean, bone_mass_age(age_years = age_years, 
                                                         age_months = age_months, height = height, weight = weight, gender = gender))]
  bone_mass_mean <- tmp_dt[tissue == "Bone", list(id, mass_mean)]
  setnames(bone_mass_mean, "mass_mean", "bonemass_mean")
  tmp_dt <- merge(tmp_dt, bone_mass_mean, by = ("id"))
  tmp_dt[tissue == "Skeleton", `:=`(mass_mean, bonemass_mean/0.5)]
  tmp_dt[, `:=`(bonemass_mean, NULL)]
  rm(bone_mass_mean)
  tmp_dt[, `:=` (q.norm, get.randoms("norm",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[, `:=` (q.logn, get.randoms("logn",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[tissue == "Muscle", `:=`(mass_mean, skeletal_muscle_mass(smm = mass_mean, age_years = age_years, height = height, gender = gender))]
  tmp_dt[tissue == "Liver" & age_years <= 18, `:=`(mass_mean, liver_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Kidney" & age_years <= 18, `:=`(mass_mean, kidney_mass_children(weight = weight, height = height, gender = gender))]
  tmp_dt[tissue == "Pancreas" & age_years <= 18, `:=`(mass_mean, pancreas_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Spleen" & age_years <= 18, `:=`(mass_mean, spleen_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Lung" & age_years <= 18, `:=`(mass_mean, lung_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[, `:=`(BSA, body_surface_area(BW = weight, H = height, age_years = age_years))]
  tmp_dt[tissue == "Skin", `:=`(mass_mean, skin_mass_bosgra(BSA = BSA))]
  tmp_dt[tissue == "Blood", `:=`(mass_mean, blood_weight(BSA = BSA/(100^2), gender = gender))]
  tmp_dt[tissue == "Blood" & mass_mean < 0.2, `:=`(mass_mean, blood_mass_correct(blood_mass = mass_mean, age_months = age_months, 
                                                                                 age_years = age_years, gender = gender, weight = weight))]
  tmp_dt[mass_dist == "Normal", `:=`(mass, truncnorm::qtruncnorm(q.norm, a = 0, mean = mass_mean, sd = mass_cv * mass_mean))]
  tmp_dt[mass_dist == "Log-normal", `:=`(mass, exp(pnorm(q.logn, mean = log(mass_mean), sd = sqrt(log(mass_cv^2 + 1)))))]
  tmp_dt[tissue == "CO", `:=`(flow_mean, tissue_scale(height_ref = height_ref, height_indiv = height,
                                                      tissue_mean_ref = 1.05 * flow_ref) * (1 - max(0, 0.005 * (age_years - 25))))]
  CO_flow_mean <- tmp_dt[tissue == "CO", list(id, flow_mean)]
  setnames(CO_flow_mean, "flow_mean", "CO_flow_mean")
  tmp_dt <- merge(tmp_dt, CO_flow_mean, by = "id", allow.cartesian = TRUE)
  tmp_dt[tissue != "CO", `:=`(flow_mean, flow_frac * CO_flow_mean)]
  tmp_dt[, `:=` (q.flow, get.randoms("flow",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[tissue != "CO" & tissue != "Lung" & !is.na(flow_mean),`:=`(flow, truncnorm::qtruncnorm(q.flow, a=0, mean=flow_mean, sd=flow_cv*flow_mean))]
  tmp_dt[tissue == "Lung", `:=`(flow, flow_frac * CO_flow_mean)]
  tmp_dt[tissue == "CO", `:=`(flow, CO_flow_mean)]
  mass_cast <- data.table::dcast.data.table(tmp_dt, id ~ tissue, value.var = "mass")
  mass_cast[, `:=`(CO, NULL)]
  mass_cast[, `:=`(Adipose, NULL)]
  mass_cast[, `:=`(Bone, NULL)]
  setnames(mass_cast, names(mass_cast)[names(mass_cast) != "id"], paste(names(mass_cast)[names(mass_cast) != "id"], "mass", sep = "_"))
  mass_cast[, `:=`(org_mass_sum, Reduce("+", .SD)), .SDcols = grep(x = names(mass_cast), pattern = "mass", value = TRUE)]
  flow_cast <- data.table::dcast.data.table(tmp_dt, id ~ tissue, value.var = "flow")
  flow_cast[, `:=`(Blood, NULL)]
  flow_cast[, `:=`(Bone, NULL)]
  setnames(flow_cast, names(flow_cast)[names(flow_cast) != "id"], paste(names(flow_cast)[names(flow_cast) != "id"], "flow", sep = "_"))
  tmf_dt <- merge(tmf_dt, mass_cast, by = "id")
  tmf_dt <- merge(tmf_dt, flow_cast, by = "id")
  tmf_dt[, `:=`(Other_mass, (0.033 + 0.014) * weight)]
  tmf_dt[, `:=`(org_mass_sum, org_mass_sum + Other_mass)]
  tmf_dt[, `:=`(q.adip, get.randoms("adip",nrow(tmf_dt),g$seeds,g$var.list,0))] 
  tmf_dt[, `:=`(Adipose_mass, exp(pnorm(q.adip, mean = log(pmax(1,weight-org_mass_sum)), sd = sqrt(log(0.42^2 + 1)))))]
  tmf_dt[(weight - org_mass_sum) <= 1, `:=`(Adipose_mass, 0)]
  tmf_dt[, `:=`(org_flow_check, Reduce("+", .SD)), .SDcols = names(flow_cast)[!(names(flow_cast) %in% c("CO_flow", "id"))]]
  tmf_dt[, `:=`(org_flow_check, org_flow_check/CO_flow)]
  tmf_dt[, `:=`(weight_adj, org_mass_sum + Adipose_mass)]
  tmf_dt[, `:=`(BSA_adj, body_surface_area(BW = weight_adj, H = height, age_years = age_years))]
  mu <- log(10^(-0.66 * log10(tmf_dt[, age_years]) + 3.1))
  mu[tmf_dt[, age_years < 20]] <- log(10^(-0.66 * log10(19) + 3.1))
  sigma.total <- ((log(444) - log(99))/2 + (log(99) - log(23))/2)/2
  Fval <- qf(0.012/2, df1 = 1, df2 = 26, lower.tail = FALSE)
  R2 <- Fval/(1 + Fval)
  sigma <- sqrt((1 - R2) * sigma.total^2)
  tmf_dt[, `:=` (q.gliv, get.randoms("gliv",nrow(tmf_dt),g$seeds,g$var.list,0))] 
  tmf_dt[, `:=`(million.cells.per.gliver, exp(pnorm(q.gliv, mean = mu, sd = sigma)))]
  setnames(tmf_dt, c("Kidney_mass", "Kidney_flow", "CO_flow"), c("Kidneys_mass", "Kidneys_flow", "CO"))
  tmf_dt[, `:=`(id, NULL)]
  # tmf_dt[, `:=`(org_mass_sum, NULL)]
  return(tmf_dt)
}


#' Generates spleen mass for children. 
#'
#' Adapted from httk: 
#' @citation Pearce, R., Setzer, R., Strope, C., Sipes, N., & Wambaugh, J. (2017). 
#'           R Package for High-Throughput Toxicokinetics. Journal of Statistical Software, 
#'           79(4), 1 - 26. doi:http://dx.doi.org/10.18637/jss.v079.i04 
#' 
#' @param  height height of individual
#' @param  weight weight of individual
#' @param  gender gender of individual
#' @return spleen mass

spleen_mass_children = function (height, weight, gender) 
{
  sm <- rep(NA, length(gender))
  sm[gender == "Male"] <- (8.75 * height[gender == "Male"]/100 * sqrt(weight[gender == "Male"]) + 11.06)/1000
  sm[gender == "Female"] <- (9.36 * height[gender == "Female"]/100 * sqrt(weight[gender == "Female"]) + 7.98)/1000
  return(sm)
}

# PopGen function:

#' Creates a population with matching physiological/toxicokinetic variables.  
#'
#' Accepts either console entry or a runfile in the /input/ folder before 
#' creating a population using PUMS and httk.
#'
#' @param   runfile character string of name of runfile, followed by .txt extension.
#' @return  a data table containing the generated random population. 

popgen = function (runfile=NULL) {
  cat("\n HEM population generator module")
  if(!is.null(runfile)) specs <- read.popfile(runfile)
  if(is.null(runfile))  specs <- read.console()
  specs$var.list <- c("pums","ahs","recs","months","nkde","hw1","hw2","norm",
                      "logn","flow","adip","gliv","hema","rfun1","rfun2")
  specs$seeds    <- get.seeds(specs$run.seed,length(specs$var.list))
  g       <<- specs
  cat("Saved run settings\n")
  pums1    <- read.pums()
  sel.pop  <- gen.pop(pums1)
  pop0     <- pums1[(sel.pop),]
  pop0$row <- 1:nrow(pop0)
  popx     <- httkvars(pop0)
  popx     <-data.table(popx)
  indiv_dt <- random_gen_height_weight(popx,g)
  indiv_dt <- random_tissue_masses_flows(tmf_dt = indiv_dt)
  indiv_dt <- estimate_hematocrit(hcttmp_dt = indiv_dt)
  indiv_dt <- estimate_gfr(gfrtmp.dt = indiv_dt)
  indiv_dt[, `:=` (bmi_adj, weight_adj/((height/100)^2))]
  indiv_dt[, `:=` (bmi,     weight/((height/100)^2))]
  indiv_dt[, `:=` (BSA, body_surface_area(weight, height, age_years))]
  indiv_dt[, `:=` (age_years,  age)]
  indiv_dt[, `:=` (age_months, temp_age_months)]  
  indiv_dt <- adjust_weight(y = indiv_dt)
  #indiv_dt$q.gliv <- NULL 
  setnames(indiv_dt,"vehicles","cars")
  indiv_dt <- setorder(indiv_dt,num)
  return(indiv_dt)
}
