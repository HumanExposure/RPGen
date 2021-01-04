# Control file for EPA's RPGen
# Designed and written for EPA by WGG of ICF, September 26, 2017
# Updated by AE of ORAU, 2020.

#' Runs RPGen. 
#'
#' Accepts either console entry or a runfile in the /input/ folder before sourcing
#' PopGen.R, Packages.R, and Housing.R to create a population using PUMS and httk
#' before matching it with households in RECS and AHS. The output is found in the 
#' /output/ folder with the name of the folder being the run name specified by the
#' user. 
#'
#' @param   runfile character string of name of runfile, followed by .txt extension.
#' @return  RECS Dataframe.
#' @export  pophouse.csv
#' @example RPGen.run("run1.txt")

RPGen.run = function(runfile=NULL) {
  
  # setup 
  inpath   <- "./data/"
  outpath  <- "./output/"
  run      <- "runfile.txt"
  pums     <- "RPGen PUMS"
  ahs      <- "RPGen AHS.rda"
  recs     <- "RPGen RECS.rda"
  
  f        <- as.list(c(inpath,outpath,run,pums,ahs,recs))
  names(f) <- c("inpath","outpath","run","pums","ahs","recs")
  files   <<- f
  
  
  suppressPackageStartupMessages(TRUE)
  # Load required packages and source all code modules.
  source("./R/Packages.R")
  source("./R/PopGen.R")
  source("./R/Housing.R")
  
  pop  <<- popgen(runfile)
  dir  <- paste0(files$outpath,g$run.name)
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
  pophouse <<- match.pop.housing(pop,NULL,NULL)
  filename <- paste0(files$outpath,g$run.name,"/pophouse.csv") 
  write.csv(pophouse,filename,row.names = FALSE)
  
  # Testing 
  cat(" Checking Pools...\n")
  poppools <- names(table(pophouse[["pool"]], useNA = "ifany"))
  load("./data/RPGen AHS.rda")
  load("./data/RPGen RECS.rda")
  source("./tests/pool test.R")

  recspools <- names((table(c(recs$pool), useNA = "ifany")))
  ahspools  <- names((table(c(ahs$pool), useNA = "ifany")))

if (length(recspools) > 0){

recsadded <- setdiff(poppools, recspools)
recsaddednames <- pool.reader(recsadded)

recsmismatch<- data.frame("Pool" = recsadded,
                          "Classification" = recsaddednames)

}

if (length(ahspools) > 0){

ahsadded  <-setdiff(poppools, ahspools)
ahsaddednames <- pool.reader(ahsadded)

ahsmismatch<- data.frame("Pool" = ahsadded,
                          "Classification" = ahsaddednames)
}

if (length(ahspools > 0) & length(recspools > 0)){
  mismatchedhousing <<- list("AHS" = ahsmismatch, "RECS" = recsmismatch)
  cat(" See mismatchedhousing list for generated persons (PUMS) for which no houses were available (AHS and RECS).\n")
} else if (length(ahspools < 0) & length(recspools > 0)){
  mismatchedhousing <<- recsmismatch
  cat(" See mismatchedhousing dataframe for generated persons (PUMS) for which no houses were available from RECS.\n")
} else if (length(ahspools > 0) & length(recspools < 0)){
  mismatchedhousing <<-ahsmismatch
  cat(" See mismatchedhousing dataframe for generated persons (PUMS) for which no houses were available from AHS.\n")
}

  
  
  cat(" \n")
  gc()
  cat(" Housing generator completed: R object = 'pophouse', filename =",filename,"\n")
  #rm(list = setdiff(ls(), c("pophouse","mismatchedhousing")))
}  

RPGen.run()


