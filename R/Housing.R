# Housing Generator for EPA's RPGen
# Designed and written for EPA by WGG of ICF, Nomvember 26, 2017
# Updated by AE of ORAU, 2020.


#' Reads in AHS data
#'
#' Uses filename from files in Control.R's Run.RPGen if
#' no filename entered(as is the case in when run) 
#'
#' @param   filename name and directory of AHS input file.
#' @return  AHS dataframe 

read.ahs = function(filename=NULL) {
  if (is.null(filename)) filename <- paste0(files$inpath,files$ahs)
  x1 <- get(load((filename)))
  x1[x1<0] <- NA
  return(x1)
}

#' Reads in RECS data
#'
#' Uses filename from files in Control.R's Run.RPGen if
#' no filename entered(as is the case in when run) 
#'
#' @param   filename name and directory of AHS input file.
#' @return  RECS dataframe

read.recs = function(filename=NULL) {
  if (is.null(filename)) filename <- paste0(files$inpath,files$recs)
  x2 <- get(load((filename)))
  x2[x2<0] <- NA
  return(x2)
}

#' Calls read.ahs and 
#'
#' Uses filename from files in Control.R's Run.RPGen if
#' no filename entered(as is the case in when run) 
#'
#' @param   pop PUMS and httk population from user input (PopGen)
#' @param   ahsname Optional name of AHS dataframe
#' @param   recsname Optional name of RECS dataframe 
#' @return  RECS Dataframe.

match.pop.housing = function(pop,ahsname=NULL,recsname=NULL) {
  cat("\n Starting population-housing matching \n")
  ahs  <- read.ahs()
  recs <- read.recs()
  ahs <- data.table(ahs)
  recs <- data.table(recs)
  apool  <- ahs$pool
  a      <- ahs[,pool:=NULL]
  rpool  <- recs$pool
  r      <- recs[,pool:=NULL]
  u.ahs  <- get.randoms("ahs",g$num.persons,g$seeds,g$var.list,0)
  u.recs <- get.randoms("recs",g$num.persons,g$seeds,g$var.list,0)
  ahsvars   <- as.data.table(matrix(0,nrow=nrow(pop),ncol=ncol(a)))
  recsvars  <- as.data.table(matrix(0,nrow=nrow(pop),ncol=ncol(r)))
  setnames(ahsvars,names(ahsvars),names(a))
  setnames(recsvars,names(recsvars),names(r))
  # mode(ahsvars$control)  <- "character"
  for (i in 1:288) {
    x <- pop$row[pop$pool==i]
    if (length(x)>0) {
      y <- a[(apool==i),]
      if (nrow(y)==0) y <- a[apool==(i-1)]
      if (nrow(y)==0) y <- a[apool==(i-2)]
      y.wt <- cumsum(y$pwt)
      cp.y <- y.wt/y.wt[nrow(y)]
      rows.y <- 1+findInterval(u.ahs[x],cp.y,rightmost.closed=TRUE)
      ahsvars[x] <- y[rows.y]
      z <- r[rpool==i]
      if (nrow(z)==0) z <- r[rpool==(i-1)]
      if (nrow(z)==0) z <- r[rpool==(i-2)]
      if (nrow(z)==0) z <- r[rpool==(i+32)]
      z.wt <- cumsum(z$nweight)
      cp.z <- z.wt/z.wt[nrow(z)]
      rows.z <- 1+findInterval(u.recs[x],cp.z,rightmost.closed=TRUE)
      recsvars[x] <- z[rows.z]
    } 
  }
  ph <- as.data.table(cbind(pop,ahsvars,recsvars))
  ph$family <- 1 + 5*ph$famcat + 4*(ph$famcat %%2)
  return(ph)
} 







