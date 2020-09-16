# Pool Reader
# AE, ORAU, 2020

#' Returns the name of any given pool numeric. 
#'
#' Used in testing (though not a test itself) Where a pool number is 
#' converted into a readable category containing location, house type,
#' family category, and income.
#'
#' @param   x a numeric or series of numerics 1-288.
#' @return  Name of a pool.
#' @example pool.reader(1)

pool.reader<- function(x){
  
  # pool = 36*(location-1)+12*(housetyp-1)+3*(famcat-1)+inccat
  
  x<-as.numeric(x)  
  
  location.des <- function(x){
    
    setting0 <- "Rural"
    setting1 <- "Urban"
    
    reg1 <- " North East"
    reg2 <- " Midwest"
    reg3 <- " South"
    reg4 <- " West"
    
    if (x %% 2 == 0){
      des<- setting1
    } else {
      des<- setting0
    }
    
    if (x %in% 7:8){
      des <- str_c(des,reg4)
    } else if (x %in% 5:6){
      des <- str_c(des,reg3)
    } else if (x %in% 3:4){
      des <- str_c(des,reg2)
    } else if (x %in% 1:2)
      des<- str_c(des,reg1)
    
    return(des)
  }
  loc.div<- sort(rep(c(1:8),36))
  loc.pos <- sapply(loc.div,location.des)
  
  housetyp.des <- function(x){
    house1<- "Stand Alone"
    house2<- "Multi Unit Structure"
    house3<- "Other Housetype"
    
    if (x == 1){
      des <- house1
    } else if (x == 2){
      des <- house2
    } else if (x == 3){
      des<-house3
    }
    return(des)
  }
  housetyp.div <- rep(sort(rep(c(1:3),12)),8)
  housetyp.pos<- sapply(housetyp.div,housetyp.des)
  
  famcat.des <-function(x){
    
    famcat1<- "One Adult"
    famcat2<- "One Adult with Kids"
    famcat3<- "Adults"
    famcat4<- "Adults with Kids"
    
    if (x == 1){
      des <- famcat1
    } else if (x == 2){
      des <- famcat2
    } else if (x == 3){
      des<- famcat3
    } else if (x == 4){
      des<- famcat4
    }
    return(des)
  }
  famcat.div <- rep(sort(rep(c(1:4),3)),24)
  famcat.pos<- sapply(famcat.div,famcat.des)
  
  inccat.des <- function(x){
    inc1<- "Low Income"
    inc2<- "Middle Income"
    inc3<- "High Income"
    
    if (x == 1){
      des <- inc1
    } else if (x == 2){
      des <- inc2
    } else if (x == 3){
      des<-inc3
    }
    return(des)
  }
  inccat.div <-rep(c(1:3),96)
  inccat.pos <-sapply(inccat.div,inccat.des)
  
  register<- str_c(loc.pos," ",housetyp.pos," ",famcat.pos," ",inccat.pos)
  output<- register[x]
  
  return(output)
}

