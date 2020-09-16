# AHS Downloader
# AE, ORAU, 2020

#' Downloads 2017 AHS Data and Formats for RPGen
#'
#' Selects columns and creates pool input (urban, location, 
#' housetyp, famcat, inccat) to create pool. Codes in lot and unit
#' size.
#'
#' Data from https://www.census.gov/programs-surveys/ahs/data/2017/ahs-2017-public-use-file--puf-/ahs-2017-national-public-use-file--puf-.html
#'
#' @param   x A hyperlink to AHS 2017 Survey Data.
#' @return  Runtime for the complete download and test.
#' @export  RPGen_RECS.rda (where the underscore is a space)


AHS.download<-function(x){
  
suppressPackageStartupMessages(
source("./R/Packages.R"))
source("./tests/contents.R")
source("./tests/report card.R")
source("./tests/pool test.R") 
  
start<-proc.time()[3]

AHSNAME<-"AHS RPGen.csv"
download(x, dest =AHSNAME, mode = "wb")
unzip(AHSNAME, files = "household.csv")

ahscols<-(toupper(c("acprimary","acsecndry","bathrooms","bedrooms","bld","control","cookfuel","cooktype",
                    "dishwash","dist","division","dryer","elecamt","fridge","garage","gasamt","heatfuel","heattype",
                    "hhage","hhgrad","hincp","kitchens","laundy","lotsize","numadults","numelders","numoldkids",
                    "numpeople","numyngkids","oilamt","omb13cbsa","otheramt","paintpeel","sewtype","totrooms",
                    "unitfloors","unitsize","walk","washer","watsource","weight","yrbuilt")))


ahs<-fread("./household.csv", encoding = "UTF-8", header=TRUE, select = ahscols)
file.remove("household.csv")
file.remove("AHS RPGen.csv")
colnames(ahs)<-tolower(colnames(ahs))

ahs<- ahs %>% mutate_each(list(str_replace_all(.,"'",""))) %>%
  mutate_if(is.character,as.numeric) %>%
  filter(numadults>0 & numpeople >0) %>%
  mutate(region = case_when(
    (division == 1 | division == 2) ~ 1,
    (division == 3 | division == 4) ~ 2,
    (division == 5 | division == 6 | division == 7) ~ 3,
    (division == 8 | division == 9) ~ 4)) %>%
  filter(region %in% 1:4) %>%
  mutate(urban = ifelse(omb13cbsa >= 99998,0,1)) %>%
  mutate(location = 2*region - 1 + urban) %>%
  filter(location %in% 1:8) %>%
  mutate(housetyp = case_when(
    (bld == 2 | bld == 3) ~ 1,
    (bld == 1 | bld == 10) ~2,
    (bld %in% 4:9 | bld > 10) ~ 3)) %>%
  mutate(numchild = numyngkids + numoldkids) %>%
  mutate(famcat = case_when(
    (numadults == 1 & numchild == 0) ~ 1,
    (numadults == 1 & numchild >  0) ~ 2,
    (numadults >  1 & numchild == 0) ~ 3,
    (numadults >  1 & numchild >  0) ~ 4)) %>%
  filter(famcat %in% 1:4) %>%
  mutate(income = ifelse(hincp > 0, hincp, 0)) %>%
  mutate(sewtype = replace(sewtype, sewtype ==-9,0)) %>%
  mutate(unitsf = case_when(
    (unitsize == 1) ~ 400,
    (unitsize == 2) ~ 600,      
    (unitsize == 3) ~ 900,      
    (unitsize == 4) ~ 1250,
    (unitsize == 5) ~ 1750,
    (unitsize == 6) ~ 2250,
    (unitsize == 7) ~ 2750,
    (unitsize == 8) ~ 3500,
    (unitsize == 9) ~ 5000,
    (unitsize == -9) ~ 1500,)) %>%
  mutate(lot_sf = case_when(
    (lotsize == 1) ~ 3000,
    (lotsize == 2) ~ 7000,
    (lotsize == 3) ~ 15000,
    (lotsize == 4) ~ 30000,
    (lotsize == 5) ~ 100000,
    (lotsize == 6) ~ 300000,
    (lotsize == 7) ~ 600000,))



split.ahs<-split(ahs, ahs$location)
inccat.gen <- function(x){
  x<-x[order(x$income),] 
  z<-as.numeric(nrow(x))
  regindex<- (z - (z %% 3))/3
  index3<-regindex
  
  if ((z %% 3) == 2){
    index1<- regindex + 1
    index2<- regindex + 1
  } else if ((z %% 3) == 1){
    index1<- regindex +1
    index2<- regindex
  } else if ((z %% 3) == 0){
    index1<- regindex
    index2<- regindex
  }
  
  x$inccat<-3
  x[c(0:(index1+index2)),]$inccat <-2
  x[c(0:index1),]$inccat<-1
  
  return(x)
}
ahs<-rbind.fill(lapply(split.ahs,inccat.gen))


ahs<- ahs %>% mutate(pool = 36*(location-1)+12*(housetyp-1)+3*(famcat-1)+inccat)

RPGen_reportcard(ahs)

old<-c("acprimary","acsecndry","bathrooms","bedrooms","yrbuilt","heattype",
       "lot_sf","weight","totrooms","sewtype","watsource","hincp")
new<-c("acprim","acsec","baths","bedrms","built","hequip","lot","pwt","rooms",
       "sewdis","water","income")

setnames(ahs, old,new)

ahs<-ahs[c("pool", "acprim", "acsec","rooms","baths","bedrms","built","hequip","heatfuel","sewdis",
           "unitsf","water","control","income")]


save(ahs, file = "./data/RPGen AHS.rda", compress = "xz")

end<-as.numeric((proc.time()[3]-start)/60)
runtime<- str_c("RPGen AHS downloaded in ", as.character(round(end,2)), " minutes.")
return(writeLines(runtime))
}

AHS.download("http://www2.census.gov/programs-surveys/ahs/2017/AHS%202017%20National%20PUF%20v3.0%20CSV.zip")
