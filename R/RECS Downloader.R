# RECS Downloader
# AE, ORAU, 2020

#' Downloads 2015 RECS Data and Formats for RPGen
#'
#' Selects relevant columns and creates pool input (urban, location, 
#' housetyp, famcat, inccat) to create pool. Renames pool,
#' meaning number of swimming pools, to 'swim' to avoid confusion.  
#' Data from https://www.eia.gov/consumption/residential/data/2015/
#'
#' @param   x A hyperlink to RECS 2015 Survey Data.
#' @return  Runtime for the complete download and test.
#' @export  RPGen_RECS.rda (where the underscore is a space)

RPGen.RECS <- function(x){

suppressPackageStartupMessages(
source("./R/Packages.R"))
source("./tests/contents.R")
source("./tests/report card.R")
source("./tests/pool test.R")  
  
start<-proc.time()[3]
recscols<-(toupper(c("adqinsul", "aircond", "atticfin", "basefin", "bedrooms", "cdd30yr", "cellar", "cooltype", "cwasher", "dishwash", "division", "dntheat",
                     "doeid", "drafty", "dryer", "dryruse", "elcool", "elperiph", "elwarm", "equipm", "fowarm", "fuelheat", "hdd30yr", "highceil", "hhage",
                     "kownrent", "lpwarm", "moneypy", "moisture", "ncombath", "nhafbath", "notmoist", "numadult", "numchild", "numberac", "numcfan",
                     "numlaptop","numtablet","nweight","othrooms","oven","ovenfuel","ovenuse","outgrill","pool","prkgplc1","recbath","regionc","stove",
                     "stovefuel","stoven","stovenfuel","stories","swimpool","temphomeac","tempniteac","totrooms","tvcolor","typehuq","uatyp10",
                     "ugwarm","usefo","uselp","useng","usewood","washload","wdwarm","windows","yearmaderange")))



recs <- fread(x, select = recscols, header = TRUE, showProgress = FALSE)

colnames(recs)<-tolower(colnames(recs))
setnames(recs,"elperiph","pcprint")
setnames(recs,"regionc","region")
setnames(recs, "uatyp10","urban")
setnames(recs, "moneypy","income")
setnames(recs, "pool", "poolh")

recs <- data.frame(recs)
recs <- recs %>%
  mutate(urban = case_when(
    (urban == 'R' ~ 0),
    (urban != 'R'~ 1))) %>%
  mutate(location = 2*region -1 + urban) %>%
  filter(!is.na(location)) %>%
  mutate(housetyp = case_when(
    (typehuq == 2 | typehuq == 3) ~ 1,
    (typehuq == 4 | typehuq == 5) ~ 2,
    (typehuq == 1) ~3)) %>%
  filter(housetyp %in% 1:3) %>%
  mutate(famcat = case_when(
    (numadult == 1 & numchild == 0 )  ~ 1,
    (numadult == 1 & numchild > 0  )  ~ 2,
    (numadult > 1 & numchild == 0  )  ~ 3,
    (numadult > 1 & numchild >  0  )  ~ 4)) %>%
  filter(famcat %in% 1:4) %>%
  mutate(swim = with(recs, ifelse(
    poolh == 1 | swimpool ==1, 2, 0))) %>%
  mutate(swim = recbath+swim)


recslist<-split(recs,recs$location)
inccat3<-function(x){
  x<-x[order(x$income),]
  
  len3<-(trunc(nrow(x)/3)) # shortest
  len1<-round(((nrow(x)+.01)-len3)/2) # one higher if row remainder .33+
  len2<-(nrow(x)-(len1+len3)) # one higher if row remainder .66+
  
  index_1<-len1
  index_2<-len2+len1
  index_3<-len3+index_2
  
  x[c(0:index_1),]$income<-1
  x[c((index_1+1):index_2),]$income<-2
  x[c((index_2+1):index_3),]$income<-3
  return(x)
}
recslist<-lapply(recslist,inccat3)
recs<-rbind.fill(recslist)
rm(recslist)
setnames(recs,"income","inccat")

recs<- recs %>% mutate(pool = 36*(location-1)+12*(housetyp-1)+3*(famcat-1)+inccat)


RPGen_reportcard(recs)


recs<-recs[c("pool","doeid","nweight","hdd30yr","cdd30yr","kownrent","stories", "stove","stovefuel", "oven","ovenfuel",
             "ovenuse","stoven","outgrill","dishwash","cwasher","washload","dryer","dryruse","tvcolor", "numlaptop",

             "numtablet", "pcprint","moisture","prkgplc1", "cooltype",  "tempniteac", "numberac", "numcfan", "notmoist",
             "highceil",  "windows", "adqinsul", "drafty", "swim","cellar","region","urban","housetyp","famcat","inccat")]

save(recs, file = "./data/RPGen RECS.rda", compress = "xz")

end<-as.numeric((proc.time()[3]-start)/60)
runtime<- str_c("RPGen RECS downloaded in ", as.character(round(end,2)), " minutes.")

return(writeLines(runtime))
}

RPGen.RECS("https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv")
