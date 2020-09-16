# PUMS Downloader
# AE, ORAU, 2020

#' Downloads 2014-2018 PUMS Person and 2014-2018 PUMS Housing Data and Formats for RPGen
#'
#' Both Data inputs are 5 year surveys. 
#' 
#' Selects columns and creates pool input (urban, location, 
#' housetyp, famcat, inccat) to create pool. Codes in lot and unit
#' size.
#'
#' Data from https://www.census.gov/programs-surveys/acs/data/pums.html
#'
#' @param   pumspeople A hyperlink to PUMS 2014 - 2018 Person data
#' @param   pumshousing A hyperlink to PUMS 2014 - 2018 Housing data
#' @return  Runtime for the complete download and test.
#' @export  RPGen_PUMS_Region_1.rda,RPGen_PUMS_Region_2.rda,RPGen_PUMS_Region_3.rda,RPGen_PUMS_Region_4.rda
#'          (where the underscore is a space)

people ="https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_pus.zip"
housing  = "https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_hus.zip"

RPGen.PUMS <- function(pumspeople,pumshousing){
  
  suppressPackageStartupMessages(
  source("./R/Packages.R"))
  source("./tests/contents.R")
  source("./tests/report card.R")
  source("./tests/pool test.R") 

  start<-proc.time()[3]


PUMSNAME<-"PUMS_Houses_RPGen.zip"
pums_housing_variables<-toupper(c("adjinc", "hincp","np","region","serialno","puma","st","bld",
                                  "wgtp","veh","type"))


download(pumshousing, dest = PUMSNAME, mode = "wb")
unzip(PUMSNAME, files = c("psam_husa.csv","psam_husb.csv","psam_husc.csv","psam_husd.csv"))


pumsa<-fread("psam_husa.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
pumsb<-fread("psam_husb.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
pumsc<-fread("psam_husc.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
pumsd<-fread("psam_husd.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
pumsh<-rbind(pumsa,pumsb,pumsc,pumsd)
rm(pumsa,pumsb,pumsc,pumsd)
file.remove(c("psam_husa.csv","psam_husb.csv","psam_husc.csv","psam_husd.csv","PUMS_Houses_RPGen.zip"))
colnames(pumsh)<-tolower(colnames(pumsh))


pumsh<-pumsh[pumsh$type == 1 & pumsh$np > 0 & pumsh$region < 5]
pumsh<-pumsh[,!("type"), with = FALSE]


PUMSPNAME<-"PUMS_People_RPGen.zip"
pums_people_variables<-toupper(c("agep","sex","serialno","pwgtp","rac1p","hisp","jwmnp"))

download(pumspeople,dest =PUMSPNAME, mode = "wb")
unzip(PUMSPNAME, files = c("psam_pusa.csv","psam_pusb.csv","psam_pusc.csv","psam_pusd.csv"))
pumspa<-fread("psam_pusa.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
pumspb<-fread("psam_pusb.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
pumspc<-fread("psam_pusc.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
pumspd<-fread("psam_pusd.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
pumsp<-rbind(pumspa,pumspb,pumspc,pumspd)
rm(pumspa,pumspb,pumspc,pumspd)
colnames(pumsp)<-tolower(colnames(pumsp))

file.remove(c("psam_pusa.csv","psam_pusb.csv","psam_pusc.csv","psam_pusd.csv"))
file.remove(PUMSPNAME)
file.remove(PUMSNAME)

pums<-merge(pumsh,pumsp,by = "serialno")
rm(pumsh,pumsp)
pums<- pums[order(pums$serialno)]


# 1. Change names from PUMS naming to RPGen convention

setnames(pums, "veh","vehicles")
setnames(pums, "serialno","recno")
setnames(pums, "jwmnp","commute")
setnames(pums,"agep","age")
setnames(pums,"hincp","income")
setnames(pums,"rac1p","race")
setnames(pums,"sex","gender")
setnames(pums, "hisp", "ethnicity")
setnames(pums,"bld","housetyp")

# 2. Remove households with children living alone , create compid and income.

pums <- data.frame(pums)
pums <- pums %>% filter(!(age < 18 && np == 1))%>%
  select(-np) %>%
  mutate(puma = sprintf("%05d",puma)) %>%
  mutate(compid = as.numeric(paste0(st,puma))) %>%
  select(-puma, -st) %>%
  mutate( income = replace(income, (income < 0) , 0)) %>%
  mutate( income = round(income * (adjinc/1000000), digits =  2))%>%
  select( -adjinc)


# 3. Import and use puma_density.csv to determine urban or rural.

pumadensity<-get(load(file = "./data/RPGen PUMA Density.rda"))
colnames(pumadensity)<-tolower(colnames(pumadensity))
pumadensity <- pumadensity %>% select(compid, ur)%>%
  mutate(ur = case_when(
    (ur == "U") ~ 1,
    (ur == "R") ~ 0))

pums<-merge(pums,pumadensity, by = "compid")
rm(pumadensity)

# 4. Calculate Location.

pums<- pums %>% mutate(location = 2*region -1 + ur) %>%
  filter(location %in% 1:8)
  setnames(pums,"ur","urban")
  
# 5. Specify race, gender, ethnicity, and housetyp.

pums <- pums %>% mutate(race = case_when(
  (race == 1) ~ "W",
  (race == 2) ~ "B",
  (race == 3) ~ "N",
  (race == 4) ~ "N",
  (race == 5) ~ "N",
  (race == 6) ~ "A",
  (race == 7) ~ "P",
  (race == 8) ~ "O",
  (race == 9) ~ "O",)) %>%
  mutate(gender = case_when(
    (gender == 1) ~ "M",
    (gender == 2) ~ "F")) %>%
  mutate(ethnicity = case_when(
    (ethnicity >= 3) ~ "O",
    (ethnicity >= 2) ~ "M",
    (ethnicity >= 1) ~ "N",)) %>%
  mutate(housetyp = case_when(
    (housetyp == 2 | housetyp == 3) ~ 1,
    (housetyp %in% 4:9) ~ 2,
    (housetyp == 1 | housetyp == 10 | is.na(housetyp)) ~ 3))


# 6. Calculate income category and remove income

inccat.gen <- function(y){
  split.database<-split(y, y$location)
  
  inccat.per.location<-function(x){
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
  
  y<-rbind.fill(lapply(split.database,inccat.per.location))
  
  return(y)
}


pums<- inccat.gen(pums)
pums<- pums %>% select(-income)

# ______________ Tidying per household variables _____________________________ #

# 7. Create a subset of the dataset that only includes variables that need to go
# through per-household splitting and calculation.

pumstidy<-pums %>% select(!c("recno","compid","age","gender"))
pums<-pums %>% select(c("recno","compid","age","gender"))

pums <- dlply(pums,"compid")

# 8. Calculate famcats, genders, and ages.

famcats.to.compid.all<- function(z){
  famcats.genders.ages<-function(x){
    
    #  famcats
    numchild<- sum(x$age<18)
    numadults<- sum(x$age>=18)
    x$famcat<- with(x, ifelse(
      numadults == 1 & numchild == 0, 1,ifelse(
        numadults == 1 & numchild > 0, 2, ifelse(
          numadults > 1 & numchild == 0, 3, ifelse(
            numadults > 1 & numchild > 0, 4,NA)))))
    
    #  genders
    x$genders <- paste(x$gender, collapse = "")
    
    #  ages
    x$ages <- paste(x$age, collapse = "")
    
    return(x)
  }
  z<-dlply(z,"recno")
  z<-rbind.fill(lapply(z,famcats.genders.ages))
  return(z)
}

pums<- rbind.fill(lapply(pums,famcats.to.compid.all))
pums<-cbind(pums,pumstidy)
rm(pumstidy)

# 9. Calculate pool and select only relevant variables.

pums<- pums %>% filter(housetyp %in% 1:3, famcat %in% 1:4, inccat %in% 1:3) %>%
  mutate(pool = 36*(location-1) + 12*(housetyp-1) + 3*(famcat-1)+inccat)

RPGen_reportcard(pums)

pums<- pums %>% select(c("pool","compid","recno","gender","race","ethnicity","age","pwgtp","ages","genders",
   "commute","vehicles","region"))


# ______________ Export  _____________________________________________________ #

# 10. Split by region, and remove region.

pums<-dlply(pums, "region")
pums<-lapply(pums, function(x) x %>% select(-region))
names(pums)<-c("region1","region2","region3","region4")

# 11. Write files. 

list2env(pums,globalenv())
rm(pums)

save(region1, file = "./data/RPGen PUMS Region 1.rda", compress = "xz")
save(region2, file = "./data/RPGen PUMS Region 2.rda", compress = "xz")
save(region3, file = "./data/RPGen PUMS Region 3.rda", compress = "xz")
save(region4, file = "./data/RPGen PUMS Region 4.rda", compress = "xz")

# 12. Stop timer.

end<-as.numeric((proc.time()[3]-start)/60)
runtime<- str_c("RPGen PUMS downloaded in ", as.character(round(end,2)), " minutes.")

return(writeLines(runtime))
}

RPGen.PUMS(people,housing)
