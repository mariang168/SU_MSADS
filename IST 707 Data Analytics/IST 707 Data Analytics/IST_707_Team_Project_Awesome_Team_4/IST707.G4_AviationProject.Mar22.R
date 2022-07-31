###  IST707 Group4 Project                                     ### 
###  AVIATION ACCIDENTS AND INCIDENTS                          ###
###  Due: Mar.21.2020                                          ###
###  Authors: Editt GF, Maria Ng, Veasna Oum, Bhavya Madhavan  ###

## Installing & Loading Libraries
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("lubridate")
# install.packages("qdapRegex")
# install.packages("Hmisc")
# install.packages("reshape2")
# install.packages("gridExtra")
# install.packages("ggthemes")
# install.packages("maps")
# install.packages("zip")              
# install.packages("usmap")
# install.packages("ggmap")
# install.packages("gdata")
# install.packages("rgdal") 
# install.packages("mapdata")
# install.packages("Gifi")  
# install.packages("stringr")
# install.packages("vegan")
# install.packages("rsample")
# install.packages("fpc")
# install.packages("purrr")
# install.packages("dendextend")
# install.packages("rattle")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("Cairo")
# install.packages("e1071")
# install.packages("caret")
# install.packages("kernlab")
# install.packages("ksvm")
library(arules)       # association rule mining
library(arulesViz)    # arm visualization
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)         # for ddply function
library(cluster)      # cluster analysis
library(factoextra)   # cluster analysis
library(lubridate)    # for managing dates
library(qdapRegex)    # for extracting city/state info
library(Hmisc)        # for misc like %nin%
library(reshape2)
library(gridExtra)
library(ggthemes)
library(maps)
library(zip)          # US mapping
library(usmap)        # mapping solution
library(ggmap)
library(gdata)
library(rgdal) 
library(mapdata)
library(Gifi)         # for Princals: mix data PCA
library(stringr)      # manipulating strings (in attribute names and values)
library(vegan)        # for clustering plotting
library(rsample)
library(fpc)          # tabling cluster statistics
library(purrr)        # working with and plotting hierarchical clusters
library(dendextend)   # plotting hierarchical clusters - dendrogram
library(rattle)       # for DT
library(rpart)        # for DT
library(rpart.plot)   # for DT
library(RColorBrewer)
library(Cairo)        # for DT
library(e1071)        # for SVM, DT, prediction
library(caret)        # for SVM, DT, prediction
library(kernlab)      # for SVM and cluster plotting
library(ksvm)         # for SVM


## SETUP
# setwd("C:/Users/mng/OneDrive - BioMarin/Desktop/R")            # Maria's setup
# setwd("~/Downloads/University/IST707/Data")                    # Editt's setup
# setwd("/Users/bhavyamadhavan/IST_707_Data Analytics/Project")  # Bhavya's setup
# setwd("C:/Users/veo78/OneDrive/Desktop/MS DataScience Syracuse/R Working Directory/IST707_Project") # Veasna's setup
# setwd("~/Downloads/Aviation") 

## Read in .csv data
# filename="IST707-Data Analytic/AviationData.28Jan2020.csv"     # Maria's path
# filename="AviationProject/AviationData.28Jan2020.csv"          # Editt's path
# filename="AviationData.28Jan2020.csv"                          # Bhavya's path
# filename <- "AviationData.28Jan2020.csv"                       # Veasna's path

original.df<- read.csv(filename, header = TRUE, na.strings = "NA")
master.df<-original.df
small.df<-master.df[1:50,]       # a small df for trial and error


###########################################################################
###########################################################################
## PART 1. CLEANING, & PREP
###########################################################################
# 1. Review the data and attributes; eliminate columns
head(master.df)
str(master.df)
table(master.df$Report.Status)    # first we should eliminate the 4.8K rows with foreign reports 
head(master.df[master.df$Report.Status==" Foreign ",],100)
master.df<-master.df[-which(master.df$Report.Status==" Foreign "),]

cols2eliminate<-c("Event.Id","Investigation.Type","Accident.Number","Registration.Number","Report.Status","Publication.Date", "X")
master.df<-master.df[,!(names(master.df) %in% cols2eliminate)]

# 2. Review attribute types
str(master.df)

# 3. Handle missing values
#Check NAs
any(is.na(master.df[]))
sum(is.na(master.df[]))

#Total.Fatal.Injuries-convert NAs to zero
sum(is.na(master.df$Total.Fatal.Injuries))
master.df$Total.Fatal.Injuries[is.na(master.df$Total.Fatal.Injuries)]<-0
table(master.df$Total.Fatal.Injuries)
sum(is.na(master.df$Total.Fatal.Injuries))
str(master.df)

#Total.Serious.Injuries-convert NAs to zero
sum(is.na(master.df$Total.Serious.Injuries))
master.df$Total.Serious.Injuries[is.na(master.df$Total.Serious.Injuries)]<-0
table(master.df$Total.Serious.Injuries)
sum(is.na(master.df$Total.Serious.Injuries))
str(master.df)

#Total.Minor.Injuries -convert NAs to zero
sum(is.na(master.df$Total.Minor.Injuries ))
master.df$Total.Minor.Injuries [is.na(master.df$Total.Minor.Injuries )]<-0
table(master.df$Total.Minor.Injuries)
sum(is.na(master.df$Total.Minor.Injuries ))
str(master.df)


#Total.Total.Uninjured 
sum(is.na(master.df$ Total.Uninjured  ))     # there are 11,691 NAs.
# The plan: populate 'uninjured' following rules; then generate total people column; normalize 'fatal','serious','minor','uninjured' as fractions of total people.
### rules found in the data: 
# 1. when uninjured =NA, and one of the other 3 is >0, then uninjured should be 0 ; total people= sum of all 4 columns.
# 2. when uninjured >0, then again total people = sum of all 4 columns.
# 3. when uninjured=0 and all other 3 values are 0, then it's an incident with no injuries, and we don't know how many total people. 
# that's ok: we DONT USE TOTAL.PEOPLE FOR ANALYSIS, just normalization. we'll let total people=0 and uninjured=0. after normalization:
# -the values in 'fatal','serious','minor' will be 0, because 0 of total people are injured (0%/TP)
# -the value in 'uninjured' should be 1 because it's a fraction and 100% of total people are uninjured (100%/TP)
## in summary: if uninjured is NA convert it to zero.
# during normalization, when total people=0, each of the columns will get 0, but uninjured will get 1.
master.df$Total.Uninjured [is.na(master.df$Total.Uninjured )]<-0
table(master.df$Total.Uninjured)
# see later below for feature generation of 'total people', and normalization, and fixing the case of 0-0-0-0.


#Number.of.Engines -convert NAs to 1  # if aircraft has more than 1 it would be reported. most privates have 1.
sum(is.na(master.df$Number.of.Engines))
master.df$Number.of.Engines[is.na(master.df$Number.of.Engines )]<- 1
table(master.df$Number.of.Engines) 
master.df[master.df$Number.of.Engines==0,] # there is one entry with 0 engines and it should be 1 engine
master.df$Number.of.Engines[master.df$Number.of.Engines==0]<- 1
table(master.df$Number.of.Engines) 
master.df$Number.of.Engines<-as.factor(master.df$Number.of.Engines)
str(master.df)

#Anymore NAs
any(is.na(master.df[]))
sum(is.na(master.df[]))
str(master.df)

# 4. Inspect for invalid values or values in bad format
# Weather.Condition has 2 bad levels to eliminate: "  " and " UKN ", and 2 levels with extra spaces to clean.
summary(master.df$Weather.Condition)
levels(master.df$Weather.Condition)
master.df<-master.df[-which(master.df$Weather.Condition==" UNK "),]    # eliminate UNK rows ~about 1K
master.df<-master.df[-which(master.df$Weather.Condition=="  "),]       # eliminate "  " rows ~about 3K
levels(master.df$Weather.Condition)[levels(master.df$Weather.Condition)==" IMC "]<-"Instruments"
levels(master.df$Weather.Condition)[levels(master.df$Weather.Condition)==" VMC "]<-"Visual"
master.df$Weather.Condition<-factor(master.df$Weather.Condition)       # refactoring
table(master.df$Weather.Condition)

# Injury.Severity has 128 levels because it has parenthesis with # of fatalities in the values.
# removing the extra info so that we only have the categrical info left, with a small number of levels.
table(master.df$Injury.Severity)
master.df$Injury.Severity <- as.factor(str_trim(gsub("Fatal.*$", "Fatal", as.character(master.df$Injury.Severity))))
# replacing a level with a more meaningful value
levels(master.df$Injury.Severity)[levels(master.df$Injury.Severity)=="Incident"]<-"No-Injury"
levels(master.df$Injury.Severity)
table(master.df$Injury.Severity)
# "Unavailable" means there were no injuries.
master.df$Injury.Severity[master.df$Injury.Severity=="Unavailable"]<-"No-Injury"
master.df$Injury.Severity<-factor(master.df$Injury.Severity)   # refactoring
table(master.df$Injury.Severity)

# check the levels of Aircraft.Damage
levels(master.df$Aircraft.Damage)
table(master.df$Aircraft.Damage)
master.df$Aircraft.Damage <- str_trim(master.df$Aircraft.Damage)   # cleaning empty spaces in levels
master.df$Aircraft.Damage[master.df$Aircraft.Damage==""]<-"Minor"
master.df$Aircraft.Damage <- factor(master.df$Aircraft.Damage, ordered=TRUE, levels=c("Minor","Substantial","Destroyed"))
table(master.df$Aircraft.Damage)

# check the levels of Aircraft.Category 
levels(master.df$Aircraft.Category)
table(master.df$Aircraft.Category)
# cleaning empty spaces in levels
master.df$Aircraft.Category <- as.factor(str_trim(master.df$Aircraft.Category))
# of 14 categories, significant ones are: airplanes, helicopters, balloons/blimp and gliders. the rest group into light-sport
# Unknown can be grouped into light-sport. "" can be grouped into airplane, there are many and it should be the default.
master.df$Aircraft.Category[master.df$Aircraft.Category==""]<-"Airplane"
levels(master.df$Aircraft.Category)[levels(master.df$Aircraft.Category)=="Unknown"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Blimp"]<-"Balloon"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Gyrocraft"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Gyroplane"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Powered Parachute"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Powered-Lift"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Rocket"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Ultralight"]<-"LightSport"
master.df$Aircraft.Category[master.df$Aircraft.Category=="Weight-Shift"]<-"LightSport"
master.df$Aircraft.Category <- factor(master.df$Aircraft.Category)
table(master.df$Aircraft.Category)

# Check the levels of Amateur.Built
# Anything "" should be "No". that's the default and they do have make and model, so they are not built by amateurs
table(master.df$Amateur.Built)
master.df$Amateur.Built <- as.factor(str_trim(master.df$Amateur.Built))
master.df$Amateur.Built[master.df$Amateur.Built==""]<-"No"
master.df$Amateur.Built <- factor(master.df$Amateur.Built)
table(master.df$Amateur.Built)

# Check the levels of Engine.Type. 15 levels - needs to be cleaned.
levels(master.df$Engine.Type)
table(master.df$Engine.Type)
master.df$Engine.Type <- as.factor(str_trim(master.df$Engine.Type))
levels(master.df$Engine.Type)[levels(master.df$Engine.Type)=="Turbo Fan"]<-"Turbofan"
levels(master.df$Engine.Type)[levels(master.df$Engine.Type)=="Turbo Jet"]<-"Turbojet"
levels(master.df$Engine.Type)[levels(master.df$Engine.Type)=="Turbo Prop"]<-"Turboprop"
levels(master.df$Engine.Type)[levels(master.df$Engine.Type)=="Turbo Shaft"]<-"Turboshaft"
# special old aircraft with both turboject and reciprocating will be listed under Turbojet, the older type before Turbofan.
master.df$Engine.Type[master.df$Engine.Type=="TJ, REC, REC, TJ"]<-"Turbojet"
master.df$Engine.Type[master.df$Engine.Type=="REC, TJ, REC, TJ"]<-"Turbojet"
master.df$Engine.Type[master.df$Engine.Type=="REC, TJ, TJ"]<-"Turbojet"
master.df$Engine.Type[master.df$Engine.Type=="REC, ELEC"]<-"Electric"
master.df$Engine.Type[master.df$Engine.Type==""]<-"Unknown"
# eliminate the outliers: the rocket, electric, none, and unknown
master.df<-master.df[-which(master.df$Engine.Type=="Hybrid Rocket"),]
master.df<-master.df[-which(master.df$Engine.Type=="Electric"),]
master.df<-master.df[-which(master.df$Engine.Type=="None"),]
master.df<-master.df[-which(master.df$Engine.Type=="Unknown"),]
master.df$Engine.Type<-factor(master.df$Engine.Type)
table(master.df$Engine.Type)
str(master.df)

# check FAR.Description
# The Federal Aviation Regulations (FARs) are rules prescribed by the Federal Aviation Administration (FAA) governing all  
# aviation activities in the United States. The FARs are part of Title 14 of the Code of Federal Regulations (CFR). 
levels(master.df$FAR.Description)
table(master.df$FAR.Description)
master.df$FAR.Description <- as.factor(str_trim(master.df$FAR.Description))
master.df$FAR.Description[master.df$FAR.Description==""]<-"Unknown"
levels(master.df$FAR.Description)[levels(master.df$FAR.Description)=="Part 91F: Special Flt Ops."]<-"Part 91F: SpecialOps"
master.df$FAR.Description[master.df$FAR.Description=="Public Aircraft"]<-"Part 91F: SpecialOps"
master.df$FAR.Description[master.df$FAR.Description=="Public Use"]<-"Part 91F: SpecialOps"
master.df$FAR.Description[master.df$FAR.Description=="Part 91 Subpart K: Fractional"]<-"Part 91F: SpecialOps"
master.df$FAR.Description[master.df$FAR.Description=="Unknown"]<-"Part 91: General Aviation" # reclassify unknown as GA
# eliminate the outliers 
master.df<-master.df[-which(master.df$FAR.Description=="Armed Forces"),]
master.df<-master.df[-which(master.df$FAR.Description=="Non-U.S., Commercial"),]
master.df<-master.df[-which(master.df$FAR.Description=="Non-U.S., Non-Commercial"),]
master.df$FAR.Description<-factor(master.df$FAR.Description)
table(master.df$FAR.Description)
str(master.df)

# Event date is factor which needs to be converted to a date. We should probably not use this column for analysis
# just used the 3 new generated columns!
master.df$Event.Date<- as.Date(master.df$Event.Date, format = "%m/%d/%y")   #specify existing date format,for function to work
master.df$WeekDay<- as.factor(weekdays(master.df$Event.Date))
master.df$Month<- as.factor(month(master.df$Event.Date))
master.df$Year<- as.factor(year(master.df$Event.Date))
master.df$Year<- ordered(year(master.df$Event.Date))  # clean outliers that would skew the annual data
master.df<-master.df[(1981 < master.df$Year)& (master.df$Year < 2020),] # too few before 1982 and after 2019
master.df$Year<- factor(master.df$Year)
table(master.df$Year)

# check Schedule - it should be scheduled only for commercial flights. default is NonScheduled. leave unknown as is.
table(master.df$Schedule)
master.df$Schedule <- as.factor(str_trim(master.df$Schedule))
levels(master.df$Schedule)[levels(master.df$Schedule)=="NSCH"]<-"NonScheduled"
levels(master.df$Schedule)[levels(master.df$Schedule)=="SCHD"]<-"Scheduled"
master.df$Schedule[master.df$Schedule==""]<-"NonScheduled"
master.df$Schedule<-factor(master.df$Schedule)
table(master.df$Schedule)
str(master.df)

# check purpose of flight
levels(master.df$Purpose.of.Flight)
table(master.df$Purpose.of.Flight)
master.df$Purpose.of.Flight <- as.factor(str_trim(master.df$Purpose.of.Flight))
# combine business and Executive/Corporate, combine air drop and skydiving, combine positioning and aerial observation;
# change personal to private and combine "", change aerial application to agriculture, combine all public aircraft
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Executive/Corporate"]<-"Business"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Air Drop"]<-"Skydiving"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Positioning"]<-"Aerial Observation"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Aerial Observation"]<-"Aerial_Observation"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Personal"]<-"Private"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)==""]<-"Private"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Air Race/Show"]<-"AirShow"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Public Aircraft"]<-"Public_Aircraft"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Public Aircraft - State"]<-"Public_Aircraft"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Public Aircraft - Federal"]<-"Public_Aircraft"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Public Aircraft - Local"]<-"Public_Aircraft"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Aerial Application"]<-"Agriculture"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Banner Tow"]<-"BannerTow"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Glider Tow"]<-"GliderTow"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="External Load"]<-"ExternalLoad"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Flight Test"]<-"FlightTest"
levels(master.df$Purpose.of.Flight)[levels(master.df$Purpose.of.Flight)=="Other Work Use"]<-"OtherWork"
# unknown: if part 137 change to agriculture, if 135 change to Ferry, if 121 change to commercialairline
master.df$Purpose.of.Flight[master.df$Purpose.of.Flight=="Unknown" & 
                              master.df$FAR.Description=="Part 137: Agricultural"]<-"Agriculture"
master.df$Purpose.of.Flight[master.df$Purpose.of.Flight=="Unknown" & 
                              master.df$FAR.Description=="Part 135: Air Taxi & Commuter"]<-"Ferry"
levels(master.df$Purpose.of.Flight)<-c(levels(master.df$Purpose.of.Flight),"CommercialAirline")
master.df$Purpose.of.Flight[master.df$Purpose.of.Flight=="Unknown" & 
                              master.df$FAR.Description=="Part 121: Air Carrier"]<-"CommercialAirline"
table(master.df$Purpose.of.Flight)

# check phase of flight
table(master.df$Broad.Phase.of.Flight)
levels(master.df$Broad.Phase.of.Flight)  # combine "", other, and unknown
master.df$Broad.Phase.of.Flight <- as.factor(str_trim(master.df$Broad.Phase.of.Flight))
levels(master.df$Broad.Phase.of.Flight)[levels(master.df$Broad.Phase.of.Flight)=="OTHER"]<-"UNKNOWN"
levels(master.df$Broad.Phase.of.Flight)[levels(master.df$Broad.Phase.of.Flight)==""]<-"UNKNOWN"
table(master.df$Broad.Phase.of.Flight)

# check Location
# Location is combination of city and state (for the most part)
master.df$Location <- as.factor(str_trim(toupper(master.df$Location)))
head(master.df$Location)
str(master.df$Location)
sum(is.na(master.df$Location))

# extract state
data(state)
master.df$State.abb<-NULL
res<-str_split(master.df$Location,",")    # split the location column
master.df$State.abb<- sapply(res,`[`,2)   # access and keep the 2nd component in every element in the list of results
master.df$State.abb<- as.factor(str_trim(master.df$State.abb))
master.df$State.abb[master.df$State.abb %nin% c(state.abb,"DC") ] <-"UNKNOWN" # mark strings not in known state-list  

# extract city
master.df$City<-NULL
master.df$City<- sapply(res,`[`,1)       # access and keep the 1st component in every element in the list of results
master.df$City<- as.factor(str_trim(master.df$City))
master.df$City<-as.factor(tolower(master.df$City))

master.df<-master.df[-which(master.df$State.abb=="UNKNOWN"),]                 # eliminate 828 rows of unclear locations
master.df$State.abb<-factor(master.df$State.abb)
master.df$City<-factor(master.df$City)
levels(master.df$City)
levels(master.df$State.abb)
table(master.df$State.abb)
table(master.df$Country)                                                     # all data in US
master.df<-master.df[,!(names(master.df) %in% c("Location","Country"))]      # dont need Location/Country columns anymore
str(master.df)


# check Airport.Code 
master.df$Airport.Code <- as.factor(str_trim(master.df$Airport.Code))
head(master.df$Airport.Code)
str(master.df$Airport.Code)

# check Airport.Name
master.df$Airport.Name <- as.factor(str_trim(tolower(master.df$Airport.Name)))
head(master.df$Airport.Name)
str(master.df$Airport.Name)

# check Make
master.df$Make <- as.factor(str_trim(tolower(master.df$Make)))
head(master.df$Make)
str(master.df$Make)

# check Model
master.df$Model <- as.factor(str_trim(tolower(master.df$Model)))
head(master.df$Model)
str(master.df$Model)

# check Air.Carrier
master.df$Air.Carrier <- as.factor(str_trim(tolower(master.df$Air.Carrier)))
head(master.df$Air.Carrier)
str(master.df$Air.Carrier)
str(master.df)

# 5. Feature generation 
# generate a 'Total.People' column
master.df$Total.People <- master.df$Total.Fatal.Injuries + master.df$Total.Minor.Injuries + 
  master.df$Total.Serious.Injuries + master.df$Total.Uninjured
nrow(master.df[master.df$Total.People ==0,]) # 38 rows with total people=0
table(master.df$Total.People)
table(master.df$Total.Uninjured)            

# 6. Transformation: normalization
# normalizing fatalities and injuries as fraction of total people. 
# if fatalities or injuries=0, the result should be 0, no matter what we have in total people. 
# otherwise total people >0 so division is safe.
safe_normalize <- function(x, y) { ifelse(x==0,0,round(x/y,2)) }

# uninjured is a special case. 
# if (uninjured / people) = (0/y>0) , the result should be 0.  But if (uninjured / people) = (0/0) 
# then the result should be 1 because 100% of people are uninjured.
safe_normalize_uninjured <- function(x, y) { ifelse(y==0,1,round(x/y,2)) }

master.df$Total.Fatal.Injuries <- safe_normalize(master.df$Total.Fatal.Injuries,master.df$Total.People)
master.df$Total.Minor.Injuries <-  safe_normalize(master.df$Total.Minor.Injuries,master.df$Total.People)
master.df$Total.Serious.Injuries <- safe_normalize(master.df$Total.Serious.Injuries,master.df$Total.People)
master.df$Total.Uninjured <- safe_normalize_uninjured(master.df$Total.Uninjured,master.df$Total.People)

table(master.df$Total.Fatal.Injuries)
table(master.df$Total.Serious.Injuries)
table(master.df$Total.Minor.Injuries)
table(master.df$Total.Uninjured)

# Transformation: going binary
# combining No-Injury and Non-Fatal so that Injury severity becomes binary
table(master.df$Injury.Severity)
levels(master.df$Injury.Severity)[levels(master.df$Injury.Severity)=="No-Injury"]<-"Non-Fatal"
table(master.df$Injury.Severity)


# 7. Transformation: Discretization
# binning some of the numeric variables
# NOTE: making a copy of the master.df
bin.df <- master.df
max(bin.df$Total.Fatal.Injuries)
bin.df$Total.Fatal.Injuries <- cut(bin.df$Total.Fatal.Injuries,
                                          breaks = c(-1,0,0.2,0.4,0.6,0.8,1),
                                          labels=c("Zero","0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"))
max(bin.df$Total.Minor.Injuries)
bin.df$Total.Minor.Injuries <- cut(bin.df$Total.Minor.Injuries,
                                          breaks = c(-1,0,0.2,0.4,0.6,0.8,1),
                                          labels=c("Zero","0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"))
max(bin.df$Total.Serious.Injuries)
bin.df$Total.Serious.Injuries <- cut(bin.df$Total.Serious.Injuries,
                                            breaks = c(-1,0,0.2,0.4,0.6,0.8,1),
                                            labels=c("Zero","0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"))
max(bin.df$Total.Uninjured)
bin.df$Total.Uninjured <- cut(bin.df$Total.Uninjured,
                                            breaks = c(-1,0,0.2,0.4,0.6,0.8,1),
                                            labels=c("Zero","0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"))
summary(bin.df$Total.Uninjured)
str(bin.df)



###########################################################################
## PART 2. EXPLORATION AND VSIUALIZATION
# see this section at the bottom
###########################################################################


###########################################################################
## PART 3. MODELS
# PREPARATION
###########################################################################
# A. subset dataframe for analysis: "numeric.df" (17 columns chosen out of 29)
numeric.df.names<-c("Injury.Severity","Aircraft.Damage","Aircraft.Category","Amateur.Built","Number.of.Engines",
                    "Engine.Type","FAR.Description","Schedule","Purpose.of.Flight","Total.Fatal.Injuries",
                    "Total.Serious.Injuries","Total.Minor.Injuries","Total.Uninjured","Weather.Condition",
                    "Broad.Phase.of.Flight","WeekDay","Month")
numeric.df<-subset(master.df, select=numeric.df.names)
# transforming all non-numeric columns to ordered factors
indx <- sapply(numeric.df, is.factor)
numeric.df[indx] <- lapply(numeric.df[indx], function(x) ordered(x))
str(numeric.df)

################################################################
# B. subset dataframe for categorical/nominal analysis: "bin.df" (17 columns of 29)
bin.df.names<-c("Injury.Severity","Aircraft.Damage","Aircraft.Category","Amateur.Built","Number.of.Engines",
                   "Engine.Type","FAR.Description","Schedule","Purpose.of.Flight","Total.Fatal.Injuries",
                   "Total.Serious.Injuries","Total.Minor.Injuries","Total.Uninjured","Weather.Condition",
                   "Broad.Phase.of.Flight","WeekDay","Month")
bin.df<-subset(bin.df, select=bin.df.names)
# transforming all non-numeric columns to ordered factors
indx <- sapply(bin.df, is.factor)
bin.df[indx] <- lapply(bin.df[indx], function(x) ordered(x))
str(bin.df)

###############################################################
# C. subset dataframe for categorical/nominal analysis without the 'binned' columns
clean.df.names<-c("Injury.Severity","Aircraft.Damage","Aircraft.Category","Amateur.Built","Number.of.Engines",
                    "Engine.Type","FAR.Description","Schedule","Purpose.of.Flight","Weather.Condition",
                    "Broad.Phase.of.Flight","WeekDay","Month")
clean.df<-subset(master.df, select=clean.df.names)
# transforming all non-numeric columns to ordered factors
indx <- sapply(clean.df, is.factor)
clean.df[indx] <- lapply(clean.df[indx], function(x) ordered(x))
str(clean.df)


######## THE MODELS ########
###########################################################################
## 1. ASSOCIATION RULE MINING
###########################################################################
# ARM requires all data to be factors.
#using the categorical/nominal dataframe bin.df
arm.df <- bin.df
#Removing the following columns for ARM analysis
#FAR.Description,Number.of.Engines,
#Engine.Type, Total.Fatal.Injuries, Total.Serious.Injuries,Total.Minor.Injuries  
#Total.Uninjured, Aircraft.Damage, WeekDay,Month
arm.df <-arm.df[ , -which(names(arm.df) %in%  c("FAR.Description","Number.of.Engines","Engine.Type","Total.Fatal.Injuries","Total.Serious.Injuries",
                                                "Total.Minor.Injuries","Total.Uninjured", "Aircraft.Damage", "WeekDay","Month"))]
str(arm.df)

myRules = arules::apriori(arm.df, parameter = list(supp = 0.001, conf = 0.7,maxlen = 4),
                          appearance = list(default="lhs", rhs=list("Injury.Severity=Fatal")))
inspect(myRules)

#include make, model and Aircraft.Category
str(master.df)
arm.df1 <- master.df
arm.df1 <-arm.df1[ , -which(names(arm.df1) %in%  c("FAR.Description","Number.of.Engines","Total.Fatal.Injuries","Total.Serious.Injuries",
                                                   "Total.Minor.Injuries","Total.Uninjured", "Aircraft.Damage", "WeekDay","Month"))]

#arm.df1 <-arm.df1[ , -which(names(arm.df1) %in%  c("FAR.Description","Number.of.Engines","Engine.Type","Total.Fatal.Injuries","Total.Serious.Injuries",
#                                              "Total.Minor.Injuries","Total.Uninjured", "Aircraft.Damage", "WeekDay","Month"))]
arm.df1 <-arm.df1[ , -which(names(arm.df1) %in%  c("Airport.Name","Airport.Code","Latitude","Year",
                                                   "Longitude","Event.Date", "City", "Total.People","State.abb"))]

str(arm.df1)
#removing rows with empty Air Carrier
arm.df1 <- arm.df1[-which(arm.df1$Air.Carrier == ""), ]
myRules1 = arules::apriori(arm.df1, parameter = list(supp = 0.001, conf = 0.85,maxlen = 4),
                           appearance = list(default="lhs", rhs=list("Injury.Severity=Fatal")))

inspect(myRules1)

plot(myRules1,method = "graph")
plot(myRules1,method = "graph",engine = "interactive")

#Non fatal
myRules2 = arules::apriori(arm.df1, parameter = list(supp = 0.1, conf = 0.9,maxlen = 3),
                           appearance = list(default="lhs", rhs=list("Injury.Severity=Non-Fatal")))

inspect(myRules2)
plot(myRules2,method = "graph")
plot(myRules2,method = "graph",engine = "interactive")




###########################################################################
## 2. CLUSTERING
###########################################################################
# Clustering  algorithms prefer numeric data, but we need to work with categorical data.
# the daisy distance function and the gower algorithm are tailored for nominal attributes.
# the pam algorithm is used instead of k-means as a k-mediod type, again for nominal attr.
cl.df<-clean.df[1:7500,]      # work with 10% of data, otherwise out of capacity
cl.df.nolabel<- cl.df[,-1]    # removing Injury.severity 'label'
str(cl.df.nolabel)
set.seed(10)
gower.dist<- daisy(cl.df.nolabel,metric=c("gower"))  # distance matrix using 'gower' method

# K-Means
k.model<- pam(gower.dist, k=4)                       # k-mediods instead of k-means
sil<- silhouette(k.model)                            # evaluate; avg silhouette value range is: (-1:1)
plot(sil, col=4:7, border=NA)                        # the closer to (1) the better, represents distance between clusters
# These are the results of evaluating k range (2:6)
# k=2  avg sil =0.24
# k=3  avg sil =0.20
# k=4  avg sil =0.28 -> this seems to be the best result
# k=5  avg sil =0.23
# k=6  avg sil =0.23
# Now need to visualize the clusters
clustergroups<-data.frame(cl.df,k.model$clustering)
view(clustergroups)
clusplot(cl.df.nolabel,k.model$clustering) # result: overlapping clusters
# 2 components plotted explain 27.83% of the point variability.
# plotting using 'Princals' object, algorithm for Principal Component Analysis
# for categorical data
pc<-princals(cl.df.nolabel)
str(pc)
plot(pc, plot.type="screeplot")
plot(pc) # loadings plot
# The loading plot graphs the coefficients of each variable for the first component versus the coefficients for the second component.
# Use the loading plot to identify which variables have the largest effect on each component. Loadings can range from -1 to 1. 
# Loadings close to -1 or 1 indicate that the variable strongly influences the component. Loadings close to 0 indicate that the variable 
# has a weak influence on the component. Evaluating the loadings can also help you characterize each component in terms of the variables.

# hierarchical clustering: AGGLOMERATIVE CLUSTERING - Complete linkages (usually the most balanced approach)
# some guidance found here: 
# https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c, main = "Agglomerative, complete linkages")
# Rainbow color option: it doesn't know how many clusters, uses a rainbow of colors
colorclusters <-color_branches(dendro)
plot(colorclusters, main="Agglomerative Clustering, Complete Linkage ")  

# evaluating the number of clusters: creating a table of cluster statistics, then doing a grid search for k
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
    }
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

# creating the cluster statistics for k ranging from 2:15           # this took hours to run, expanding the search for k
stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 15) 
stats.df.aggl
# plotting the silhouette coefficients for k ranging from 2:15
g<-ggplot(data = data.frame(t(stats.df.aggl)), 
          aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
g
# we could go for the number of clusters as 3 or 7. 3 has highest silhouette, 7 separates better the big group.
# plotting a dendrogram
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 7, value =   c("darkslategray", "darkslategray4", "darkslategray3", "orange", "darkcyan", "cyan3","gold")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)          # this part takes a few minutes
ggplot(ggd1, theme=theme_minimal()) + labs(x="Num. observations", y="Height", title="Dendrogram, k=7")
# as alternative to using ggplot in the last 2 commands:
plot(dendro.col)
# Radial dendrogram plot!
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta="x")





###########################################################################
## 3. DECISION TREES
###########################################################################
# analysis of categorical values
Aviations<-master.df
##print it...
(head(Aviations,n=5))
str(Aviations)
Aviations$Month<-as.factor(Aviations$Month)
str(Aviations)

cols2eliminate<-c("Event.Date","Location","Country","Airport.Code","Airport.Name","Total.People", "Year", "Make", "Model", "Air.Carrier")
Aviations<-Aviations[,!(names(Aviations) %in% cols2eliminate)]
str(Aviations)
##rsample package:
library(rsample)  
# choosing 75% of the data to be the training data
Aviations_split <- initial_split(Aviations, prop = .75)
# extracting training data and test data as two seperate dataframes
Aviations_train <- training(Aviations_split)
Aviations_test  <- testing(Aviations_split)

######################################### Clean and check data ----------------------------
##Remove unnecessary columns
str(Aviations_train)
str(Aviations_test)

## How many rows are complete?
cat("The Aviation Test data has a total of ", nrow(Aviations_test), "rows.")
cat("The Aviation Train data has a total of ", nrow(Aviations_train), "rows.")

TotalCompleteRowsTrain <- (nrow(Aviations_train[complete.cases(Aviations_train),]))
TotalCompleteRowsTest <- (nrow(Aviations_test[complete.cases(Aviations_test),]))

cat("The Aviation Train data has a total of ", TotalCompleteRowsTrain, "complete rows.")
cat("The Aviation Test data has a total of ", TotalCompleteRowsTest, "complete rows.")

## Now its time to discretize
## Let's look at the str and tables

(str(Aviations_test))
(str(Aviations_train))

## change Number.of.Engines to a factor
Aviations_test$Number.of.Engines=factor(Aviations_test$Number.of.Engines)
Aviations_train$Number.of.Engines=factor(Aviations_train$Number.of.Engines)

(freq=table(Aviations_test$Total.Fatal.Injuries))

## Bin
Aviations_test$Total.Fatal.Injuries <- cut(Aviations_test$Total.Fatal.Injuries,
                                           breaks = c(-1,0,1),
                                           labels=c("0","1"))
Aviations_train$Total.Fatal.Injuries <- cut(Aviations_train$Total.Fatal.Injuries,
                                            breaks = c(-1,0,1),
                                            labels=c("0","1"))

Aviations_test$Total.Minor.Injuries <- cut(Aviations_test$Total.Minor.Injuries,
                                           breaks = c(-1,0,1),
                                           labels=c("0","1"))
Aviations_train$Total.Minor.Injuries <- cut(Aviations_train$Total.Minor.Injuries,
                                            breaks = c(-1,0,1),
                                            labels=c("0","1"))

Aviations_test$Total.Serious.Injuries <- cut(Aviations_test$Total.Serious.Injuries,
                                             breaks = c(-1,0,1),
                                             labels=c("0","1"))
Aviations_train$Total.Serious.Injuries <- cut(Aviations_train$Total.Serious.Injuries,
                                              breaks = c(-1,0,1),
                                              labels=c("0","1"))

Aviations_test$Total.Uninjured <- cut(Aviations_test$Total.Uninjured,
                                      breaks = c(-1,0,1),
                                      labels=c("0","1"))
Aviations_train$Total.Uninjured <- cut(Aviations_train$Total.Uninjured,
                                       breaks = c(-1,0,1),
                                       labels=c("0","1"))

str(Aviations_test)

########################## BUILD Decision Trees #############################
## Create the decision tree using rpart for screening
fit1 <- rpart(Aviations_train$Aircraft.Damage ~ ., data = Aviations_train, method="class")
summary(fit1)
(predicted= predict(fit1,Aviations_test, type="class"))
fancyRpartPlot(fit1)

fit1_pred <- predict(fit1, Aviations_test, type = "class")
fit1_pred

confusionMatrix(Aviations_test$Aircraft.Damage, fit1_pred)
#accuracy= 86.3%


fit2 <- rpart(Aviations_train$Aircraft.Category ~ ., data = Aviations_train, method="class")
summary(fit2)
(predicted= predict(fit2,Aviations_test, type="class"))
fancyRpartPlot(fit2)

fit2_pred <- predict(fit2, Aviations_test, type = "class")
fit2_pred

confusionMatrix(Aviations_test$Aircraft.Category, fit2_pred)
#accuracy= 97.16%


fit3 <- rpart(Aviations_train$Injury.Severity ~ ., data = Aviations_train, method="class")
summary(fit3)
(predicted= predict(fit3,Aviations_test, type="class"))
fancyRpartPlot(fit3)

fit3_pred <- predict(fit3, Aviations_test, type = "class")
fit3_pred

confusionMatrix(Aviations_test$Injury.Severity, fit3_pred)
#accuracy= 99.41%


fit4 <- rpart(Aviations_train$Number.of.Engines ~ ., data = Aviations_train, method="class")
summary(fit4)
(predicted= predict(fit4,Aviations_test, type="class"))
fancyRpartPlot(fit4)

fit4_pred <- predict(fit4, Aviations_test, type = "class")
fit4_pred

confusionMatrix(Aviations_test$Number.of.Engines, fit4_pred)
#accuracy= 89.98%


fit5 <- rpart(Aviations_train$Engine.Type ~ ., data =Aviations_train, method="class")
summary(fit5)
(predicted= predict(fit5,Aviations_test, type="class"))
fancyRpartPlot(fit5)

fit5_pred <- predict(fit5, Aviations_test, type = "class")
fit5_pred

confusionMatrix(Aviations_test$Engine.Type, fit5_pred)
#accuracy= 89.77%

fit6 <- rpart(Aviations_train$FAR.Description ~ ., data = Aviations_train, method="class")
summary(fit6)
(predicted= predict(fit6,Aviations_test, type="class"))
fancyRpartPlot(fit6)

fit6_pred <- predict(fit6, Aviations_test, type = "class")
fit6_pred

confusionMatrix(Aviations_test$FAR.Description, fit6_pred)
#accuracy= 97%


fit7 <- rpart(Aviations_train$Schedule ~ ., data =Aviations_train, method="class")
summary(fit7)
(predicted= predict(fit7,Aviations_test, type="class"))
fancyRpartPlot(fit7)

fit7_pred <- predict(fit7, Aviations_test, type = "class")
fit7_pred

confusionMatrix(Aviations_test$Schedule, fit7_pred)
#accuracy= 96.9%


fit8 <- rpart(Aviations_train$Purpose.of.Flight ~ ., data = Aviations_train, method="class")
summary(fit8)
(predicted= predict(fit8,Aviations_test, type="class"))
fancyRpartPlot(fit8)

fit8_pred <- predict(fit8, Aviations_test, type = "class")
fit8_pred

confusionMatrix(Aviations_test$Purpose.of.Flight, fit8_pred)
#accuracy= 67.97%


fit9 <- rpart(Aviations_train$Total.Fatal.Injuries ~ ., data = Aviations_train, method="class")
summary(fit9)
(predicted= predict(fit9,Aviations_test, type="class"))
fancyRpartPlot(fit9)

fit9_pred <- predict(fit9, Aviations_test, type = "class")
fit9_pred

confusionMatrix(Aviations_test$Total.Fatal.Injuries, fit9_pred)
#accuracy= 99.97%



fit10 <- rpart(Aviations_train$Total.Serious.Injuries ~ ., data = Aviations_train, method="class")
summary(fit10)
(predicted= predict(fit10,Aviations_test, type="class"))
fancyRpartPlot(fit10)

fit10_pred <- predict(fit10, Aviations_test, type = "class")
fit10_pred

confusionMatrix(Aviations_test$Total.Serious.Injuries, fit10_pred)
#accuracy= 93.8%



fit11 <- rpart(Aviations_train$Total.Minor.Injuries ~ ., data = Aviations_train, method="class")
summary(fit11)
(predicted= predict(fit11,Aviations_test, type="class"))
fancyRpartPlot(fit11)

fit11_pred <- predict(fit11, Aviations_test, type = "class")
fit11_pred

confusionMatrix(Aviations_test$Total.Minor.Injuries, fit11_pred)
#accuracy= 92.23%


fit12 <- rpart(Aviations_train$Total.Uninjured ~ ., data = Aviations_train, method="class")
summary(fit12)
(predicted= predict(fit12,Aviations_test, type="class"))
fancyRpartPlot(fit12)

fit12_pred <- predict(fit12, Aviations_test, type = "class")
fit12_pred

confusionMatrix(Aviations_test$Total.Uninjured, fit12_pred)
#accuracy= 94.31%


fit13 <- rpart(Aviations_train$Weather.Condition ~ ., data = Aviations_train, method="class")
summary(fit13)
(predicted= predict(fit13,Aviations_test, type="class"))
fancyRpartPlot(fit13)

fit13_pred <- predict(fit13, Aviations_test, type = "class")
fit13_pred

confusionMatrix(Aviations_test$Weather.Condition, fit13_pred)
#accuracy= 92.83%


fit14 <- rpart(Aviations_train$Broad.Phase.of.Flight ~ ., data = Aviations_train, method="class")
summary(fit14)
(predicted= predict(fit14,Aviations_test, type="class"))
fancyRpartPlot(fit14)

fit14_pred <- predict(fit14, Aviations_test, type = "class")
fit14_pred

confusionMatrix(Aviations_test$Broad.Phase.of.Flight, fit14_pred)
#accuracy= 36.85%


fit15 <- rpart(Aviations_train$Month ~ ., data = Aviations_train, method="class")
summary(fit15)
(predicted= predict(fit15,Aviations_test, type="class"))
fancyRpartPlot(fit15)

fit15_pred <- predict(fit15, Aviations_test, type = "class")
fit15_pred

confusionMatrix(Aviations_test$Month, fit15_pred)
#accuracy= 12.15%

fit15a <- rpart(Aviations_train$Amateur.Built~ ., data = Aviations_train, method="class")
summary(fit15)
(predicted= predict(fit15a,Aviations_test, type="class"))
fancyRpartPlot(fit15)

fit15a_pred <- predict(fit15a, Aviations_test, type = "class")
fit15a_pred

confusionMatrix(Aviations_test$Amateur.Built, fit15a_pred)
#accuracy= 89.8%

##decision tree for presentation
## Association Rule #1 {Weather.condition = Instructments, Broad.Phase.of.Flight=Maneuvering}=>{Injury.Severity=Fatal}
#use fit16 for presentation
fit16 <- rpart(Injury.Severity ~ Weather.Condition + Broad.Phase.of.Flight,
               data=Aviations_train,
               method="class", 
               control=rpart.control(minsplit=1, cp=0))
summary(fit16)
#(predicted= predict(fit16,Aviations_test, type="class"))
fancyRpartPlot(fit16)

# prediction models is the decision tree
fit16_pred <- predict(fit16, Aviations_test, type = "class")
fit16_pred

confusionMatrix(Aviations_test$Injury.Severity, fit16_pred)
#Accuracy 80.5%


## Association Rule #12 {Weather.condition = Instructments, Broad.Phase.of.Flight=Maneuvering}=>{Injury.Severity=Fatal}
fit17 <- rpart(Injury.Severity ~ Amateur.Built + Weather.Condition + Broad.Phase.of.Flight,
               data=Aviations_train,
               method="class", 
               control=rpart.control(minsplit=1, cp=0))
summary(fit17)
#(predicted= predict(fit17,Aviations_test, type="class"))
fancyRpartPlot(fit17)

# prediction models is the decision tree
fit17_pred <- predict(fit17, Aviations_test, type = "class")
fit17_pred


confusionMatrix(Aviations_test$Injury.Severity, fit17_pred)
#Accuracy 80.8%

# Injury.Severity
fit18 <- rpart(Injury.Severity ~.,
               data=Aviations_train,
               method="class", 
               control=rpart.control(minsplit=1, cp=0.002))
summary(fit18)
#(predicted= predict(fit18,Aviations_test, type="class"))
fancyRpartPlot(fit18)

fit18_pred <- predict(fit18, Aviations_test, type = "class")
fit18_pred


confusionMatrix(Aviations_test$Injury.Severity, fit18_pred)
#Accuracy 99.5%

#What factors caused an accident-based on any factors above 95% in accuracy
#use fit19 for presentation
fit19 <- rpart(Injury.Severity ~ Total.Fatal.Injuries + Schedule +  Aircraft.Category + FAR.Description,
               data=Aviations_train,
               method="class", 
               control=rpart.control(minsplit=2, cp=0.0001))
summary(fit19)
#(predicted= predict(fit19,Aviations_test, type="class"))
fancyRpartPlot(fit19)

fit19_pred <- predict(fit19, Aviations_test, type = "class")
fit19_pred

confusionMatrix(Aviations_test$Injury.Severity, fit19_pred)
#Accuracy 97.19%



###########################################################################
## 4. SUPPORT VECTOR MACHINES
###########################################################################

# 1. Review the data and attributes; eliminate variables as needed.
master.df <- original.df
head(master.df)
str(master.df)

#REMOVING DUPS ON EVENT ID

duplicated(master.df$Event.Id)
x <- duplicated(master.df$Event.Id)

y <- x[duplicated(master.df$Event.Id)]

z <- data.frame(table(master.df$Event.Id))

z[z$Freq > 1,]

#master.df2 <- master.df[which(master.df$Event.Id %in% y),]

master.df2 <- master.df[!master.df$Event.Id %in% z$Var1[z$Freq > 1],]

str(master.df2)


# dput(colnames(master.df))
# 
# master.df.cleans = c("Event.Id", "Investigation.Type", "Accident.Number", "Event.Date", 
#                      "Location", "Country", "Latitude", "Longitude", "Airport.Code", 
#                      "Airport.Name", "Injury.Severity", "Aircraft.Damage", "Aircraft.Category", 
#                      "Registration.Number", "Make", "Model", "Amateur.Built", "Number.of.Engines", 
#                      "Engine.Type", "FAR.Description", "Schedule", "Purpose.of.Flight", 
#                      "Air.Carrier", "Total.Fatal.Injuries", "Total.Serious.Injuries", 
#                      "Total.Minor.Injuries", "Total.Uninjured", "Weather.Condition", 
#                      "Broad.Phase.of.Flight", "Report.Status", "Publication.Date", 
#                      "X")
# 
# for(icol in master.df.cleans){
#   
#   print(icol)
# }

# 2. Review and fix attribute types
str(master.df)


# 3. Handle missing values

#Check NAs
master.df$Total.Fatal.Injuries
any(is.na(master.df[]))
sum(is.na(master.df[]))

#Total.Fatal.Injuries-convert NAs to zero
sum(is.na(master.df$Total.Fatal.Injuries))
master.df$Total.Fatal.Injuries[is.na(master.df$Total.Fatal.Injuries)]<-0
master.df$Total.Fatal.Injuries
sum(is.na(master.df$Total.Fatal.Injuries))
str(master.df)

#Total.Serious.Injuries-convert NAs to zero
sum(is.na(master.df$Total.Serious.Injuries))
master.df$Total.Serious.Injuries[is.na(master.df$Total.Serious.Injuries)]<-0
master.df$Total.Serious.Injuries
sum(is.na(master.df$Total.Serious.Injuries))
str(master.df)


#Total.Minor.Injuries -convert NAs to zero
sum(is.na(master.df$Total.Minor.Injuries ))
master.df$Total.Minor.Injuries [is.na(master.df$Total.Minor.Injuries )]<-0
master.df$Total.Minor.Injuries 
sum(is.na(master.df$Total.Minor.Injuries ))
str(master.df)

#Total.Minor.Uninjured -convert NAs to zero
# sum(is.na(master.df$ Total.Uninjured  ))
# master.df$ Total.Uninjured  [is.na(master.df$ Total.Uninjured  )]<-0
# master.df$ Total.Uninjured  
# sum(is.na(master.df$ Total.Uninjured  ))
# str(master.df)

#Number.of.Engines - convert NAs to zero
sum(is.na(master.df$Number.of.Engines))
master.df$Number.of.Engines[is.na(master.df$Number.of.Engines  )]<-0
master.df$Number.of.Engines  
sum(is.na(master.df$Number.of.Engines))
str(master.df)

# 4. Inspect for invalid values or values in bad format
# reviewing a table of values for every variable
# for(i in 1:ncol(master.df)){
#  print(table(master.df[i]))
# }

## SVM
# setRepositories()
#ap <- available.packages()

str(master.df)
dim(master.df)

# Data has white spaces

#master.df3 <- master.df[, c(2,11, 15, 16, 17, 27, 28)]

#master.df3 <- master.df3[-which(master.df$Injury.Severity == " Incident "), ]

# Only want accidents and certain columns for prediction
# Columns for SVM

# Investigation Type - Only want Accident and not incident
# Injury Severity 
# Make 
# Model - this was removed as this was too much noise
# Amateur Built 
# Number of Engines 
# Engine Type
# FAR Description - removed as this serves no purpose
# Purpose of Flight 
# Weather Condition 
# Broad Phase of Flight - Added in


# master.df_VO <- master.df[-which(master.df$Investigation.Type==" Incident "), c(11, 17, 18, 19, 22, 28, 29)]
# Excluding amateur built... it was giving problem
master.df_VO <- master.df[-which(master.df$Investigation.Type==" Incident "), c(11, 18, 19, 22, 28, 29)]
# master.df_VO <- master.df[-which(master.df$Investigation.Type==" Incident "), c(11, 19, 28)]

# Only want Aerial, Public and Personal

master.df_VO[which(master.df_VO$Purpose.of.Flight==' Public Aircraft - Federal '),] <- ' Public Aircraft '
master.df_VO[which(master.df_VO$Purpose.of.Flight==' Public Aircraft - State '),] <- 'Public Aircraft '

master.df_VO <- filter(master.df_VO, Purpose.of.Flight == ' Aerial Application ' | Purpose.of.Flight == ' Aerial Observation ' | Purpose.of.Flight == ' Personal ' | Purpose.of.Flight == ' Public Aircraft ' | Purpose.of.Flight == ' Public Aircraft - Federal ' | Purpose.of.Flight == ' Public Aircraft - State ' | Purpose.of.Flight == ' Public Aircraft - Local ')

# Quick view
str(master.df_VO)

#Dropped unused levels

master.df_VO$Purpose.of.Flight <- factor(master.df_VO$Purpose.of.Flight)

# Quick view
str(master.df_VO)
unique(master.df_VO$Purpose.of.Flight)

master.df_VO <- filter(master.df_VO, Purpose.of.Flight == ' Aerial Application ' | Purpose.of.Flight == ' Aerial Observation ' | Purpose.of.Flight == ' Personal ')

#Dropped unused levels

master.df_VO$Purpose.of.Flight <- factor(master.df_VO$Purpose.of.Flight)

# str(master.df_VO)

# https://stringr.tidyverse.org/reference/str_replace.html

#master.df3$Injury.Severity == " Fatal. "

# Binning all "Fatals" together

master.df_VO$Injury.Severity <- str_replace_all(master.df_VO$Injury.Severity, "[()]", "")
master.df_VO$Injury.Severity <- str_replace_all(master.df_VO$Injury.Severity, "[0123456789]", "")
master.df_VO$Injury.Severity <- str_replace_all(master.df_VO$Injury.Severity, "[,]", "")
master.df_VO

master.df_VO2 <- master.df_VO

str(master.df_VO2)

# Amateur.Built column was taken out, but if needed run code below

# Need Amateur.Built into numeric instead of Yes and No

# master.df_VO2[master.df_VO2==" "] <- NA
# 
# master.df_VO2 <- na.omit(master.df_VO2)

# master.df_VO2 <- master.df_VO2[!apply(is.na(master.df_VO2) | master.df_VO2 == "", 1, all),]
# 
# master.df_VO$Amateur.Built <- factor(master.df_VO$Amateur.Built)

# master.df_VO2$Amateur.Built <- ifelse(master.df_VO2$Amateur.Built==' Yes ', 1,0)
# 
# master.df_VO2$Amateur.Built 


# master.df_VO2$Amateur.Built <-  as.numeric(master.df_VO2$Amateur.Built) 
# master.df_VO2$Number.of.Engines <-  as.numeric(master.df_VO2$Number.of.Engines) 

str(master.df_VO2)
unique(master.df_VO$Purpose.of.Flight)

master.df_VO2[master.df_VO2$Purpose.of.Flight==" Aerial Application "] <- "1"
master.df_VO2[master.df_VO2$Purpose.of.Flight==" Aerial Observation "] <- "2"
master.df_VO2[master.df_VO2$Purpose.of.Flight==" Personal "] <- "3"
master.df_VO2[master.df_VO2$Purpose.of.Flight==" Public Aircraft "] <- "4"
master.df_VO2[master.df_VO2$Purpose.of.Flight==" Public Aircraft - Federal "] <- "5"
master.df_VO2[master.df_VO2$Purpose.of.Flight==" Public Aircraft - Local "] <- "6"
master.df_VO2[master.df_VO2$Purpose.of.Flight==" Public Aircraft - State "] <- "7"


master.df_VO2$Purpose.of.Flight <-  as.numeric(master.df_VO2$Purpose.of.Flight) 

# str(master.df_VO2)

# This part is Transforming nominal to numeric

# master.df_VO2$Amateur.Built <- ifelse(master.df_VO2$Amateur.Built==' Yes ', 2,1)

if(master.df_VO2$Weather.Condition==' IMC '){
  master.df_VO2[master.df_VO2$Weather.Condition==' IMC '] <- "1"
} else {
  if(master.df_VO2$Weather.Condition==' VMC '){
    master.df_VO2[master.df_VO2$Weather.Condition==' VMC '] <- "2"
  } else {
    master.df_VO2[master.df_VO2$Weather.Condition] <- "3"
  }
}

master.df_VO2$Weather.Condition <-  as.numeric(master.df_VO2$Weather.Condition) 

# str(master.df_VO2)

if(master.df_VO2$Broad.Phase.of.Flight==' APPROACH '){
  master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' APPROACH '] <- "1"
} else {
  if(master.df_VO2$Broad.Phase.of.Flight==' CLIMB '){
    master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' CLIMB '] <- "2"
  } else {
    if(master.df_VO2$Broad.Phase.of.Flight==' CRUISE '){
      master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' CRUISE '] <- "3"
    } else {
      if(master.df_VO2$Broad.Phase.of.Flight==' DESCENT '){
        master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' DESCENT '] <- "4"
      } else {
        if(master.df_VO2$Broad.Phase.of.Flight==' GO-AROUND '){
          master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' GO-AROUND '] <- "5"
        } else {
          if(master.df_VO2$Broad.Phase.of.Flight==' LANDING '){
            master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' LANDING '] <- "6"
          } else {
            if(master.df_VO2$Broad.Phase.of.Flight==' MANEUVERING '){
              master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' MANEUVERING '] <- "7"
            } else {
              if(master.df_VO2$Broad.Phase.of.Flight==' OTHER '){
                master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' OTHER '] <- "8"
              } else {
                if(master.df_VO2$Broad.Phase.of.Flight==' STANDING '){
                  master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' STANDING '] <- "9"
                } else {
                  if(master.df_VO2$Broad.Phase.of.Flight==' TAKEOFF '){
                    master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' TAKEOFF '] <- "10"
                  } else {
                    if(master.df_VO2$Broad.Phase.of.Flight==' TAXI '){
                      master.df_VO2[master.df_VO2$Broad.Phase.of.Flight==' TAXI '] <- "11"    
                    } else {
                      master.df_VO2[master.df_VO2$Broad.Phase.of.Flight] <- "12"
                    }
                    
                  }}}}}}}}}}

master.df_VO2$Broad.Phase.of.Flight <-  as.numeric(master.df_VO2$Broad.Phase.of.Flight) 

# str(master.df_VO2)

if(master.df_VO2$Engine.Type==' Electric '){
  master.df_VO2[master.df_VO2$Engine.Type==' Electric '] <- "1"
} else {
  if(master.df_VO2$Engine.Type==' REC, ELEC '){
    master.df_VO2[master.df_VO2$Engine.Type==' REC, ELEC '] <- "2"
  } else {
    if(master.df_VO2$Engine.Type==' REC, TJ, TJ '){
      master.df_VO2[master.df_VO2$Engine.Type==' REC, TJ, TJ '] <- "3"
    } else {
      if(master.df_VO2$Engine.Type==' Reciprocating '){
        master.df_VO2[master.df_VO2$Engine.Type==' Reciprocating '] <- "4"
      } else {
        if(master.df_VO2$Engine.Type==' Turbo Fan '){
          master.df_VO2[master.df_VO2$Engine.Type==' Turbo Fan '] <- "5"
        } else {
          if(master.df_VO2$Engine.Type==' Turbo Jet '){
            master.df_VO2[master.df_VO2$Engine.Type==' Turbo Jet '] <- "6"
          } else {
            if(master.df_VO2$Engine.Type==' Turbo Prop '){
              master.df_VO2[master.df_VO2$Engine.Type==' Turbo Prop '] <- "7"
            } else {
              if(master.df_VO2$Engine.Type==' Turbo Shaft '){
                master.df_VO2[master.df_VO2$Engine.Type==' Turbo Shaft '] <- "8"
              } else {
                master.df_VO2[master.df_VO2$Engine.Type] <- "9"
              }
              
            }}}}}}}

master.df_VO2$Engine.Type <-  as.numeric(master.df_VO2$Engine.Type) 
master.df_VO2$Number.of.Engines <-  as.numeric(master.df_VO2$Number.of.Engines) 
master.df_VO2$Injury.Severity <-  as.factor(master.df_VO2$Injury.Severity) 

master.df_VO3 <- filter(master.df_VO2, Injury.Severity == ' Fatal ' | Injury.Severity == ' Non-Fatal ')

master.df_VO3$Injury.Severity <- as.factor(master.df_VO3$Injury.Severity)

# str(master.df_VO3)
# unique(master.df_VO3$Injury.Severity)


master.df_VO3$Injury.Severity <- factor(master.df_VO3$Injury.Severity)

# SVM Setup

# Cutting the rows to save run time
# Setting up Training and Testing data

master.df_VO4 <- (master.df_VO3[1:2000,])

randIndex <- sample(1:dim(master.df_VO4)[1])
summary(randIndex)

length(randIndex)

head(randIndex)

cutPoint2_3 <- floor(2 * dim(master.df_VO4)[1]/3)
cutPoint2_3

trainData <- master.df_VO4[randIndex[1:cutPoint2_3],]
testData <- master.df_VO4[randIndex[(cutPoint2_3 + 1):dim(master.df_VO4)[1]],]

str(trainData)
str(testData)

# view(trainData)
# view(testData)

str(master.df_VO4)
anyNA(master.df_VO4)



# SVM using Caret 

table(master.df_VO4$Injury.Severity)
str(master.df_VO4)

set.seed(100)

# Set trainControl

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     sampling = 'down')

# Cost
grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5))

# Train and Tune the SVM

# master.df_VO4[master.df_VO4$Injury.Severity==" Non-Fatal "] <- "NonFatal"
# master.df_VO4[master.df_VO4$Injury.Severity==" Fatal "] <- "Fatal"

# Updating values so there are no spaces and special sign
master.df_VO4$Injury.Severity <- ifelse(!is.na(master.df_VO4$Injury.Severity) & master.df_VO4$Injury.Severity == " Non-Fatal ", "NonFatal", "Fatal")

trainData$Injury.Severity <- ifelse(!is.na(trainData$Injury.Severity) & trainData$Injury.Severity == " Non-Fatal ", "NonFatal", "Fatal")
testData$Injury.Severity <- ifelse(!is.na(testData$Injury.Severity) & testData$Injury.Severity == " Non-Fatal ", "NonFatal", "Fatal")

trainData$Injury.Severity <- factor(trainData$Injury.Severity)
testData$Injury.Severity <- factor(testData$Injury.Severity)

# SVM Linear Model

svmLin_mod <- train(Injury.Severity ~., data = trainData,
                    method = "svmLinear",
                    preProc = c("center", "scale"),
                    metric = "ROC",
                    tuneGrid = grid,
                    trControl = ctrl)

svmLin_mod

# SVM Radial Kernel

grid <- expand.grid(sigma = c(.01, .015, 0.2), 
                    C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5))

svmRad_mod <- train(Injury.Severity ~., data = trainData,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    metric = "ROC",
                    tuneGrid = grid,
                    trControl = ctrl)
svmRad_mod

# SVM Polynomial Kernel

grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                    degree =c(1, 2, 3),
                    scale=c(.001, 0.01, 0.1))

# str(master.df_VO4)

svmPoly_mod <- train(Injury.Severity ~., data = trainData,
                     method = "svmPoly",
                     preProc = c("center", "scale"),
                     metric = "ROC",
                     tuneLength = 3,
                     trControl = ctrl)
svmPoly_mod

# Resamples

comparisons <- resamples(list(linear=svmLin_mod,
                              radial=svmRad_mod,
                              poly=svmPoly_mod))

summary(comparisons)
# comparisons$values

# Prediction

# str(testData)

pred <- predict(svmRad_mod, testData)

# table(testData$Injury.Severity)

# data("segmentationData", package = "caret")
# table(segmentationData$Class)

# *** *** *** *** *** *** *** ***


###########################################################################
## PART 2. EXPLORATION AND VSIUALIZATION
###########################################################################
# Visualizations based on aircraft damage

ggMeltInjuries <- melt(data=master.df, 
                       measure.vars=c("Total.Fatal.Injuries","Total.Serious.Injuries","Total.Minor.Injuries", "Total.Uninjured"))
ggplot(data=ggMeltInjuries, aes(x=Aircraft.Damage)) + geom_bar(aes(y=value, fill=variable), stat='identity') + 
  ggtitle("Type of Injury based on Aircraft Damage") + ylab('Incident count')

#based on Number.of.Engines
ggplot(data=ggMeltInjuries, aes(x=Number.of.Engines)) + geom_bar(aes(y=value, fill=variable), stat='identity') + 
  ggtitle("Type of Injury based on Number of Engines") +ylab('Incident count')

#based on Broad.Phase.of.Flight
ggplot(data=ggMeltInjuries, aes(x=Broad.Phase.of.Flight)) + geom_bar(aes(y=value, fill=variable), stat='identity') + 
  ggtitle("Type of Injury based on Phase of Flight") + ylab('Incident count')+ 
  theme(axis.text.x = element_text(angle = 90))

#based on day of week
ggplot(data=ggMeltInjuries, aes(x=WeekDay)) + geom_bar(aes(y=value, fill=variable), stat='identity') + 
  ggtitle("Type of Injury based on Day of Week") + 
  ylab('Incident count')+ theme(axis.text.x = element_text(angle = 90))


###########################################################################
# Visualizing on US map 
# define function to convert state abbreviations to state name
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}

# prepare data for mapping
map.df<-master.df[,c("Longitude","Latitude","State.abb","City","Injury.Severity","Aircraft.Damage","Aircraft.Category","Number.of.Engines",
                     "Purpose.of.Flight","Total.Fatal.Injuries","Weather.Condition","Broad.Phase.of.Flight")]
# note: less than 30K rows have lon/lat info. so we're only using a third of original dataset for mapping.
map.df<-map.df[-which(is.na(map.df$Longitude)),]
map.df<-map.df[-which(is.na(map.df$Latitude)),]
sum(is.na(map.df))
map.df$region<-tolower(abb2state(map.df$State.abb))  # new state solumn named 'region' has full state names 
# need to provide a filler for color per state. fill out avg weather by state. 
# first create a numeric value for the weather by state. 
# Instrument=1, Visual=2. if avg closer to 2 there were more visual conditions in the accidents that occurred in the state.
my.summary <- aggregate(as.numeric(map.df$Weather.Condition), by = list(map.df$region), FUN = mean )
colnames(my.summary) <- c('region', 'avg.weather')
str(my.summary)  # merge the list of averages by state with the big dataframe will populate new agv.weather based on state
map.df <- merge(map.df, my.summary, by='region', replace=TRUE) 
str(map.df)

# actual mapping
us<-ggplot2::map_data("state")
av.map<-ggplot(map.df, aes(map_id=region))+coord_map()
av.map<-av.map+geom_map(map=us,aes(fill=avg.weather), color="white")
#av.map<-av.map+expand_limits(x=map.df$Longitude,y=map.df$Latitude)
av.map<-av.map+xlim(-125,-67) + ylim(25,50)
av.map<- av.map + geom_point(aes(x=map.df$Longitude,y=map.df$Latitude,
                                 shape=".",size=0.5,
                                 color=map.df$Total.Fatal.Injuries))+ 
  scale_color_gradient(low="gold",high="red",guide=guide_legend(title="% Fatalities"))+
  scale_size_identity()  # overrides automatic sizing, important to keep!!
av.map<- av.map + guides(fill=guide_legend(title="State Avg Weather"))
av.map<-av.map+ labs(title = "US Map of Aviation Accident Fatalities",
                     subtitle = "Event weather state avg is from instrument (cloud/fog) to visual (clear) conditions",
                     caption = "Source: NTSB.gov, 1982-2019", 
                     x = "Longitude", y = "Latitude") 
av.map



#### End of Project AVIATION #####


