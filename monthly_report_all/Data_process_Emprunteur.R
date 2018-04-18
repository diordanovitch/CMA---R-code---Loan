frdata <- read.csv(file=crawling_file, header=TRUE, sep=";")


## We remove useless columns and rearrange them. We delete also the rows with no insurer.And we change some column names.

frdata_actual = frdata[,c(1,2,4,5,6,7,14,15,16,8)]
frdata_actual = subset(frdata_actual, frdata_actual[,3]!= "NO_INSURER")
names(frdata_actual)[names(frdata_actual)=="prix"] <- "price"
names(frdata_actual)[names(frdata_actual)=="date_extraction"] <- "date_aspiration"



frdata_actual$fees[frdata_actual$fees=='Gratuits'| frdata_actual$fees=='Gratuits\n'] <- 0
frdata_actual$price <- as.numeric(as.vector(frdata_actual$price))
frdata_actual$fees <- as.numeric(as.vector(frdata_actual$fees))
frdata_actual$priceYCfees <- frdata_actual$price + frdata_actual$fees
frdata_actual <- frdata_actual[c(1,2,3,4,5,10,6,7,8,9,11)]




## Define period and yearmonth

frdata_actual$period <- paste( "Y",substr(frdata_actual$year,3,4), "W",formatC(frdata_actual$week,width=2, flag="0") , sep = "")
frdata_actual$yearmonth <- paste( "Y",substr(frdata_actual$year,3,4), "M",formatC(frdata_actual$month,width=2, flag="0") , sep = "")


#We remove useless columns

frdata_actual <- subset(frdata_actual, select = -c(year,month,week,campaignID,fees))

## We remove duplicates

frdata_actual=frdata_actual[!duplicated(frdata_actual[c("profilID","insurer","coverage","period","yearmonth","price")]),]





## We define scopes of covers and insurers. And filter the data on them.

## Covers

covfr = c("Formule Optimum","Minimum")


frdata_actual=frdata_actual[frdata_actual$coverage%in%covfr,]



## Insurers

MUTUELLEPlayers <- c("MAAF Assurances")

BANCASSUREURPlayers <- c("Cardif") 

CLASSIQUEPlayers <- c("Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA")

ALTERNATIFSPlayers <- c("AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")

All<-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")


frdata_actual <- frdata_actual[frdata_actual$insurer %in% All,]



## Final database.

New_Table <- frdata_actual





