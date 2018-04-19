data <- read.csv(file="C:/Documents/R.Marsal/Documents/Code R modif originale/output_MR_all/Assurland_emprunteur/Assurland_Loan_prices_November.csv", header=TRUE, sep=";")


frdata_actual_AL = data[,c(1,2,4,5,6,7,14,15,16,8)]

frdata_actual_AL = subset(frdata_actual_AL, frdata_actual_AL[,3]!= "NO_INSURER")

frdata_actual = frdata_actual_AL
frdata_actual$fees[frdata_actual$fees=='Gratuits'] <- 0
frdata_actual$prix <- as.numeric(frdata_actual$prix)
frdata_actual$fees <- as.numeric(frdata_actual$fees)
frdata_actual$priceYCfees <- frdata_actual$prix + frdata_actual$fees
#frdata_actual$fees <- NULL
frdata_actual <- frdata_actual[c(1,2,3,4,5,10,6,7,8,9,11)]


save(frdata_actual,file=paste0("./output_MR_all/Assurland_emprunteur/test_emp.RData"))
## Define period and yearmonth

frdata_actual$period <- paste( "Y",substr(frdata_actual$year,3,4), "W",formatC(frdata_actual$week,width=2, flag="0") , sep = "")
frdata_actual$yearmonth <- paste( "Y",substr(frdata_actual$year,3,4), "M",formatC(frdata_actual$month,width=2, flag="0") , sep = "")
# frdata_actual[frdata_actual$yearmonth == "Y16M01" & frdata_actual$period == "Y16W01","period"] <- "Y15W53"
# frdata_actual[frdata_actual$yearmonth == "Y16M01" & frdata_actual$period == "Y15W53","yearmonth"] <- "Y15M12"


#Correct periods if other campaign
#???MyData$period[grepl(paste(KeyWords, sep = "", collapse = "|"),MyData$campagneID)] = 
  #paste0(MyData$period[grepl(paste(KeyWords, sep = "", collapse = "|"),MyData$campagneID)],
   #      na.omit(unlist(apply(sapply(KeyWords, grepl, MyData$campagneID),MARGIN = 1,FUN  = function(x) names(x)[x][1]   ))))

#MyData$yearmonth[grepl(paste(KeyWords, sep = "", collapse = "|"),MyData$campagneID)] = 
 # paste0(MyData$yearmonth[grepl(paste(KeyWords, sep = "", collapse = "|"),MyData$campagneID)],
  #       na.omit(unlist(apply(sapply(KeyWords, grepl, MyData$campagneID),MARGIN = 1,FUN  = function(x) names(x)[x][1]   ))))

#Remove unuseful columns
frdata_actual <- subset(frdata_actual, select = -c(year,month,week,campaignID,fees))


save(frdata_actual,file = "./Example_clean_prices_all/Assurland_Report/data_assurland_emprunteur.Rdata")




## Define scopes of Insurers and covers

#Covers
covfr = c("Formule Optimum","Minimum")

frdata_actual=frdata_actual[!duplicated(frdata_actual[c("profilID","insurer","coverage","period","yearmonth","prix")]),]
#frdata_actual=frdata_actual[!duplicated(frdata_actual[c("profilID","insurer","coverage","period","yearmonth","prix", "priceYCfees")]),]
frdata_actual=frdata_actual[frdata_actual$coverage%in%covfr,]
names(frdata_actual)[names(frdata_actual)=="prix"] <- "price"
names(frdata_actual)[names(frdata_actual)=="date_extraction"] <- "date_aspiration"
#Scopes of insurer: Can change according to AL report or LL report
MUTUELLEPlayers <- c("MAAF Assurances")

BANCASSUREURPlayers <- c("Cardif") 

CLASSIQUEPlayers <- c("Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA")

ALTERNATIFSPlayers <- c("AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")

All<-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")


#Filter on the selected scope  
unique(frdata_actual$insurer)[!unique(frdata_actual$insurer)%in%c(MUTUELLEPlayers,BANCASSUREURPlayers,CLASSIQUEPlayers,ALTERNATIFSPlayers) ]
frdata_actual <- frdata_actual[frdata_actual$insurer %in% c(MUTUELLEPlayers,BANCASSUREURPlayers,CLASSIQUEPlayers,ALTERNATIFSPlayers),]

New_Table <- frdata_actual





