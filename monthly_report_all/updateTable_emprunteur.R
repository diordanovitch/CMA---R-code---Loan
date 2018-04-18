lastupdateperiod <-lp
lastupdateperiod = substr(lastupdateperiod,5,6)


Year <-year 
Week <-weekormonth
  

  

source("2Libraries_emprunteur.R")
source("3function for reporting_Generic_emprunteur.R")
source("4Directories_emprunteur.R")




moiactu=strftime(as.Date(Sys.Date(),format="%Y-%m-%d"),format="Y%yM%m")   
moiactu_1=pm(moiactu) 



weekactu=getWeekActu(Sys.Date(),3)


source(file = "Data_process_Emprunteur.R")



save(New_Table, file = "Crawling data/Assurland_Loan_prices_November_NEW_TABLE.RData")





## Now we merge the database with the previous crawling (if we want to import old datasets -> source(6.addtopreviousdataset.R)).

crawling_old <- read.csv(file="Crawling data/Assurland_Loan_prices_June.csv", header=TRUE, sep=";")

crawling_old <- crawling_old[!crawling_old$period %in% unique(as.character(New_Table$period)),!colnames(crawling_old) %in% c("type","id","formula","profilid")]


crawling_all <- rbind(crawling_old,New_Table[,colnames(crawling_old)])
crawling_all <- crawling_all[!duplicated(crawling_all[,c("profilID","period","yearmonth","insurer","coverage" )]),]






if(Report == "Assurland_Report"){
  crawling_all$Segment <- "Global"
  save(crawling_all, file = "Crawling data/Assurland_Loan_prices_MERGED.RData")
  
}else if(Report == "LeLynx_Report"){
  crawling_all$period=ifelse(crawling_all$period=="Y16W02_NewMRP", "Y16W02",crawling_all$period)
  save(crawling_all, file = "./output_MR_all/LeLynx_Report/data_LeLynx.RData")
}else if(Report == "Italy_Report"){
  save(crawling_all, file = "./output_MR_all/Italy_Report/data_Italy.RData")
}else if(Report == "Spain_Report"){
  save(crawling_all, file = "./output_MR_all/Spain_Report/data_Spain.RData")
}else if(Report == "Japan_Report"){
  save(crawling_all, file = "./output_MR_all/Japan_Report/data_Japan.RData")
}
