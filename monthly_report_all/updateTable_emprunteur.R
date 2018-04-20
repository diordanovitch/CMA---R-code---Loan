## We fix period parameters.


lastupdateperiod <-lp
lastupdateperiod = substr(lastupdateperiod,5,6)


Year <-year 
Week <-weekormonth


  


## We run the others R files  

source("./monthly_report_all/2Libraries_emprunteur.R")
source("./monthly_report_all/3function for reporting_Generic_emprunteur.R") 
source("./monthly_report_all/4Directories_emprunteur.R") # verify use
source("./monthly_report_all/7Parameters_emprunteur.R") #• new

source(file = "./monthly_report_all/Data_process_Emprunteur.R")



## We fix time parameters.

moiactu=strftime(as.Date(Sys.Date(),format="%Y-%m-%d"),format="Y%yM%m")   
moiactu_1=pm(moiactu) 


weekactu=getWeekActu(Sys.Date(),3)



## We save New_Table, which is the cleaning new crawling.

New_Table$Segment <- "Global"
save(New_Table, file = "./output_MR_all/Assurland_Loan/New_Table_Assurland_emp.RData")





ForceUpdate = T #If force update = T, periods already present are removed and replaced by new imported ones. ?????????????




## Now we merge the database with the previous crawling (if we want to import old datasets -> source(6.addtopreviousdataset.R)).

#crawling_old <- read.csv(file="Crawling data/Assurland_Loan_prices_June.csv", header=TRUE, sep=";")

crawling_old <- get(load(file="./output_MR_all/Assurland_Loan/data_Assurland_emp.RData"))


crawling_old <- crawling_old[!crawling_old$period %in% unique(as.character(New_Table$period)),!colnames(crawling_old) %in% c("type","id","formula","profilid")]


crawling_all <- rbind(crawling_old,New_Table[,colnames(crawling_old)])
crawling_all <- crawling_all[!duplicated(crawling_all[,c("profilID","period","yearmonth","insurer","coverage" )]),]






if(Report == "Assurland_Loan"){
  crawling_all$Segment <- "Global"
  save(crawling_all, file = "./output_MR_all/Assurland_Loan/data_Assurland_emp.RData")
  
  
  
## Following lines are normally usless for emprunteur process.  
  
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
