Source = "MONTHLY"


lp = "Y17W25"     # lastperiod, from where we want to update
weekormonth = 20
year = 2017

Report = "Assurland_Loan"

crawling_file = "Crawling data/Assurland_Loan_prices_November.csv"


source("updateTable_emprunteur.R")



if(Source == "MONTHLY"){ 
  source("./monthly_report_all/11Cumul_Evolution_emprunteur.R")
}else{
  source("./monthly_report_all/12.Cumul_Evolution_weekly.R")
}


