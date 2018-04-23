
### Ranking ###

data <- crawling_all


## Process already done normally.
# data$period <- paste( "Y",substr(data$year,3,4), "W",formatC(data$week,width=2, flag="0") , sep = "")
# data$yearmonth <- paste( "Y",substr(data$year,3,4), "M",formatC(data$month,width=2, flag="0") , sep = "")



## We filter in the seperiod scope.

frdatatemp=data[data$yearmonth%in%seperiod ,]




## Change structure of table - Reformating

frdatatemp$Segment <- 'Global'
frdatatemp$period<-NULL
frdatatemp$date_aspiration<-NULL
frdatatemp=data.frame(frdatatemp$profilID,frdatatemp$insurer,frdatatemp$coverage,frdatatemp$price,frdatatemp$yearmonth, frdatatemp$Segment)
names(frdatatemp)=c("profilID","insurer","coverage","prix","period","Segment")


## Compute the mean by month for the price for every profilID (in case there are several lines for one ProfilId).

frdatatemp=aggregate(frdatatemp$prix ~ frdatatemp$period+frdatatemp$profilID+frdatatemp$insurer+frdatatemp$coverage+frdatatemp$Segment, FUN=mean)
names(frdatatemp)=c("period","profilID","insurer","coverage","Segment","prix")


## We check that there are only known insurers in the DB.  

frdatatemp_classique= frdatatemp[frdatatemp$insurer%in% All,]




## We compute top 1 tables for each coverage, and one table with all coverages.


frdatatemp_classique_opti=frdatatemp_classique[frdatatemp_classique$coverage%in%c("Formule Optimum"),]

top1_opti=top_propor_Generic(frdatatemp_classique_opti,top = 1)


frdatatemp_other=frdatatemp_classique[frdatatemp_classique$coverage%in%c("Minimum"),]

top1_other=top_propor_Generic(frdatatemp_other,top = 1)


#table_classique=rbind(table_classique_opti, table_classique_other) # Should be the same as :
top1_classique = top_propor_Generic(frdatatemp_classique,top = 1)

save(top1_classique,file= ("./output_MR_all/Assurland_Loan/rank_top1_emp.RData")) 





## We compute top 3 tables for each coverage, and one table with all coverages.

frdatatemp_top3_opti=frdatatemp_classique[frdatatemp_classique$coverage%in%c("Formule Optimum"),]

top3_opti=top_propor_Generic(frdatatemp_top3_opti,top = 3)


frdatatemp_top3_other=frdatatemp_classique[frdatatemp_classique$coverage%in%c("Minimum"),]
top3_other=top_propor_Generic(frdatatemp_top3_other,top = 3)


#top3_classique=rbind(table_top3_opti, table_top3_other) # Should be the same as :
top3_classique = top_propor_Generic(frdatatemp_classique,top = 3)

top3_classique$proportion=top3_classique$cumsum/top3_classique$cumsum2
top3_classique$proportion=round(top3_classique$proportion*100)
top3_classique = top3_classique[,-c(8,9,10)]

save(top3_classique,file= ("./output_MR_all/Assurland_Loan/rank_top3_emp.RData")) 


# ## Compute ranking graphs : TO BE FIXED ##
# 
# ## Top 1
# 
# # All competitors
# top_Generic(top1_classique,covfr,coveragenames)
# 
# # Without alternatifs players
# top_Generic(top1_classique,covfr, coveragenames, exclude_insurer = ALTERNATIFSPlayers, TitleComplement = "Without Alternatifs Players")
# 
# 
# 
# ## Top 3
# 
# # All competitors
# top_Generic(top3_classique,covfr,coveragenames)
# 
# # Without alternatifs players
# top_Generic(top3_classique,covfr, coveragenames,  exclude_insurer = ALTERNATIFSPlayers, TitleComplement = "Without Alternatifs Players")
# 
# 
