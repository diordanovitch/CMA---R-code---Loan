### Assurland - Loan ###


## Parameters setting

Report = "Assurland_Loan"

source("./monthly_report_all/2Libraries_emprunteur.R")
source("./monthly_report_all/3function for reporting_Generic_emprunteur.R")
source("./monthly_report_all/registerfunctions.R")
source("./monthly_report_all/players_setting.R")



moiactu=strftime(as.Date(Sys.Date(),format="%Y-%m-%d"),format="Y%yM%m")   
moiactu_1=pm(moiactu) 
a = moiactu

l = c()

for(i in 1:13)
{
  tmp = pm(a)
  l = c(l,tmp)
  a = tmp
}



insert_base = T # ?



## We load the data, and we transform it.

load("./output_MR_all/Assurland_Loan/New_Table_Assurland_emp.RData") # New crawling
load("./output_MR_all/Assurland_Loan/data_Assurland_emp.RData") # Merged crawling
New_Table$type = "UNKNOWN"
New_Table[New_Table$insurer%in%CLASSIQUEPlayers,]$type = "CLASSIQUE"
New_Table[New_Table$insurer%in%ALTERNATIFSPlayers,]$type = "ALTERNATIFS"
New_Table[New_Table$insurer%in%BANCASSUREURPlayers,]$type = "BANCASSUREUR"
New_Table[New_Table$insurer%in%MUTUELLEPlayers,]$type = "MUTUELLE"




frdatatemp <- New_Table

tab=data.frame(period=frdatatemp$period, yearmonth=frdatatemp$yearmonth, profilID=frdatatemp$profilID,
               insurer=frdatatemp$insurer,coverage=frdatatemp$coverage,type=frdatatemp$type, 
               date_aspiration = frdatatemp$date_aspiration,priceYCfees=frdatatemp$priceYCfees)


## We store clean price table. 

save(tab, file = "./output_MR_all/Assurland_Loan/tab_emp.RData")
store_cleanprice(tab)




## Now we create and store another clean price table, with the mean of the price for same ProfilID.

meanna=function(x) {
  return(mean(x,na.rm=T))
}
if(is.data.frame(frdatatemp) && nrow(frdatatemp)==0) {
  print("yes!")
  tab2=tab
}else{
  tab2=aggregate(frdatatemp$priceYCfees ~ frdatatemp$yearmonth + frdatatemp$profilID + frdatatemp$insurer + frdatatemp$coverage+ frdatatemp$type ,FUN=meanna)
  
  names(tab2)=c("yearmonth","profilID","insurer","coverage","type","priceYCfees")
}

save(tab2, file = "./output_MR_all/Assurland_Loan/tab2_emp.RData")





## We load the log variation file, and we store it as cumulated evolution DB.


load("./output_MR_all/Assurland_Loan/logevolfinal2_emp.RData")

colnames(logevolfinal2)[5]<-"type"

oldcumul=unique(downloadCumulatedEvolution())
oldcumul=na.omit(oldcumul)

cumulEvolResult <- logevolfinal2#[as.character(logevolfinal2$period)>max(oldcumul$period) ,]
tab = data.frame(period=cumulEvolResult$period,insurer=cumulEvolResult$insurer,type=cumulEvolResult$type,coverage=cumulEvolResult$coverage,mean=cumulEvolResult$mean)
tab = unique(tab)


StorecumulatedEvolution(tab)





## We load the summary tab file, and we store it as avg premium.


load("./output_MR_all/Assurland_Loan/summaryTab_emp.RData")

stfinal <- summaryTab[as.character(summaryTab$period)>max(oldcumul$period) ,]


tab = data.frame(period=stfinal$period,insurer=stfinal$insurer,avg=stfinal$AvgPremium,type=stfinal$PlayerType,formula=stfinal$coverage)
tab = unique(tab)


Storeavepremium(tab)





## We load the summary tab file, and we store it as display prop.


tab = data.frame(period=stfinal$period,insurer=stfinal$insurer,display_prop=stfinal$Display_prop,type=stfinal$PlayerType,formula=stfinal$coverage)
tab = unique(tab)

for (a in unique(tab$type)){
  print(a)
  print(unique(tab[tab$type == a,]$insurer))
  for (i in unique(tab[tab$type == a,]$insurer)) {
    for (j in unique(tab[tab$type == a,]$period)) {
      for (k in unique(tab[tab$type == a,]$formula)) {
        if(as.numeric(nrow(tab[tab$type == a & tab$insurer == i & tab$period == j & tab$formula == k,]))==0)
        {
          tab = rbind(tab,data.frame(period=j,insurer=i,display_prop=0,type=a,formula=k))
        }
      }
    }
  }
}

StoredisplayEvolution(tab)







## We compute the pricegap table and we save it. 

load("./output_MR_all/Assurland_Loan/data_Assurland_emp.RData")
frdata1_new <- crawling_all #MyData#[as.character(data$period)>max(liste_period$period),]
frdata1_new=frdata1_new[!duplicated(frdata1_new[c("profilID","insurer","coverage","period","yearmonth","price")]),]



pricegap_predata = data.frame(period = NaN, coverage = NaN, x = NaN, y = NaN)


for(p in sort(unique(frdata1_new$yearmonth)))
{
  lastmonth=frdata1_new[frdata1_new$yearmonth==p & frdata1_new$insurer%in%All,]
  lweekes=aggregate(lastmonth$price ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
  names(lweekes)=c("profilID","insurer","coverage","price")
  
  lweekeskminmarket=lweekes[!grepl("Groupe AXA",lweekes$insurer),]
  
  lweekeskminmarket=lweekeskminmarket[order(lweekeskminmarket$price),]
  
  lweekeskminmarket=lweekeskminmarket[!duplicated(lweekeskminmarket[c("profilID","coverage")]),]
  
  three=lweekes[grepl("Groupe AXA", lweekes$insurer),]
  
  deltamineslast=merge(three,lweekeskminmarket,by=c("profilID","coverage"))
  
  
  deltamineslast$delta=(deltamineslast$price.x/deltamineslast$price.y)-1
  
  for(c in sort(unique(deltamineslast$coverage)))
  {
    deltaminesmars=deltamineslast[deltamineslast$coverage == c,] # 'Formule Optimum' -> c
     
    a=density(deltaminesmars$delta,bw = "sj")
    res=data.frame(x=a$x,y=a$y)
    
    pricegap_predata_1 = data.frame(period = p, coverage = c, x = res$x,y = res$y)
    pricegap_predata = rbind(pricegap_predata, pricegap_predata_1)
    
    #if(insert_base)
    #{  
     # drv <- dbDriver("PostgreSQL")
      #con <- dbConnect(drv, dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
      #dataW= dbWriteTable(con,c("report_france","PriceGap_monthly_assurland"), pricegap_predata,row.names=FALSE,append = TRUE)
      ###
      #lapply(dbListConnections(drv), FUN = dbDisconnect)
      
      ## Frees all the resources on the driver
      #dbUnloadDriver(drv)
    #}
  }
}
pricegap_predata <- pricegap_predata[-1,]

save(pricegap_predata,file="./output_MR_all/Assurland_Loan/pricegap_emp.RData")










## We compute the market intensity table and we save it. 


frdatatemp=data.frame(frdata1_new$yearmonth,frdata1_new$profilID,frdata1_new$insurer,frdata1_new$coverage,frdata1_new$price,frdata1_new$Segment)
names(frdatatemp)=c("period","profilID","insurer","coverage","price","Segment")

frdatatemp_classique= frdatatemp[frdatatemp$insurer%in%CLASSIQUEPlayers,]

marketdispersion_predata = data.frame(period=NaN,coverage=NaN,x=NaN,y=NaN)


if(is.data.frame(frdatatemp_classique) && nrow(frdatatemp_classique)==0) {
  print("yes!")
}else {
  top3_classique=top_propor_Generic(frdatatemp_classique,top = 3)
  top3_classique <- subset(top3_classique, select = - c(Segment)) # Remove Segment column. 
}

  
for(p in sort(unique(top3_classique$period)))
{
  lastmonth=top3_classique[top3_classique$period==p,]
  lweekes=aggregate(lastmonth$price ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
  names(lweekes)=c("profilID","insurer","coverage","price")
  lweekes=unique(lweekes)
    
  for(cov in sort(unique(top3_classique$coverage)))
  {
    if(nrow(lweekes[lweekes$coverage==cov,]) != 0){
    # Now ce compute the coefficient of variation (= sd/mean), which is a standardized measure of dispersion.
    dispesRCmar=aggregate(lweekes$price[lweekes$coverage==cov] ~ lweekes$profilID[lweekes$coverage==cov], FUN= sd)[[2]]/
      aggregate(lweekes$price[lweekes$coverage==cov] ~ lweekes$profilID[lweekes$coverage==cov], FUN=mean)[[2]]
        
    dispesRCmar = na.omit(dispesRCmar)
    res = density(dispesRCmar,bw = "sj")
    res$x=abs(min(res$x))+res$x # We put a very small shift (<0.1) in the variation coefficient values to begin at 0.
    res= data.frame(res$x,res$y)
    colnames(res) = c("x","y")
        
    marketdispersion_predata_1 = data.frame(period=p,coverage=cov,x=res$x,y=res$y)
    marketdispersion_predata = rbind(marketdispersion_predata, marketdispersion_predata_1)
        #if(insert_base)
        #{  
         # drv <- dbDriver("PostgreSQL")
          #con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
          #dataW= dbWriteTable(con,c("report_france","MarketDisp_monthly_assurland"), marketdispersion_predata,row.names=FALSE,append = TRUE)
          ###
          #lapply(dbListConnections(drv), FUN = dbDisconnect)
          
          ## Frees all the resources on the driver
          #dbUnloadDriver(drv)
        #}
    }
  }
}

marketdispersion_predata <- marketdispersion_predata[-1,]


save(marketdispersion_predata,file="./output_MR_all/Assurland_Loan/marketint_emp.RData")



# plot(marketdispersion_predata$x, marketdispersion_predata$y)
# plot(pricegap_predata$x, pricegap_predata$y)














# ## ??????????
# 
# 
# ##Subset
# MyComplete$Seg_smoke[MyComplete$Seg_smoke=='oui'] <- 'Oui'
# Badprofiles <- deltaminesmars[deltaminesmars$delta >= 0.9900646,]
# Badfeatures <- subset(MyComplete, (profilID %in% Badprofiles$profilID))
# Badfeatures <- Badfeatures[!duplicated(Badfeatures$profilID), ]
# 
# Badfeatures$insurer <- NULL
# Badfeatures$coverage <- NULL
# Badfeatures$date_extraction <- NULL
# Badfeatures$period <- NULL
# Badfeatures$yearmonth <- NULL
# Badfeatures$Seg_location <- NULL
# Badfeatures$delta <- Badprofiles$delta
# 
# 
# ##the whole dataset 
# load("./output_MR_all/Assurland_emprunteur/Segment_table_all.RData")
# MyComplete$Seg_smoke[MyComplete$Seg_smoke=='oui'] <- 'Oui'
# Badprofiles <- deltaminesmars
# Badfeatures <- subset(MyComplete, (profilID %in% Badprofiles$profilID))
# Badfeatures <- Badfeatures[!duplicated(Badfeatures$profilID), ]
# 
# Badfeatures$insurer <- NULL
# Badfeatures$coverage <- NULL
# Badfeatures$date_extraction <- NULL
# Badfeatures$period <- NULL
# Badfeatures$yearmonth <- NULL
# Badfeatures$Seg_location <- NULL
# Badfeatures$delta <- Badprofiles$delta
# 
# 
# 
# library(rpart)
# 
# 
# frmla = delta ~ loan_amount + loan_duration + age  + Seg_CSP  + Seg_smoke 
# 
# # Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)
# 
# fit = rpart(frmla, method="class", data=Badfeatures)
# 
# 
# printcp(fit) # display the results
# plotcp(fit) # visualize cross-validation results
# summary(fit) # detailed summary of splits
# 
# # plot tree
# plot(fit, uniform=TRUE, main="Classification Tree for bad profiles")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)
# 
# # tabulate some of the data
# table(subset(raw, Koc>=190.5)$Metal)
# 
# ## EVTREE (Evoluationary Learning)
# install.packages('evtree')
# library(evtree)
# 
# 
# Badfeatures$loan_amount=round(Badfeatures$loan_amount/1000)
# ev.raw = evtree(frmla, data=Badfeatures, control=evtree.control(minbucket=1L, alpha = 1.1))
# plot(ev.raw)
# text(ev.raw, cex=0.5)
# table(predict(ev.raw), Badfeatures$delta)
# 1-mean(predict(ev.raw) == Badfeatures$delta)
# 
# ##quantile plot
# plot(quantile(Badfeatures$delta, probs = seq(0, 1, 0.025)), seq(0, 1, 0.025), xlab = 'quantile', ylab = '%')


