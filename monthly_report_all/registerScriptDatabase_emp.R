
## Parameters setting

source("./monthly_report_all/2Libraries_emprunteur.R")
source("./monthly_report_all/3function for reporting_Generic_emprunteur.R")
source("./monthly_report_all/France/registerfunctions.R")
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

#############
# ASSURLAND #
#############

insert_base = T

# Clean_Price_assurland

#############
# ASSURLAND #
#############


# Clean_Price_assurland


load("./output_MR_all/Assurland_Loan/New_Table_Assurland_emp.RData")
load("./output_MR_all/Assurland_Loan/data_Assurland_emp.RData")
New_Table$type = "UNKNOWN"
New_Table[New_Table$insurer%in%CLASSIQUEPlayers,]$type = "CLASSIQUE"
New_Table[New_Table$insurer%in%ALTERNATIFSPlayers,]$type = "ALTERNATIFS"
New_Table[New_Table$insurer%in%BANCASSUREURPlayers,]$type = "BANCASSUREUR"
New_Table[New_Table$insurer%in%MUTUELLEPlayers,]$type = "MUTUELLE"




frdatatemp <- New_Table

tab=data.frame(period=frdatatemp$period, yearmonth=frdatatemp$yearmonth, profilID=frdatatemp$profilID,
               insurer=frdatatemp$insurer,coverage=frdatatemp$coverage,type=frdatatemp$type, 
               date_aspiration = frdatatemp$date_aspiration,priceYCfees=frdatatemp$priceYCfees)

save(tab, file = "./output_MR_all/Assurland_emprunteur/tab_emp.RData")
## store clean price table 

#store_cleanprice(tab)

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


save(tab2, file = "./output_MR_all/Assurland_emprunteur/tab2_emp.RData")






load("./output_MR_all/Assurland_emprunteur/logevolfinal2_emp.RData")

colnames(logevolfinal2)[5]<-"type"

#oldcumul=unique(downloadCumulatedEvolution())
#oldcumul=na.omit(oldcumul)

cumulEvolResult <- logevolfinal2#[as.character(logevolfinal2$period)>max(oldcumul$period) ,]
tab = data.frame(period=cumulEvolResult$period,insurer=cumulEvolResult$insurer,type=cumulEvolResult$type,coverage=cumulEvolResult$coverage,mean=cumulEvolResult$mean)
tab = unique(tab)


## store cumulated price
StorecumulatedEvolution(tab)

#Rerun with new MRP on Jan
# AvgPrem_monthly_assurland
load("./output_MR_all/Assurland_Report/summaryTab.RData")

# stfinal = summaryTab[!summaryTab$period%in%unique(oldcumul$period),]
stfinal <- summaryTab[as.character(summaryTab$period)>max(oldcumul$period) ,]


tab = data.frame(period=stfinal$period,insurer=stfinal$insurer,avg=stfinal$AvgPremium,type=stfinal$PlayerType,formula=stfinal$coverage)
tab = unique(tab)

## store average premium
#Storeavepremium(tab)

#Rerun with new MRP on Jan
# Display_monthly_assurland

#tab = data.frame(period=stfinal$period,insurer=stfinal$insurer,display_prop=stfinal$Display_prop,type=stfinal$PlayerType,formula=stfinal$coverage)
#tab = unique(tab)
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

# store display

#StoredisplayEvolution(tab)

#####Pricegap classic function#######

load("./output_MR_all/Assurland_emprunteur/data_Assurland_emp.RData")
frdata_actual=frdata_actual[!duplicated(frdata_actual[c("profilID","insurer","coverage","period","yearmonth","prix")]),]
frdata1_new <- frdata_actual#MyData#[as.character(data$period)>max(liste_period$period),]

meanna=function(x) {
  return(mean(x,na.rm=T))
}

for(p in sort(unique(frdata1_new$yearmonth)))
{
  lastmonth=frdata1_new[frdata1_new$yearmonth==p & frdata1_new$insurer%in%Competitors,]
  lweekes=aggregate(lastmonth$prix ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
  names(lweekes)=c("profilID","insurer","coverage","prix")
  
  lweekeskminmarket=lweekes[!grepl("Groupe AXA",lweekes$insurer),]
  
  lweekeskminmarket=lweekeskminmarket[order(lweekeskminmarket$prix),]
  
  lweekeskminmarket=lweekeskminmarket[!duplicated(lweekeskminmarket[c("profilID","coverage")]),]
  
  three=lweekes[grepl("Groupe AXA", lweekes$insurer),]
  
  deltamineslast=merge(three,lweekeskminmarket,by=c("profilID","coverage"))
  
  
  deltamineslast$delta=(deltamineslast$prix.x/deltamineslast$prix.y)-1
  
  for(c in sort(unique(deltamineslast$coverage)))
  {
    deltaminesmars=deltamineslast[deltamineslast$coverage == 'Formule Optimum',]
    
    a=density(deltaminesmars$delta,bw = "sj")
    res=data.frame(x=a$x,y=a$y)
    
    pricegap_predata = data.frame(period = p, coverage = c, x = res$x,y = res$y)
    
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


######New Pricegap function#####


load("./Example_clean_prices_all/Assurland_Emprunteur/data_assurland_emprunteur.Rdata")
frdata_actual=frdata_actual[!duplicated(frdata_actual[c("profilID","insurer","coverage","period","yearmonth","prix")]),]
frdata1_new <- frdata_actual#MyData#[as.character(data$period)>max(liste_period$period),]

meanna=function(x) {
  return(mean(x,na.rm=T))
}

for(p in sort(unique(frdata1_new$yearmonth)))
{
  lastmonth=frdata1_new[frdata1_new$yearmonth==p & frdata1_new$insurer%in%Competitors,]
  lweekes=aggregate(lastmonth$prix ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
  names(lweekes)=c("profilID","insurer","coverage","prix")
  
  lweekeskminmarket=lweekes[!grepl("Groupe AXA",lweekes$insurer),]
  
  lweekeskminmarket=lweekeskminmarket[order(lweekeskminmarket$prix),]
  
  lweekeskminmarket=lweekeskminmarket[!duplicated(lweekeskminmarket[c("profilID","coverage")]),]
  
  three=lweekes[grepl("Groupe AXA", lweekes$insurer),]
  
  deltamineslast=merge(three,lweekeskminmarket,by=c("profilID","coverage"))
  
  
  deltamineslast$delta=(deltamineslast$prix.x/deltamineslast$prix.y)-1
  
  for(c in sort(unique(deltamineslast$coverage)))
  {
    deltaminesmars=deltamineslast[deltamineslast$coverage == 'Formule Optimum',]
    
    a=density(deltaminesmars$delta,bw = "sj")
    res=data.frame(x=a$x,y=a$y)
    
    pricegap_predata = data.frame(period = p, coverage = c, x = res$x,y = res$y)
    
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






##Subset
MyComplete$Seg_smoke[MyComplete$Seg_smoke=='oui'] <- 'Oui'
Badprofiles <- deltaminesmars[deltaminesmars$delta >= 0.9900646,]
Badfeatures <- subset(MyComplete, (profilID %in% Badprofiles$profilID))
Badfeatures <- Badfeatures[!duplicated(Badfeatures$profilID), ]

Badfeatures$insurer <- NULL
Badfeatures$coverage <- NULL
Badfeatures$date_extraction <- NULL
Badfeatures$period <- NULL
Badfeatures$yearmonth <- NULL
Badfeatures$Seg_location <- NULL
Badfeatures$delta <- Badprofiles$delta


##the whole dataset 
load("./output_MR_all/Assurland_emprunteur/Segment_table_all.RData")
MyComplete$Seg_smoke[MyComplete$Seg_smoke=='oui'] <- 'Oui'
Badprofiles <- deltaminesmars
Badfeatures <- subset(MyComplete, (profilID %in% Badprofiles$profilID))
Badfeatures <- Badfeatures[!duplicated(Badfeatures$profilID), ]

Badfeatures$insurer <- NULL
Badfeatures$coverage <- NULL
Badfeatures$date_extraction <- NULL
Badfeatures$period <- NULL
Badfeatures$yearmonth <- NULL
Badfeatures$Seg_location <- NULL
Badfeatures$delta <- Badprofiles$delta



library(rpart)
 
 
frmla = delta ~ loan_amount + loan_duration + age  + Seg_CSP  + Seg_smoke 

# Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)

fit = rpart(frmla, method="class", data=Badfeatures)


printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE, main="Classification Tree for bad profiles")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# tabulate some of the data
table(subset(raw, Koc>=190.5)$Metal)

## EVTREE (Evoluationary Learning)
install.packages('evtree')
library(evtree)


Badfeatures$loan_amount=Badfeatures$loan_amount/1000
Badfeatures$loan_amount=round(Badfeatures$loan_amount)
ev.raw = evtree(frmla, data=Badfeatures, control=evtree.control(minbucket=1L, alpha = 1.1))
plot(ev.raw)
text(ev.raw, cex=0.5)
table(predict(ev.raw), Badfeatures$delta)
1-mean(predict(ev.raw) == Badfeatures$delta)

##quantile plot
plot(quantile(Badfeatures$delta, probs = seq(0, 1, 0.025)), seq(0, 1, 0.025), xlab = 'quantile', ylab = '%')


save(pricegap_predata,file= paste0("./output_MR_all/Assurland_emprunteur/pricegap_emp.RData"))
frdatatemp=data.frame(frdata1_new$yearmonth,frdata1_new$profilID,frdata1_new$insurer,frdata1_new$coverage,frdata1_new$prix,frdata1_new$Segment)
names(frdatatemp)=c("period","profilID","insurer","coverage","prix","Segment")

frdatatemp_classique= frdatatemp[frdatatemp$insurer%in%CLASSIQUEPlayers,]

if(is.data.frame(frdatatemp_classique) && nrow(frdatatemp_classique)==0) {
  print("yes!")
}else {
  top3_classique=top_propor_Generic(frdatatemp_classique,top = 3)
  #Remove column segment
  top3_classique <- subset(top3_classique, select = - c(Segment))
  
  meanna=function(x) {
    return(mean(x,na.rm=T))
  }
  
  for(p in sort(unique(top3_classique$period)))
  {
    lastmonth=top3_classique[top3_classique$period==p,]
    lweekes=aggregate(lastmonth$prix ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
    names(lweekes)=c("profilID","insurer","coverage","prix")
    lweekes=unique(lweekes)
    
    for(cov in sort(unique(top3_classique$coverage)))
    {
      if(nrow(lweekes[lweekes$coverage==cov,]) != 0){
        dispesRCmar=aggregate(lweekes$prix[lweekes$coverage==cov] ~ lweekes$profilID[lweekes$coverage==cov], FUN= sd)[[2]]/aggregate(lweekes$prix[lweekes$coverage==cov] ~ lweekes$profilID[lweekes$coverage==cov], FUN=mean)[[2]]
        
        dispesRCmar = na.omit(dispesRCmar)
        res = density(dispesRCmar,bw = "sj")
        res$x=abs(min(res$x))+res$x
        res= data.frame(res$x,res$y)
        colnames(res) = c("x","y")
        marketdispersion_predata = data.frame(period=p,coverage=cov,x=res$x,y=res$y)
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
}


plot(marketdispersion_predata$x, marketdispersion_predata$y)
plot(pricegap_predata$x, pricegap_predata$y)

