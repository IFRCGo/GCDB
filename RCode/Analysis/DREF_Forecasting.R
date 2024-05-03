# Create table of number of DREF's per country, then take the top-10

# Now plot seasonality for that country with respect to a given hazard, using DREF data

# Now using EM-DAT, Desinventar and IDMC data

# 

procDBfore<-function(Monty){
  # Events database
  ev<-Monty_Ev2Tab(Monty)
  # Impacts database
  imp<-Monty_Imp2Tab(Monty)
  # Housecleaning
  rm(Monty)
  # Select only the required columns for the analysis
  ev_cols<-c(
    "event_ID","ev_sdate","ev_fdate","ev_ISO3s","unregion","worldbankregion",
    "continent","unsubregion","worldbankincomegroup","haz_Ab"
  )
  imp_cols<-c(
    "event_ID","imp_sub_ID","exp_spec_lab","exp_subcat_lab","exp_cat_lab",
    "imp_value","imp_type_lab","imp_unit_lab","imp_sdate","imp_fdate","imp_ISO3",
    "imp_unregion","imp_worldbankregion","imp_continent","imp_unsubregion",
    "imp_worldbankincomegroup","imp_spat_covlab","imp_srcdb_code"
  )
  ev%<>%dplyr::select(all_of(ev_cols))
  imp%<>%dplyr::select(all_of(imp_cols))
  # Left-join event + impact data
  taby<-left_join(ev,imp,by="event_ID"); rm(ev,imp)
  # Also group hazards to make up the numbers
  taby$haz_Ab_grp<-taby$haz_Ab; 
  taby$haz_Ab_grp[taby$haz_Ab_grp%in%c("DR","HW","WF","ET")]<-"HT"
  taby$haz_Ab_grp[taby$haz_Ab_grp%in%c("EQ","LS","VO","TS")]<-"non-CC"
  # Extract the year information
  taby$year<-AsYear(taby$ev_sdate)
  
  return(taby)
}
# Get the DREF data and convert into tabular format
tDREF<-convGOApp_Monty()%>%procDBfore()
# Filter out all years previous to 2008
tDREF%<>%filter(ev_sdate>=as.Date("2008-01-01") & imp_srcdb_code=="GO-DREF")

# Same for EM-DAT
tEMDAT<-convEMDAT_Monty()%>%procDBfore()

# Check the average number of events + total yearly funding allocated
#@@@ reflects that, other than 2010, number of events funded and total yearly funding has grown since 2018
tDREF%>%mutate(ev_sdate=as.Date(ev_sdate))%>%arrange(ev_sdate)%>%
  filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  group_by(year)%>%reframe(Count=length(year),Cost=sum(imp_value))%>%
  ggplot()+geom_point(aes(year,Count,size=Cost,colour=Cost))+geom_line(aes(year,Count))

# Check only the median yearly funding allocated per event
#@@@ reflects that median funding allocation per event has also been growing
#@@@ and also, as previous, the number of events is growing since 2018
tDREF%>%mutate(ev_sdate=as.Date(ev_sdate))%>%arrange(ev_sdate)%>%
  filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  group_by(year)%>%reframe(Count=length(year),
                                     Sum=sum(imp_value),
                                     Cost=median(imp_value))%>%
  ggplot()+geom_point(aes(year,Cost,size=Count,colour=Sum))+
  geom_text(aes(year,Cost+2e4,label=Count))+
  geom_line(aes(year,Cost))+
  xlab("Year")+ylab("Yearly Median Funding Allocated Per Event")

# Split by region
#@@@ shows that Asia and Africa dominate the statistics
tDREF%>%mutate(ev_sdate=as.Date(ev_sdate))%>%arrange(ev_sdate)%>%
  filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  group_by(year,continent)%>%reframe(Count=length(year),
                           Sum=sum(imp_value),
                           Cost=median(imp_value))%>%
  ggplot()+geom_point(aes(year,Cost,size=Count,colour=Sum))+
  geom_text(aes(year,Cost+2e4,label=Count))+
  geom_line(aes(year,Cost))+
  xlab("Year")+ylab("Yearly Median Funding Allocated Per Event")+
  facet_wrap(~continent)

# Calculate the average amount allocated to each disaster per year
#@@@ We see that Floods dominate roughly 60% of all allocated funds
tmp<-tDREF%>%mutate(ev_sdate=as.Date(ev_sdate))%>%arrange(ev_sdate)%>%
  filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  group_by(year,haz_Ab)%>%
  reframe(Count=length(year),
          Sum=sum(imp_value),
          Cost=median(imp_value))
tmp%>%group_by(year,haz_Ab)%>%
  reframe(Proportion=100*Sum/sum(tmp$Sum[tmp$year==year]))%>%
  ggplot()+geom_point(aes(year,Proportion,colour=haz_Ab))
  


#@@@ If we can accurately predict floods in Africa and Asia, the rest will follow suit


dateyyy<-Sys.Date()
yearyyy<-AsYear(dateyyy)
monthyyy<-AsMonth(dateyyy)



# There is a clear phase difference between certain years. Let's fit the model piecemeal
# Thus, for both phase 1 and 2, do the following:
# Non-CC hazards - calculate average number of yearly events, and cost allocated per event
nonCCcost<-tDREF%>%filter(haz_Ab_grp=="Non-CC" & exp_spec=="expspec_ecoifrcall")%>%
  mutate(month=month(ev_sdate))%>%group_by(worldbankregion,year,month)%>%
  reframe(Cost=sum(imp_value))%>%
  filter(!(year==yearyyy & month==monthyyy))%>%
  group_by(worldbankregion)%>%
  reframe(Month=month.abb[month(rep(dateyyy+c(30,60,90)))],
          DREF=median(Cost),
          lowDREF=quantile(Cost,probs=0.05),
          uppDREF=quantile(Cost,0.95))%>%mutate(Hazard="Non-CC")    
# CC hazards - calculate average number of yearly events, and cost allocated per event
# Calculate the seasonality of the event occurrence, then split the cost per month
Seasonality<-

# Then for phase 2, add the extra step of:
# Re-calculate average number of yearly events, and funding allocated per event
# See whether there is a difference in the number of yearly events, 
# or just the funding allocated per event
# Do this for both non-CC and CC


# FOR LATER:
# How to handle EM-DAT + DREF data?
# For EM-DAT, per country, calculate the probability of 














# CC-related hazards, auto-regression at a seasonal level
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

tDREF$Season<-getSeason(tDREF$ev_sdate)%>%
  factor(levels=c("Spring","Summer","Fall","Winter"))

Seasonality<-tDREF%>%filter(haz_Ab_grp=="CC" & exp_spec=="expspec_ecoifrcall" & 
                 !is.na(worldbankregion) & worldbankregion!="Not Classified" & 
                 !(worldbankregion=="Middle East & North Africa"))%>%
  group_by(worldbankregion,haz_Ab,Season,year)%>%
  reframe(Cost=sum(imp_value))%>%
  group_by(worldbankregion,haz_Ab,Season)%>%
  reframe(DREF=median(Cost),lowDREF=quantile(Cost,probs=0.05),uppDREF=quantile(Cost,0.95))%>%
  rename("Hazard"="haz_Ab")
  # group_by(worldbankregion,Season)%>%
  # reframe(DREF=sum(DREF)/4,lowDREF=sum(lowDREF)/4,uppDREF=sum(uppDREF)/4)
  
Seasonality%>%group_by(worldbankregion,Season)%>%reframe(DREF=sum(DREF))%>%
  ggplot()+geom_point(aes(Season,DREF))+facet_wrap(~worldbankregion,scales = "fixed")

numy<-as.character(as.numeric(factor(month.abb,levels=month.abb))); numy[nchar(numy)==1]<-paste0("0",numy[nchar(numy)==1])
monthy<-data.frame(numMonth=1:12,Month=month.abb,Season=getSeason(paste0("2000-",numy,"-15")))

CCcost<-left_join(Seasonality,monthy,
          relationship="many-to-many",by="Season")%>%
  dplyr::select(-Season)

intrpC<-CCcost%>%filter(Month%in%c())

# Bind CC and non-CC together
foreDREF<-rbind(CCcost,nonCCcost)%>%
  group_by(worldbankregion,Month)%>%
  reframe(DREF=sum(DREF),lowDREF=sum(lowDREF),uppDREF=sum(uppDREF))

facy<-sapply(unique(foreDREF$Month),function(x) which(x==month.abb)); facy<-facy-min(facy)+1; facy<-sort(facy)
foreDREF$Month%<>%factor(levels=names(facy),labels=names(facy))
foreDREF%<>%arrange(Month)

# Per region
foreDREF%>%group_by(Month,worldbankregion)%>%
  reframe(DREF=sum(DREF),lowDREF=sum(lowDREF),uppDREF=sum(uppDREF))
# Total
foreDREF%>%group_by(Month)%>%
  reframe(DREF=sum(DREF),lowDREF=sum(lowDREF),uppDREF=sum(uppDREF))
# 

library(RColorBrewer)
coul <- brewer.pal(length(unique(foreDREF$worldbankregion)), "Set2")

p<-foreDREF%>%ggplot(aes(Month,DREF,fill=worldbankregion))+geom_bar(colour="black",stat="identity",width=0.6)+ #,position=position_dodge())+
  labs(fill="Region")+ylab("Predicted DREF Allocation")+scale_fill_manual(values=coul); p
ggsave(path = "./Plots/DREF_Forecasting/",paste0("DREF_Forecast_",Sys.Date(),".png"),p,height=6,width=8)

tots<-foreDREF%>%group_by(Month)%>%
  reframe(worldbankregion="Total",
          DREF=sum(DREF),lowDREF=sum(lowDREF),uppDREF=sum(uppDREF))
foreDREF<-rbind(foreDREF,tots)%>%
  arrange(Month)

foreDREF%>%openxlsx::write.xlsx(paste0("./Analysis_Results/DREF_forecast_",Sys.Date(),".xlsx"))
  # geom_errorbar(aes(x=Month, ymin=lowDREF, ymax=uppDREF), width=.2,
  #               position=position_dodge(0.9))
# 



mnCost<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  arrange(ev_sdate)%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
  group_by(MnYr)%>%reframe(Cost=sum(imp_value),Month=unique(AsMonth(ev_sdate)),year=unique(year))

mnCost$Month[nchar(mnCost$Month)==1]<-paste0("0",mnCost$Month[nchar(mnCost$Month)==1])
mnCost%<>%mutate(Date=paste0(year,"-",Month,"-15"))

mnCost%>%ggplot()+geom_line(aes(as.Date(Date),Cost))
# 

tDREF[as.Date(tDREF$ev_sdate)>=as.Date("2024-04-01") & tDREF$exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)",]%>%
  pull(imp_value)%>%sum()
# 
mnCost%>%filter(year>2020 & year<=2024 & Month=="04")%>%dplyr::select(Cost)
# 
mnEvs<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  arrange(ev_sdate)%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
  group_by(haz_Ab,MnYr)%>%reframe(NoEvents=length(imp_value),Month=unique(AsMonth(ev_sdate)),year=unique(year))

mnEvs$Month[nchar(mnEvs$Month)==1]<-paste0("0",mnEvs$Month[nchar(mnEvs$Month)==1])
mnEvs%<>%mutate(Date=paste0(year,"-",Month,"-15"))  

mnEvs$Date%<>%as.Date()
mnEvs%<>%arrange(Date)%>%group_by(haz_Ab,Date)%>%
  reframe(NoEvents=sum(mnEvs$NoEvents[mnEvs$Date<=Date & mnEvs$haz_Ab==haz_Ab]))
mnEvs%>%ggplot()+geom_line(aes(as.Date(Date),NoEvents,colour=haz_Ab))+scale_y_log10()

mnEvs%>%group_by(haz_Ab)%>%reframe(Date=Date,Diff=c(NA_real_,diff(NoEvents)))%>%View()
  ggplot()+geom_line(aes(Date,Diff,colour=haz_Ab))
# 




























# costie<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)" &
#                          !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#                          haz_Ab=="FL")%>%
#   mutate(year=AsYear(ev_sdate))%>%
#   group_by(worldbankregion,Season,year)%>%
#   reframe(Cost=sum(imp_value))
# 
# costie%>%ggplot()+geom_boxplot(aes(Season,Cost,fill=worldbankregion))+
#   scale_y_log10(limits=c(NA,1e7))+
#   facet_wrap(~worldbankregion)
# # 
# costie%>%ggplot()+geom_smooth(aes(year,Cost,colour=worldbankregion),se=F)+
#   scale_y_log10()
# # 
# 
# 
# 
# 
# costie<-tDREF%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
#   filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
#   group_by(MnYr)%>%
#   reframe(Cost=sum(imp_value),
#           Month=unique(AsMonth(ev_sdate)),
#           Year=unique(AsYear(ev_sdate)))
# costie$Month[nchar(costie$Month)==1]<-paste0("0",costie$Month[nchar(costie$Month)==1])
# 
# costie%<>%mutate(Date=as.Date(paste0(Year,"-",Month,"-15")))%>%
#   dplyr::select(-c(MnYr))
# 
# costie$Cost%<>%log10()
# 
# costie%<>%dplyr::select(Date,Cost)%>%setNames(c("ds","y"))
# # 
# mody<-prophet(costie,growth = "flat",
#               weekly.seasonality = F,
#               daily.seasonality = F,
#               yearly.seasonality = T,
#               seasonality.mode = "multiplicative")
# # 
# future <- make_future_dataframe(mody, periods = 90)
# forecast <- predict(mody, future)
# 
# plot(mody, forecast)
# 
# 
# # 
# 
# 
# costie%>%arrange(Date)%>%group_by(worldbankregion)%>%reframe(Year=Year,Cost=cumsum(Cost))%>%
#   ggplot()+geom_line(aes(Year,Cost,colour=worldbankregion))+ scale_y_log10(limits=c(3e6,5e8))+
#   xlim(c(2010,2024))
# 
# costie%>%arrange(Date)%>%group_by(worldbankregion,Month)%>%reframe(Cost=sum(Cost))%>%
#   ggplot()+geom_smooth(aes(as.numeric(Month),Cost,colour=worldbankregion),se=F)+
#   facet_wrap(~worldbankregion)+
#   scale_x_continuous(breaks=1:12,labels=month.abb)+scale_y_log10()
#   
# 
# 
# 
# 
# 
# 
# 
# 
# costie<-tEMDAT%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
#   filter(imp_type_lab=="Deaths" & 
#            !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#            haz_Ab=="TC")%>%
#   group_by(worldbankregion,MnYr)%>%
#   reframe(Cost=sum(imp_value),
#           Month=unique(AsMonth(ev_sdate)),
#           Year=unique(AsYear(ev_sdate)))
# costie$Month[nchar(costie$Month)==1]<-paste0("0",costie$Month[nchar(costie$Month)==1])
# 
# costie%<>%mutate(Date=as.Date(paste0(Year,"-",Month,"-15")))%>%
#   dplyr::select(-c(MnYr))
# 
# costie$Cost%<>%log10()
# 
# costie%<>%dplyr::select(worldbankregion,Date,Cost)%>%setNames(c("worldbankregion","ds","y"))
# # 
# mody<-lapply(unique(costie$worldbankregion),function(ww){
#   mody<-costie%>%filter(worldbankregion==ww)%>%
#     prophet(growth = "linear",
#               daily.seasonality = F,
#             weekly.seasonality = F,
#               yearly.seasonality = T)
#     future <- make_future_dataframe(mody, periods = 90)
#     list(mody,predict(mody, future))
# })
# 
# i<-7
# unique(costie$worldbankregion)[i]
# plot(mody[[i]][[1]], mody[[i]][[2]])
# prophet_plot_components(mody[[i]][[1]], mody[[i]][[2]])
# # 
# 
# 
# costie<-tEMDAT%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
#   filter(imp_type_lab=="Deaths" & 
#            !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#            haz_Ab=="TC")%>%
#   group_by(MnYr)%>%
#   reframe(Cost=sum(imp_value),
#           Month=unique(AsMonth(ev_sdate)),
#           Year=unique(AsYear(ev_sdate)))
# costie$Month[nchar(costie$Month)==1]<-paste0("0",costie$Month[nchar(costie$Month)==1])
# 
# costie%<>%mutate(Date=as.Date(paste0(Year,"-",Month,"-15")))%>%
#   dplyr::select(-c(MnYr))
# 
# costie$Cost%<>%log10()
# 
# costie%<>%dplyr::select(Date,Cost)%>%setNames(c("ds","y"))
# # 
# mody<-costie%>%
#     prophet(growth = "linear",
#             daily.seasonality = F,
#             weekly.seasonality = F,
#             yearly.seasonality = T)
# future <- make_future_dataframe(mody, periods = 90)
# forecast<-predict(mody, future)
# plot(mody,forecast)
# prophet_plot_components(mody,forecast)
# 
# 
# 
# 
# 
# 
# costie<-tEMDAT%>%
#   filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted" & 
#            !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#            haz_Ab=="FL")%>%
#   mutate(imp_value=log10(imp_value))%>%
#   dplyr::select(worldbankregion,ev_sdate,imp_value)%>%setNames(c("worldbankregion","ds","y"))
# # 
# mody<-lapply(unique(costie$worldbankregion),function(ww){
#   mody<-costie%>%filter(worldbankregion==ww)%>%
#     prophet(growth = "linear",
#             daily.seasonality = F,
#             weekly.seasonality = F,
#             yearly.seasonality = T)
#   future <- make_future_dataframe(mody, periods = 90)
#   list(mody,predict(mody, future))
# })
# 
# i<-3
# unique(costie$worldbankregion)[i]
# plot(mody[[i]][[1]], mody[[i]][[2]])
# prophet_plot_components(mody[[i]][[1]], mody[[i]][[2]])
# 
# 
# 
# 
# 
# 
# 
# 
# 
# costie%>%arrange(Date)%>%
#   # filter(worldbankregion%in%c("East Asia & Pacific","Europe & Central Asia","South Asia"))%>%
#   group_by(worldbankregion,Month)%>%reframe(Cost=sum(Cost))%>%
#   ggplot()+geom_smooth(aes(as.numeric(Month),Cost,colour=worldbankregion),se = F)+
#   # facet_wrap(~worldbankregion,scales = "free_y")+
#   scale_x_continuous(breaks=1:12,labels=month.abb)+scale_y_log10(limits=c(10,NA))
# 
# 
# 
# 
# 
# 
# 
# # costie%>%group_by(worldbankregion)%>%ggplot()+geom_point(aes(Date,Cost,colour=worldbankregion))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# cumsumy<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)" & 
#                  imp_srcdb_code=="GO-DREF" & !is.na(imp_worldbankregion))%>%
#   mutate(days=as.numeric(as.Date(ev_sdate)-as.Date("2008-01-01")))%>%
#   arrange(days)%>%
#   group_by(haz_Ab_grp,exp_spec_lab,exp_subcat_lab,imp_type_lab,imp_worldbankregion)%>%
#   reframe(days=days,
#           cumfund=cumsum(imp_value),
#           Database="DREF",
#           noEvents=length(imp_type_lab))
#   
# cumsumy%>%ggplot()+geom_line(aes(days,cumfund,colour=imp_worldbankregion))+
#   scale_y_log10()+ facet_wrap(~haz_Ab_grp)
# 
# cumsumy%>%ggplot()+geom_line(aes(cos(pi*days/365),cumfund,colour=imp_worldbankregion))+
#   scale_y_log10()+ facet_wrap(~haz_Ab_grp)
# 
# 
# tEMDAT$days<-as.numeric(as.Date(tEMDAT$ev_sdate)-min(as.Date(tEMDAT$ev_sdate),na.rm = T))
# 
# tEMDAT$DayOfYear<-as.numeric(as.Date(tEMDAT$ev_sdate)-as.Date(paste0(tEMDAT$year,"-01-01")))
# 
# 
# # For each year, group by DayOfYear and calculate the total sum, per hazard, per region
# # tEMDAT%>%filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted")%>%
# #   group_by(haz_Ab_grp,imp_worldbankregion,DayOfYear)%>%
# #   reframe(MonthlyCost=imp_value[as.Date(ev_sdate)<=as.Date(dd)+15 &
# #                                          as.Date(tEMDAT$ev_sdate)>as.Date(dd)-15)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# tEMDAT%>%filter(imp_type_lab=="Deaths" & 
#                   !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#                   haz_Ab%in%c("EQ","FL") &
#                   year>2000)%>%
#   mutate(Month=factor(as.numeric(AsMonth(ev_sdate)),labels = month.abb))%>%
#   group_by(haz_Ab,imp_worldbankregion,Month)%>%
#   reframe(Cost=sum(imp_value)/(max(year)-min(year)))%>%
#   ggplot()+geom_col(aes(Month,Cost,fill=haz_Ab), colour="black", width = 1, position = "identity") +
#   coord_polar() + facet_wrap(~imp_worldbankregion) + scale_y_log10()
# 
# colnom<-rep(month.abb,3);names(colnom)=as.character(1:(12L*3L))
# 
# tmp<-tEMDAT
# tmp%<>%rbind(tmp%>%mutate(DayOfYear=DayOfYear-365),tmp%>%mutate(DayOfYear=DayOfYear+365))
# tmp%>%filter(imp_type_lab=="Deaths" & 
#                !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#                haz_Ab_grp%in%c("FL","TC","ST","non-CC"))%>%
#   ggplot()+geom_density(aes(DayOfYear,after_stat(count),fill=haz_Ab_grp), colour="black", alpha=0.4)+
#   facet_wrap(~imp_worldbankregion)+
#   scale_x_continuous(breaks=(0:13*30),labels=c(" ",month.abb," "),limits=c(1,365+30))
#   # coord_polar()
# 
# 
# tmp%>%filter(imp_type_lab=="Deaths" & 
#                !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#                haz_Ab%in%c("FL","TC","ST","EQ") &
#                year>2000)%>%
#   ggplot()+geom_density(aes(Month,after_stat(count),fill=haz_Ab), colour="black", alpha=0.4)+
#   facet_wrap(~imp_worldbankregion)+
#   scale_x_continuous(breaks=0:13,labels=c(" ",month.abb," "),limits=c(0,13))
# 
# 
# tEMDAT%>%filter(imp_type_lab=="Deaths" & 
#                   !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#                   haz_Ab%in%c("FL") &
#                   year>2000)%>%
#   mutate(Month=factor(as.numeric(AsMonth(ev_sdate)),labels = month.abb))%>%
#   group_by(haz_Ab,imp_worldbankregion,Month)%>%
#   reframe(Cost=sum(imp_value))%>%
#   ggplot()+geom_col(aes(Month,Cost,fill=haz_Ab), colour="black", width = 1, position = "identity") +
#   coord_polar() +  facet_wrap(~imp_worldbankregion) + scale_y_sqrt()
#  
# 
# 
# 
# tEMDAT%>%filter(imp_type_lab=="Deaths" & 
#                   !is.na(imp_worldbankregion) & imp_worldbankregion!="Not Classified" & 
#                   haz_Ab%in%c("EQ") &
#                   year>2000)%>%
#   mutate(Month=factor(as.numeric(AsMonth(ev_sdate)),labels = month.abb))%>%
#   # group_by(haz_Ab,imp_worldbankregion,Month)%>%
#   ggplot()+geom_density(aes(as.numeric(Month),y=imp_value,fill=imp_worldbankregion), colour="black", alpha=0.2)
# 
# # tEMDAT$MonthlyCost<-sapply(tEMDAT$ev_sdate,function(dd){
# #   tEMDAT$imp_value[as.Date(tEMDAT$ev_sdate)<=as.Date(dd)+15 &
# #                    as.Date(tEMDAT$ev_sdate)>as.Date(dd)-15 &
# #                    ]
# # }))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# tmp<-tEMDAT%>%
#   filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted" & 
#            !is.na(imp_worldbankregion))
# 
# cumsumy<-do.call(rbind,lapply(365:(max(tEMDAT$days)-min(tEMDAT$days)),
#                      function(dd){
#                        tmp[tmp$days<=dd & tmp$days>dd-30,]%>%
#                          group_by(haz_Ab_grp,imp_worldbankregion)%>%
#                          reframe(days=dd,
#                                  cumfund=sum(imp_value),
#                                  Database="EM-DAT",
#                                  noEvents=length(imp_type_lab))
# }))
# 
# 
# 
# 
# cumsumy%>%ggplot() + stat_density(aes(cos(2*pi*days/365),cumfund),
#                                   breaks = seq(0,2*pi), width = 2, colour = "grey") + 
#   coord_polar(start = 0) +
#   facet_wrap(~haz_Ab_grp)
# 
# 
# cumsumy<-tEMDAT%>%filter(exp_spec_lab=="Aid Contributions (Unspecified-Inflation-Adjustment)" & 
#                           !is.na(imp_WorldBankRegion))%>%
#   mutate(days=as.numeric(as.Date(ev_sdate)-min(as.Date(ev_sdate),na.rm = T)))%>%
#   arrange(days)%>%
#   group_by(haz_Ab_grp,exp_spec_lab,exp_subcat_lab,imp_type_lab,imp_WorldBankRegion,year)%>%
#   reframe(days=days,
#           cumfund=cumsum(imp_value),
#           Database="EMDAT",
#           noEvents=length(imp_type_lab))
# 
# cumsumy%>%ggplot()+geom_line(aes(days,cumfund,colour=imp_WorldBankRegion))+
#   scale_y_log10()+ facet_wrap(~haz_Ab_grp)
# 
# cumsumy%>%ggplot()+geom_line(aes(cos(2*pi*days/365),cumfund,colour=imp_WorldBankRegion))+
#   scale_y_log10()+ facet_wrap(~haz_Ab_grp)






# Plot some stuff!
# UNSubRegion time-series of cumulative DREF and EM-DAT, on a log-scale









# REMEMBER: NON-HYDROMET HAZARDS DO NOT HAVE SEASONALITY, MODEL SEPARATELY








# Time series analyis with irregular data
#     Predict on 1-month accumulated DREF, per hazard, per sub-region, on the 18th of each month 
#     For out-of-sample validation, do this analysis with hold out of next 3 months over 1:8 years (thus 2016-2024)
#     Take the cumulative DREF funding value, per hazard, per sub-region, since 2008
#     Take the cumulative EM-DAT deaths, number of events, aid contributions and total cost, per hazard, per sub-region, since 2008
#     Do a simple regression of these variables
#     Check the residuals for each out-of-sample fold, comparing e.g. if predictions were made in 20016 to 2019 to 2021 and per season
#     Then apply autoregression on the residuals AFTER AGGREGATING EACH SUBREGION, since ...2018...? Plot the residuals
#     Do this for each country to see whether performance is comparable




# Convert into usable form... imp_tab and ev_tab



# Data wrangling
# Only DREF after 2008, IDMC since 2016 and EM-DAT since 2000
# 


# EM-DAT data, per country, per hazard in DREF hazards, per impact type,
#     make the monthly seasonality using Fourier Series and one yearly general trend
#     do this for a few countries in central Americas that have had rpevious DREFs
#     MAKE THE FUNCTION INDEPENDENT OF DATABASE/ORGANISATION

# What should I plot? 
#     DREF flood allocations, per sub-region (UN sub-regions), with respect to monthly seasonality
#     DREF cumulative funding per country, per year (all hazards)
#     DREF 1st fourier series component per country

# Predict DREF allocated budget per sub-region, cumulative per month
# Hurdle model based on EM-DAT fourier decomposition 