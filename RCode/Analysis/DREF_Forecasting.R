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
    "imp_UNRegion","imp_WorldBankRegion","imp_Continent","imp_UNSubRegion",
    "imp_WorldBankIncomeGroup","imp_spat_covlab","imp_srcdb_code"
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





stop("Add the ability to not remove NA haz_Ab values from GO-DREF")





# Get the DREF data and convert into tabular format
tDREF<-convGOApp_Monty()%>%procDBfore()
# Same for EM-DAT
tEMDAT<-convEMDAT_Monty()%>%procDBfore()

# Filter out all years previous to 2008
tDREF%<>%filter(ev_sdate>=as.Date("2008-01-01"))

tDREF%>%mutate(year=AsYear(ev_sdate))%>%
  filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  group_by(year)%>%reframe(Cost=sum(imp_value))%>%
  ggplot()+geom_line(aes(year,Cost))

# Empty skeleton
foreDREF<-data.frame(Month=month.abb[month(rep(Sys.Date()+c(30,60,90)))])
# All non-CC related hazards, take median, Q05 and Q95 values per month
nonCC<-tDREF%>%filter(haz_Ab_grp=="non-CC" & exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  mutate(month=month(ev_sdate),year=AsYear(ev_sdate))%>%group_by(year,month)%>%
  reframe(Cost=sum(imp_value))%>%ungroup()%>%
  reframe(DREF=median(Cost),lowDREF=quantile(Cost,probs=0.25),uppDREF=quantile(Cost,0.75))
foreDREF%<>%cbind(rbind(nonCC,nonCC,nonCC)); rm(nonCC)

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

costie<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)" &
                         !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
                         haz_Ab=="FL")%>%
  mutate(year=AsYear(ev_sdate))%>%
  group_by(worldbankregion,Season,year)%>%
  reframe(Cost=sum(imp_value))

costie%>%ggplot()+geom_boxplot(aes(Season,Cost,fill=worldbankregion))+
  scale_y_log10()+
  facet_wrap(~worldbankregion)
# 
costie%>%ggplot()+geom_smooth(aes(year,Cost,colour=worldbankregion),se=F)+
  scale_y_log10()
# 


Seasonality<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)" &
                 !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
                 !(worldbankregion=="Middle East & North Africa" & haz_Ab_grp=="TC"))%>%
  group_by(worldbankregion,haz_Ab_grp,Season,year)%>%
  reframe(Cost=sum(imp_value))%>%
  group_by(worldbankregion,haz_Ab_grp,Season)%>%
  reframe(DREF=median(Cost),lowDREF=quantile(Cost,probs=0.25),uppDREF=quantile(Cost,0.75))%>%
  group_by(Season)%>%
  reframe(DREF=sum(DREF),lowDREF=sum(lowDREF),uppDREF=sum(uppDREF))
  
CCcost<-cbind(data.frame(Month=month.abb[month(Sys.Date()+1:3*30)]),
      Seasonality[unname(sapply(getSeason(Sys.Date()+1:3*30),function(ss) which(Seasonality$Season==ss))),2:4]/4)

foreDREF[,2:4]<-CCcost[,2:4]%>%as.matrix()+foreDREF[,2:4]%>%as.matrix()

foreDREF
# 













# Group by month, per year
# Check non-CC hazard time trends
# Prophet model per region
# Add the values together



costie<-tDREF%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
  filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)")%>%
  group_by(MnYr)%>%
  reframe(Cost=sum(imp_value),
          Month=unique(AsMonth(ev_sdate)),
          Year=unique(AsYear(ev_sdate)))
costie$Month[nchar(costie$Month)==1]<-paste0("0",costie$Month[nchar(costie$Month)==1])

costie%<>%mutate(Date=as.Date(paste0(Year,"-",Month,"-15")))%>%
  dplyr::select(-c(MnYr))

costie$Cost%<>%log10()

costie%<>%dplyr::select(Date,Cost)%>%setNames(c("ds","y"))
# 
mody<-prophet(costie,growth = "flat",
              weekly.seasonality = F,
              daily.seasonality = F,
              yearly.seasonality = T,
              seasonality.mode = "multiplicative")
# 
future <- make_future_dataframe(mody, periods = 90)
forecast <- predict(mody, future)

plot(mody, forecast)


# 


costie%>%arrange(Date)%>%group_by(worldbankregion)%>%reframe(Year=Year,Cost=cumsum(Cost))%>%
  ggplot()+geom_line(aes(Year,Cost,colour=worldbankregion))+ scale_y_log10(limits=c(3e6,5e8))+
  xlim(c(2010,2024))

costie%>%arrange(Date)%>%group_by(worldbankregion,Month)%>%reframe(Cost=sum(Cost))%>%
  ggplot()+geom_smooth(aes(as.numeric(Month),Cost,colour=worldbankregion),se=F)+
  facet_wrap(~worldbankregion)+
  scale_x_continuous(breaks=1:12,labels=month.abb)+scale_y_log10()
  








costie<-tEMDAT%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
  filter(imp_type_lab=="Deaths" & 
           !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
           haz_Ab=="TC")%>%
  group_by(worldbankregion,MnYr)%>%
  reframe(Cost=sum(imp_value),
          Month=unique(AsMonth(ev_sdate)),
          Year=unique(AsYear(ev_sdate)))
costie$Month[nchar(costie$Month)==1]<-paste0("0",costie$Month[nchar(costie$Month)==1])

costie%<>%mutate(Date=as.Date(paste0(Year,"-",Month,"-15")))%>%
  dplyr::select(-c(MnYr))

costie$Cost%<>%log10()

costie%<>%dplyr::select(worldbankregion,Date,Cost)%>%setNames(c("worldbankregion","ds","y"))
# 
mody<-lapply(unique(costie$worldbankregion),function(ww){
  mody<-costie%>%filter(worldbankregion==ww)%>%
    prophet(growth = "linear",
              daily.seasonality = F,
            weekly.seasonality = F,
              yearly.seasonality = T)
    future <- make_future_dataframe(mody, periods = 90)
    list(mody,predict(mody, future))
})

i<-7
unique(costie$worldbankregion)[i]
plot(mody[[i]][[1]], mody[[i]][[2]])
prophet_plot_components(mody[[i]][[1]], mody[[i]][[2]])
# 


costie<-tEMDAT%>%mutate(MnYr=paste0(AsMonth(ev_sdate),"-",AsYear(ev_sdate)))%>%
  filter(imp_type_lab=="Deaths" & 
           !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
           haz_Ab=="TC")%>%
  group_by(MnYr)%>%
  reframe(Cost=sum(imp_value),
          Month=unique(AsMonth(ev_sdate)),
          Year=unique(AsYear(ev_sdate)))
costie$Month[nchar(costie$Month)==1]<-paste0("0",costie$Month[nchar(costie$Month)==1])

costie%<>%mutate(Date=as.Date(paste0(Year,"-",Month,"-15")))%>%
  dplyr::select(-c(MnYr))

costie$Cost%<>%log10()

costie%<>%dplyr::select(Date,Cost)%>%setNames(c("ds","y"))
# 
mody<-costie%>%
    prophet(growth = "linear",
            daily.seasonality = F,
            weekly.seasonality = F,
            yearly.seasonality = T)
future <- make_future_dataframe(mody, periods = 90)
forecast<-predict(mody, future)
plot(mody,forecast)
prophet_plot_components(mody,forecast)






costie<-tEMDAT%>%
  filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted" & 
           !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
           haz_Ab=="FL")%>%
  mutate(imp_value=log10(imp_value))%>%
  dplyr::select(worldbankregion,ev_sdate,imp_value)%>%setNames(c("worldbankregion","ds","y"))
# 
mody<-lapply(unique(costie$worldbankregion),function(ww){
  mody<-costie%>%filter(worldbankregion==ww)%>%
    prophet(growth = "linear",
            daily.seasonality = F,
            weekly.seasonality = F,
            yearly.seasonality = T)
  future <- make_future_dataframe(mody, periods = 90)
  list(mody,predict(mody, future))
})

i<-3
unique(costie$worldbankregion)[i]
plot(mody[[i]][[1]], mody[[i]][[2]])
prophet_plot_components(mody[[i]][[1]], mody[[i]][[2]])









costie%>%arrange(Date)%>%
  # filter(worldbankregion%in%c("East Asia & Pacific","Europe & Central Asia","South Asia"))%>%
  group_by(worldbankregion,Month)%>%reframe(Cost=sum(Cost))%>%
  ggplot()+geom_smooth(aes(as.numeric(Month),Cost,colour=worldbankregion),se = F)+
  # facet_wrap(~worldbankregion,scales = "free_y")+
  scale_x_continuous(breaks=1:12,labels=month.abb)+scale_y_log10(limits=c(10,NA))







# costie%>%group_by(worldbankregion)%>%ggplot()+geom_point(aes(Date,Cost,colour=worldbankregion))



















cumsumy<-tDREF%>%filter(exp_spec_lab=="IFRC Aid Contributions (Unspecified-Inflation-Adjustment)" & 
                 imp_srcdb_code=="GO-DREF" & !is.na(imp_WorldBankRegion))%>%
  mutate(days=as.numeric(as.Date(ev_sdate)-as.Date("2008-01-01")))%>%
  arrange(days)%>%
  group_by(haz_Ab_grp,exp_spec_lab,exp_subcat_lab,imp_type_lab,imp_WorldBankRegion)%>%
  reframe(days=days,
          cumfund=cumsum(imp_value),
          Database="DREF",
          noEvents=length(imp_type_lab))
  
cumsumy%>%ggplot()+geom_line(aes(days,cumfund,colour=imp_WorldBankRegion))+
  scale_y_log10()+ facet_wrap(~haz_Ab_grp)

cumsumy%>%ggplot()+geom_line(aes(cos(pi*days/365),cumfund,colour=imp_WorldBankRegion))+
  scale_y_log10()+ facet_wrap(~haz_Ab_grp)


tEMDAT$days<-as.numeric(as.Date(tEMDAT$ev_sdate)-min(as.Date(tEMDAT$ev_sdate),na.rm = T))

tEMDAT$DayOfYear<-as.numeric(as.Date(tEMDAT$ev_sdate)-as.Date(paste0(tEMDAT$year,"-01-01")))


# For each year, group by DayOfYear and calculate the total sum, per hazard, per region
# tEMDAT%>%filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted")%>%
#   group_by(haz_Ab_grp,imp_WorldBankRegion,DayOfYear)%>%
#   reframe(MonthlyCost=imp_value[as.Date(ev_sdate)<=as.Date(dd)+15 &
#                                          as.Date(tEMDAT$ev_sdate)>as.Date(dd)-15)


















tEMDAT%>%filter(imp_type_lab=="Deaths" & 
                  !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
                  haz_Ab%in%c("EQ","FL") &
                  year>2000)%>%
  mutate(Month=factor(as.numeric(AsMonth(ev_sdate)),labels = month.abb))%>%
  group_by(haz_Ab,imp_WorldBankRegion,Month)%>%
  reframe(Cost=sum(imp_value)/(max(year)-min(year)))%>%
  ggplot()+geom_col(aes(Month,Cost,fill=haz_Ab), colour="black", width = 1, position = "identity") +
  coord_polar() + facet_wrap(~imp_WorldBankRegion) + scale_y_log10()

colnom<-rep(month.abb,3);names(colnom)=as.character(1:(12L*3L))

tmp<-tEMDAT
tmp%<>%rbind(tmp%>%mutate(DayOfYear=DayOfYear-365),tmp%>%mutate(DayOfYear=DayOfYear+365))
tmp%>%filter(imp_type_lab=="Deaths" & 
               !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
               haz_Ab_grp%in%c("FL","TC","ST","non-CC"))%>%
  ggplot()+geom_density(aes(DayOfYear,after_stat(count),fill=haz_Ab_grp), colour="black", alpha=0.4)+
  facet_wrap(~imp_WorldBankRegion)+
  scale_x_continuous(breaks=(0:13*30),labels=c(" ",month.abb," "),limits=c(1,365+30))
  # coord_polar()


tmp%>%filter(imp_type_lab=="Deaths" & 
               !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
               haz_Ab%in%c("FL","TC","ST","EQ") &
               year>2000)%>%
  ggplot()+geom_density(aes(Month,after_stat(count),fill=haz_Ab), colour="black", alpha=0.4)+
  facet_wrap(~imp_WorldBankRegion)+
  scale_x_continuous(breaks=0:13,labels=c(" ",month.abb," "),limits=c(0,13))


tEMDAT%>%filter(imp_type_lab=="Deaths" & 
                  !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
                  haz_Ab%in%c("FL") &
                  year>2000)%>%
  mutate(Month=factor(as.numeric(AsMonth(ev_sdate)),labels = month.abb))%>%
  group_by(haz_Ab,imp_WorldBankRegion,Month)%>%
  reframe(Cost=sum(imp_value))%>%
  ggplot()+geom_col(aes(Month,Cost,fill=haz_Ab), colour="black", width = 1, position = "identity") +
  coord_polar() +  facet_wrap(~imp_WorldBankRegion) + scale_y_sqrt()
 



tEMDAT%>%filter(imp_type_lab=="Deaths" & 
                  !is.na(imp_WorldBankRegion) & imp_WorldBankRegion!="Not Classified" & 
                  haz_Ab%in%c("EQ") &
                  year>2000)%>%
  mutate(Month=factor(as.numeric(AsMonth(ev_sdate)),labels = month.abb))%>%
  # group_by(haz_Ab,imp_WorldBankRegion,Month)%>%
  ggplot()+geom_density(aes(as.numeric(Month),y=imp_value,fill=imp_WorldBankRegion), colour="black", alpha=0.2)

# tEMDAT$MonthlyCost<-sapply(tEMDAT$ev_sdate,function(dd){
#   tEMDAT$imp_value[as.Date(tEMDAT$ev_sdate)<=as.Date(dd)+15 &
#                    as.Date(tEMDAT$ev_sdate)>as.Date(dd)-15 &
#                    ]
# }))















tmp<-tEMDAT%>%
  filter(exp_spec_lab=="Total Direct Costs Inflation-Adjusted" & 
           !is.na(imp_WorldBankRegion))

cumsumy<-do.call(rbind,lapply(365:(max(tEMDAT$days)-min(tEMDAT$days)),
                     function(dd){
                       tmp[tmp$days<=dd & tmp$days>dd-30,]%>%
                         group_by(haz_Ab_grp,imp_WorldBankRegion)%>%
                         reframe(days=dd,
                                 cumfund=sum(imp_value),
                                 Database="EM-DAT",
                                 noEvents=length(imp_type_lab))
}))




cumsumy%>%ggplot() + stat_density(aes(cos(2*pi*days/365),cumfund),
                                  breaks = seq(0,2*pi), width = 2, colour = "grey") + 
  coord_polar(start = 0) +
  facet_wrap(~haz_Ab_grp)


cumsumy<-tEMDAT%>%filter(exp_spec_lab=="Aid Contributions (Unspecified-Inflation-Adjustment)" & 
                          !is.na(imp_WorldBankRegion))%>%
  mutate(days=as.numeric(as.Date(ev_sdate)-min(as.Date(ev_sdate),na.rm = T)))%>%
  arrange(days)%>%
  group_by(haz_Ab_grp,exp_spec_lab,exp_subcat_lab,imp_type_lab,imp_WorldBankRegion,year)%>%
  reframe(days=days,
          cumfund=cumsum(imp_value),
          Database="EMDAT",
          noEvents=length(imp_type_lab))

cumsumy%>%ggplot()+geom_line(aes(days,cumfund,colour=imp_WorldBankRegion))+
  scale_y_log10()+ facet_wrap(~haz_Ab_grp)

cumsumy%>%ggplot()+geom_line(aes(cos(2*pi*days/365),cumfund,colour=imp_WorldBankRegion))+
  scale_y_log10()+ facet_wrap(~haz_Ab_grp)






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