source("./RCode/Analysis/SpatialAnalysis.R")
options(scipen = 999)

lhaz<-c("EQ","FL","TC","VO","DR","ET","LS","ST","WF","DZ")

# centrams<-c("COL","CRI","PAN","HND")
centrams<-c("IDN","NPL","PAK","TLS","LKA","MNG","FJI")

# impies<-GatherAllImps(lhaz)
# saveRDS(impies,"./CleanedData/MostlyImpactData/AllHaz_impies.RData")
impies<-readRDS("./CleanedData/MostlyImpactData/AllHaz_impies_20230928.RData")
impies%<>%filter(haz_Ab%in%lhaz)
# Create a variable to separate what is and isn't RC data 
impies%<>%mutate(RCnot="Not RC",RCnot=replace(RCnot, grepl("GO-",imp_src_db), "RC"))
# Add the year variable
impies$Year<-AsYear(impies$ev_sdate)
# Purely EM-DAT data for non-multiple entry visualisations
EMDAT<-impies%>%filter(imp_src_db=="EM-DAT" & ev_sdate>as.Date("2000-01-01"))
# Taxonomies
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Admin boundaries
ADM<-ImpactAggADM0(impies)

pal <- c(
  "EQ" = "magenta",
  "FL" = "blue",
  "TC" = "darkviolet",
  "VO" = "grey23",
  "DR" = "chocolate4",
  "ET" = "darkorange",
  "LS" = "peachpuff4",
  "ST" = "darkturquoise",
  "WF" = "red"
)

p<-impies%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                     Year>1970 & imp_value>0 & imp_src_db=="EM-DAT" &
                     haz_type!="haztypeenviron")%>%
  mutate(YearWindow=cut(Year, breaks=seq.int(1970,2020,by=10),labels=as.character(seq.int(1975,2020,by=10))))%>%
  group_by(imp_src_db,haz_type,YearWindow,ISO3)%>%
  summarise(imp_value=mean(imp_value))%>%
  ggplot()+geom_boxplot(aes(YearWindow, imp_value, fill=haz_type))+
  # geom_smooth(aes(YearWindow, imp_value, colour=haz_type))+
  scale_y_log10();p
  # facet_wrap(~imp_src_db, scales = "free");p

tmp<-impies%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                       Year>1970 & imp_value>0 & imp_src_db=="EM-DAT" &
                       haz_type!="haztypeenviron")

sapply(1975:2022, function(yeary){
  quantile(tmp$imp_value[tmp$Year<=yeary & tmp$Year>=yeary-5],
           probs=c(...................))
})

p<-impies%>%filter(imp_det=="impdetbuild" & imp_type=="imptypdama" &
                     Year>1970 & imp_value>0 & Year<2023 &
                     haz_type!="haztypeenviron")%>%
  group_by(imp_src_db,haz_type,Year,ISO3)%>%
  summarise(impval=mean(imp_value))%>%
  ggplot()+geom_point(aes(Year, impval, colour=haz_type))+
  geom_smooth(aes(Year, impval, colour=haz_type))+
  scale_y_log10()+
  facet_wrap(~imp_src_db, scales = "free");p


p<-impies%>%filter(impies$imp_cats=="impcatfineco" &
                     imp_subcats%in%c("impecoaid","impecodirtot","impecotot") &
                   imp_type%in%c("imptypcost","imptypaidreqifrc") &
                     imp_det%in%c("impdetaidgen","impdetinfloccur","impdetusdunsure") &
                     Year>1970 & Year<2023 & imp_value>0 & 
                     haz_type!="haztypeenviron")%>%
  ggplot()+geom_point(aes(Year, imp_value, colour=haz_type))+
  geom_smooth(aes(Year, imp_value, colour=haz_type))+
  scale_y_log10()+
  facet_wrap(~imp_src_db, scales = "free");p
  
  
imp_subcats%in%c("impecodirtot","impecoaid") &
  imp_type%in%c("imptypaidreqifrc","imptypcost") &


p<-impies%>%group_by(haz_Ab)%>%reframe(Count=length(unique(event_ID)))%>%arrange(desc(Count))%>%
  ggplot()+geom_bar(aes(x=haz_Ab,y=Count,fill=haz_Ab),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Number of Impact Records")+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_bar.png",p,path="./Plots/",width = 10,height = 8)  

p<-impies%>%group_by(imp_src_db,spat_res)%>%reframe(Count=length(unique(event_ID)))%>%
  ggplot()+geom_bar(aes(x=imp_src_db,y=Count,fill=spat_res),colour="black",stat="identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+
  labs(fill="Spatial Resolution");p
ggsave("AllHazards_imp_src_db_ADM_bar.png",p,path="./Plots/",width = 9,height = 8)  


p<-impies%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat")%>%
  ggplot()+geom_bar(aes(x=haz_Ab,fill=spat_res),colour="black")+
  xlab("Impact Database")+ylab("Number of Impact Records")+
  labs(fill="Spatial Resolution");p
ggsave("AllHazards_imp_src_db_bar.png",p,path="./Plots/",width = 9,height = 8)  

p<-impies%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" & 
                     imp_src_db!="GO-FR" &  ev_sdate>as.Date("2000-01-01"))%>%
  group_by(haz_Ab)%>%reframe(Deaths=sum(imp_value))%>%arrange(desc(Deaths))%>%
  ggplot()+geom_bar(aes(x=haz_Ab,y=Deaths,fill=haz_Ab),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Total Deaths")+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_deaths_bar.png",p,path="./Plots/",width = 10,height = 8)  


minitax<-taxies%>%filter(list_name=="imp_cats")%>%
  transmute(imp_cats=name,label=label)

p<-left_join(impies,minitax,by="imp_cats")%>%
  filter(!is.na(label))%>%
  ggplot()+geom_bar(aes(label,fill=label),colour="black")+
  xlab("Impact Category")+labs(fill="Impact Category")+
  ylab("Number of Impact Recordings")+
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.position="none");p
ggsave("AllHazards_impcats_bar.png",p,path="./Plots/")  


minitax1<-taxies%>%filter(list_name=="imp_subcats")%>%
  transmute(imp_subcats=name,label=label)
minitax2<-taxies%>%filter(list_name=="imp_type")%>%
  transmute(imp_type=name,label=label)
tmp<-left_join(left_join(impies,minitax1,by="imp_subcats"),minitax2,by="imp_type")
tmp$label.y[tmp$label.y=="Internally Displaced Persons (IDPs)"]<-"Internally Displaced (IDPs)"
tmp%<>%mutate(label=paste0(str_split(str_split(label.x," \\(",simplify = T)[,1]," – ",simplify = T)[,1],
                           " ",label.y))%>%
  filter(!grepl("NA",label))

# Group to show only the top-10 impacts
toppies<-names(sort(table(tmp$label),decreasing = T)[1:10])
tmp$label[!tmp$label%in%toppies]<-"Other"

tmp$label%<>%factor(levels=c(toppies,"Other"))

p<-tmp%>%ggplot()+geom_bar(aes(label,fill=label),colour="black")+
  xlab("Impact")+labs(fill="Impact")+
  ylab("Number of Impact Recordings")+
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.position="none");p
ggsave("AllHazards_impacts_bar.png",p,path="./Plots/")  

p<-PlotImpAgg(ADM,"N",F)+
  scale_fill_gradient(name = "No. Events",guide="legend", trans = "log",
                      high="chartreuse",low="chartreuse4",
                      breaks=rev(c(10,100, 1000, 10000, 100000, 1000000, 10000000)),
                      labels=rev(c("10^1","10^2","10^3","10^4","10^5","10^6","10^7")));p
ggsave("AllHazards_spatial.png",p,path="./Plots/",width = 13,height = 7)  


impies$Year<-AsYear(impies$ev_sdate)

p<-impies%>%filter(Year>1950 & Year<2024 & haz_Ab!="LS")%>%
  group_by(Year,haz_Ab)%>%reframe(Count=length(unique(event_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=haz_Ab))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  scale_colour_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_temporal.png",p,path="./Plots/",width = 13,height = 7)  

impies$Climate<-!impies$haz_Ab%in%c("EQ","VO")
impies$Climate[impies$haz_Ab=="LS"]<-NA

temp<-impies%>%filter(Year>1900 & Year<2024 & haz_Ab!="LS")%>%
  group_by(Year,Climate)%>%
  reframe(Count=length(unique(event_ID)))

p<-data.frame(Year=unique(temp$Year),Ratio=sapply(unique(temp$Year),function(Y){
  clim<-temp$Climate & temp$Year==Y
  noclim<-!temp$Climate & temp$Year==Y
  if(sum(clim)==0 & sum(noclim)==0) return(NA)
  if(sum(noclim)==0) return(Inf)
  if(sum(clim)==0) return(0)
  temp$Count[clim]/temp$Count[noclim]
}))%>%
  ggplot()+geom_point(aes(Year,Ratio))+geom_hline(yintercept = 1,colour="red");p
ggsave("Clim-Noclim_temporal.png",p,path="./Plots/",width = 13,height = 7)  


FL<-impies%>%filter(haz_Ab=="FL")%>%ImpactAggADM0()
# ST<-impies%>%filter(haz_Ab=="ST")%>%ImpactAggADM0()
# TC<-impies%>%filter(haz_Ab=="TC")%>%ImpactAggADM0()
EQ<-impies%>%filter(haz_Ab=="EQ")%>%ImpactAggADM0()

p<-PlotImpAgg(FL,loggie = F)+
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log",
                      # low="moccasin",high="green4",
                      breaks=c(10,100, 1000, 10000, 100000, 1000000, 10000000),
                      labels=c("10^1","10^2","10^3","10^4","10^5","10^6","10^7"))+
  guides(fill = guide_legend(reverse=TRUE));p
ggsave("FL_spatial_deaths.png",p,path="./Plots/",width = 13,height = 7)

p<-PlotImpAgg(EQ,loggie = F)+
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log",
                      low="moccasin",high="magenta",
                      breaks=c(10,100, 1000, 10000, 100000, 1000000, 10000000),
                      labels=c("10^1","10^2","10^3","10^4","10^5","10^6","10^7"));p
ggsave("EQ_spatial_deaths.png",p,path="./Plots/",width = 13,height = 7)

p<-impies%>%filter(Year>1950 & Year<2024 & haz_Ab!="LS" & imp_src_db!="GO-FR")%>%
  group_by(Year,imp_src_db)%>%reframe(Count=length(unique(event_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=imp_src_db))+
  scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+facet_wrap(~imp_src_db)+
  labs(colour="Database");p
ggsave("Alldatabases_temporal.png",p,path="./Plots/",width = 13,height = 7)  

p<-impies%>%filter(Year>1952 & Year<2023 & 
                     haz_Ab%in%c("EQ","VO") & 
                     imp_src_db!="GO-FR")%>%
  group_by(Year,imp_src_db)%>%
  reframe(Count=length(unique(event_ID)))%>%
  # filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=imp_src_db))+
  # geom_line(aes(Year,Count,colour=imp_src_db))+
  # scale_y_log10(n.breaks=5)+
  scale_x_continuous(n.breaks=15)+
  facet_wrap(~imp_src_db)+
  ylab("Number of Impact Records")+xlab("Year")+
  labs(colour="Database");p
ggsave("EQs_Alldatabases_temporal.png",p,path="./Plots/",width = 12,height = 8)  

p<-impies%>%filter(Year>1952 & Year<2023 & 
                     haz_Ab%in%c("EQ","VO") & 
                     imp_src_db!="GO-FR")%>%
  group_by(Year = findInterval(Year, seq(min(Year), max(Year), 5)),imp_src_db)%>%
  reframe(Count=length(unique(event_ID)))%>%
  mutate(Year=(Year-1)*5+1952.5)%>%
  # filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=imp_src_db))+
  geom_line(aes(Year,Count,colour=imp_src_db))+
  # scale_y_log10(n.breaks=5)+
  scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+xlab("5-Year Interval")+
  labs(colour="Database");p
ggsave("EQs_Alldatabases_temporal_5YrBin.png",p,path="./Plots/",width = 8,height = 5)  










#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GCDB WORKSHOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

p<-EMDAT%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" & 
                  ev_sdate>as.Date("2000-01-01"))%>%
  group_by(haz_Ab)%>%reframe(Deaths=sum(imp_value))%>%arrange(desc(Deaths))%>%
  ggplot()+geom_bar(aes(x=haz_Ab,y=Deaths,fill=haz_Ab),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Total Deaths")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma)+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_deaths_bar.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 8)  

p<-EMDAT%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" & 
                    ev_sdate>as.Date("2000-01-01"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(loggie = F) +
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);p
                      # breaks=c(10000000,1000000,100000,10000,1000,100,10),
                      # labels=c("10^7","10^6","10^5","10^4","10^3","10^2","10^1"));p
ggsave("allhaz_spatial_deaths.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(ev_sdate>as.Date("2000-01-01"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact = "N",loggie = F) +
  scale_fill_gradient(name = "No. Impact Records",guide="legend", trans = "log10",
                      high="chartreuse",low="chartreuse4");p
                      # breaks=c(10000,1000,100,10,1),
                      # labels=c("10^4","10^3","10^2","10^1","10^0"));p
ggsave("allhaz_spatial_records.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(ev_sdate>as.Date("2000-01-01") & imp_src_db=="GO-App")%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact = "N",loggie = F) +
  scale_fill_gradient(name = "No. Impact Records",guide="legend", trans = "log10",
                      high="orange4",low="orange");p
ggsave("allhaz_spatial_records_GOEmApps.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(haz_Ab%in%names(pal))%>%
  group_by(haz_Ab)%>%reframe(Records=length(imp_value))%>%arrange(desc(Records))%>%
  ggplot()+geom_bar(aes(x=haz_Ab,y=Records,fill=haz_Ab),colour="black",stat = "identity")+
  xlab("Hazard")+ylab("Total No. Impact Records")+
  scale_fill_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_records_bar.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 8)  

p<-impies%>%filter(Year>1950 & Year<2024 & haz_Ab!="LS" & haz_Ab%in%names(pal))%>%
  group_by(Year,haz_Ab)%>%reframe(Count=length(unique(event_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=haz_Ab))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  scale_colour_manual("Hazard",values = pal,limits = names(pal));p
ggsave("AllHazards_temporal.png",p,path="./Plots/GCDB_Workshop/",width = 8,height = 5)  

p<-impies%>%filter(imp_subcats%in%c("impecodirtot","impecoaid") &
                     imp_type%in%c("imptypaidreqifrc","imptypcost") &
                     ev_sdate>as.Date("2000-01-01") &
                     !is.na(haz_Ab) & haz_Ab%in%lhaz &
                     imp_src_db!="GO-FR")%>%
  group_by(haz_Ab,RCnot)%>%reframe(Cost=sum(imp_value))%>%
  ggplot()+geom_bar(aes(x=haz_Ab,y = Cost,fill=RCnot),colour="black",stat = "identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+#scale_y_log10()+
  labs(fill="Data Source")+facet_wrap(~RCnot,scales = "free");p
ggsave("AllHazards_imp_src_db_bar.png",p,path="./Plots/",width = 9,height = 8)  

p<-impies%>%filter(imp_subcats=="impecodirtot" &
                     imp_type=="imptypcost" &
                     ev_sdate>as.Date("2000-01-01") &
                     !imp_src_db%in%c("GO-FR","GO-App"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact="impecodirtot-imptypcost",loggie = F) +
  scale_fill_gradient(name = "Total Cost",guide="legend", trans = "log",
                      low="moccasin",high="magenta",
                      breaks=c(10000000,1000000,100000,10000,1000,100,10),
                      labels=c("10^7","10^6","10^5","10^4","10^3","10^2","10^1"));p
ggsave("NoGO_spatial_costs.png",p,path="./Plots/GCDB_Workshop/",width = 10)

p<-impies%>%filter(imp_subcats=="impecoaid" &
                   imp_type=="imptypaidreqifrc" &
                     ev_sdate>as.Date("2000-01-01") &
                     imp_src_db%in%c("GO-App"))%>%
  ImpactAggADM0()%>%
  PlotImpAgg(impact = "impecoaid-imptypaidreqifrc",loggie = F) +
  scale_fill_gradient(name = "Total Appeal Requested",guide="legend", trans = "log",
                      low="moccasin",high="magenta",
                      breaks=c(10000000,1000000,100000,10000,1000,100,10),
                      labels=c("10^7","10^6","10^5","10^4","10^3","10^2","10^1"));p
ggsave("GO_spatial_costs.png",p,path="./Plots/GCDB_Workshop/",width = 10)


tmp<-impies%>%filter(imp_type%in%c("imptypaidallifrc","imptypcost") &
                       ev_sdate>as.Date("2000-01-01") &
                       imp_src_db=="GO-App")

p<-impies%>%filter(ev_sdate>as.Date("2000-01-01") &
                     !is.na(haz_Ab) & haz_Ab%in%lhaz &
                     imp_src_db!="GO-FR")%>%
  group_by(haz_Ab,RCnot)%>%reframe(Counts=length(imp_value))%>%
  ggplot()+geom_bar(aes(x=haz_Ab,y = Counts,fill=RCnot),colour="black",stat = "identity")+
  xlab("Impact Database")+ylab("Number of Impact Records")+#scale_y_log10()+
  labs(fill="Data Source")+facet_wrap(~RCnot,scales = "free");p
ggsave("AllHazards_Records_bar_Gocomp.png",p,path="./Plots/GCDB_Workshop/",width = 9,height = 4)  


p<-impies%>%filter(Year>1950 & Year<2024 & imp_src_db!="GO-FR")%>%
  group_by(Year,RCnot)%>%reframe(Count=length(unique(event_ID)))%>%
  filter(Count>0)%>%
  ggplot()+geom_point(aes(Year,Count,colour=RCnot))+
  scale_y_log10(n.breaks=5)+scale_x_continuous(n.breaks=15)+
  ylab("Number of Impact Records")+
  labs(colour="Data Source");p
ggsave("RCnot_temporal.png",p,path="./Plots/GCDB_Workshop/",width = 8,height = 5)  

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COUNTRY-SPECIFIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

subbie<-impies%>%filter(ISO3%in%centrams)

p<-subbie%>%filter(!is.na(haz_Ab) & haz_Ab%in%lhaz &
                     ev_sdate>as.Date("2000-01-01") &
                     imp_src_db!="GO-FR")%>%
  group_by(RCnot,haz_Ab,ISO3)%>%reframe(Counts=length(imp_src_db))%>%
  ggplot(aes(x=ISO3,y=Counts,group=haz_Ab))+geom_bar(aes(x=ISO3,y=Counts,fill=haz_Ab),colour="black",stat = "identity")+
  xlab("Country ISO-3 Code")+ylab("Number of Impact Records")+#scale_y_log10()+
  # scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
  labs(fill="Data Source")+facet_wrap(~RCnot,scales = "free");p
ggsave("NS_allhazards_records_notRC_APR.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 5)  


p<-subbie%>%filter(!is.na(haz_Ab) & haz_Ab%in%lhaz &
                     imp_det=="impdetallpeop" & imp_type=="imptypdeat" & 
                     ev_sdate>as.Date("2000-01-01") &
                     imp_src_db!="GO-FR")%>%
  group_by(haz_Ab,ISO3)%>%reframe(Deaths=sum(imp_value))%>%
  ggplot()+geom_bar(aes(x=ISO3,y=Deaths,fill=haz_Ab),colour="black",stat="identity")+
  xlab("Country ISO-3 Code")+ylab("Total Deaths")+#scale_y_log10()+
  # scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
  labs(fill="Data Source"); p #+facet_wrap(~RCnot,scales = "free");p
ggsave("NS_allhazards_Deaths_APR.png",p,path="./Plots/GCDB_Workshop/",width = 9,height = 8)  


p<-subbie%>%filter(!is.na(haz_Ab) & haz_Ab%in%lhaz &
                     imp_subcats=="impecodirtot" & imp_type=="imptypcost" & 
                     ev_sdate>as.Date("2000-01-01") &
                     imp_src_db!="GO-FR")%>%
  group_by(haz_Ab,ISO3)%>%reframe(Cost=sum(imp_value))%>%
  ggplot()+geom_bar(aes(x=ISO3,y=Cost,fill=haz_Ab),colour="black",stat="identity")+
  xlab("Impact Database")+ylab("Total Economic Cost")+#scale_y_log10()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma)+
  # scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
  labs(fill="Data Source"); p #+facet_wrap(~RCnot,scales = "free");p
ggsave("NS_allhazards_Cost_APR.png",p,path="./Plots/GCDB_Workshop/",width = 10,height = 7)  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXCEEDANCE CURVE DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

impRP_calc<-function(indf,retper=5,timeline=NULL){
  # Time frame to correctly calculate return periods
  if(is.null(timeline))  timeline<-as.numeric(max(as.Date(indf$ev_sdate),na.rm = T)-
                                                min(as.Date(indf$ev_sdate),na.rm = T))/365
  # Exceedance probability curve data
  rankie<-data.frame(innie=sort(indf%>%pull(imp_value)%>%na.omit()),
                     ranking=1:nrow(indf))%>%
    mutate(prob=rev(ranking))
  # Plot?
  # rankie%>%ggplot()+geom_point(aes(innie,prob))+
  #   scale_y_log10() + scale_x_log10() +
  #   xlab("No. Deaths") + ylab("Annual Probability %")
  
  data.frame(impRP=rankie$innie[which.min(abs(1/rankie$prob-retper))],
             N=nrow(indf))
}


impies%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                  ev_sdate>1975 & !is.na(imp_value))%>%
  impRP_calc()

timeliner<-2023-1975

deathsRP<-do.call(rbind,lapply(unique(impies$ISO3),function(iso3){
  tryCatch(impies%>%filter(ISO3==iso3,
                           imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                             ev_sdate>1975 & !is.na(imp_value))%>%
             impRP_calc()%>%cbind(data.frame(ISO3=iso3,impact="Deaths",imp_src_db="All")),
           error=function(e) data.frame(impRP=NA,N=NA,ISO3=iso3,impact="Deaths",imp_src_db="All"))
}))%>%filter(!is.na(N))

# Extract Global ADM
ADM <- rworldmap::getMap(resolution='low')
ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
ADM@data%<>%left_join(deathsRP,by="ISO3")

ADM$impRP[ADM$N<15]<-NA

# Change the projection
crs_mappy <- "+proj=wintri +x_0=-74 +y_0=0 +datum=WGS84 +no_defs +over" # TRIPEL PROJECTION
# crs_mappy <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=0" # Lambert azimuthal equal-area projection
projdat <- lwgeom::st_transform_proj(st_as_sf(ADM), crs = crs_mappy)
# Add the grid layout to the plot (graticule)
grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  lwgeom::st_transform_proj(crs = crs_mappy)
# To add the blue background to the map
lats <- c(90:-90, -90:90, 90)
# longs <- c(rep(c(180-74, -180-74), each = 181), 180-74)
longs <- c(rep(c(180, -180), each = 181), 180)
# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  lwgeom::st_transform_proj(crs = crs_mappy)

# Plot it out!
q<-ggplot()+
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = "black", alpha=0.5) +
  geom_sf(data = grat_wintri, color = "gray30", linewidth = 0.05, alpha=0.1) +
  geom_sf(data=projdat,aes(fill=impRP), color = "grey30", linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Expected No. Deaths Assoc. 5-Year Return Period")+
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q
ggsave("allhaz_spatial_deaths_5Yr-RP.png",q,path="./Plots/GCDB_Workshop/",width = 10)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# Economic cost RP too!
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

costRP<-do.call(rbind,lapply(unique(impies$ISO3),function(iso3){
  tryCatch(impies%>%filter(ISO3==iso3,
                           imp_subcats%in%c("impecotot","impecodirtot") & imp_type=="imptypcost" & 
                             ev_sdate>1975 & !is.na(imp_value))%>%
             impRP_calc()%>%cbind(data.frame(ISO3=iso3,impact="Deaths",imp_src_db="All")),
           error=function(e) data.frame(impRP=NA,N=NA,ISO3=iso3,impact="Deaths",imp_src_db="All"))
}))%>%filter(!is.na(N))

# Extract Global ADM
ADM <- rworldmap::getMap(resolution='low')
ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
ADM@data%<>%left_join(costRP,by="ISO3")

ADM$impRP[ADM$N<15]<-NA

ADM$impRP<-ADM$impRP/1e6

# Change the projection
crs_mappy <- "+proj=wintri +x_0=-74 +y_0=0 +datum=WGS84 +no_defs +over" # TRIPEL PROJECTION
# crs_mappy <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=0" # Lambert azimuthal equal-area projection
projdat <- lwgeom::st_transform_proj(st_as_sf(ADM), crs = crs_mappy)
# Add the grid layout to the plot (graticule)
grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  lwgeom::st_transform_proj(crs = crs_mappy)
# To add the blue background to the map
lats <- c(90:-90, -90:90, 90)
# longs <- c(rep(c(180-74, -180-74), each = 181), 180-74)
longs <- c(rep(c(180, -180), each = 181), 180)
# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  lwgeom::st_transform_proj(crs = crs_mappy)

# Plot it out!
q<-ggplot()+
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = "black", alpha=0.5) +
  geom_sf(data = grat_wintri, color = "gray30", linewidth = 0.05, alpha=0.1) +
  geom_sf(data=projdat,aes(fill=impRP), color = "grey30", linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Expected Economic Cost Assoc. 5-Year Return Period")+
  scale_fill_gradient(name = "Total Cost (Millions USD-2011)",guide="legend", trans = "log10",
                      high="chartreuse",low="chartreuse4",n.breaks=6);q
ggsave("allhaz_spatial_cost_5Yr-RP.png",q,path="./Plots/GCDB_Workshop/",width = 10)












timeliner<-2023-1975

deathsRP<-do.call(rbind,lapply(unique(impies$ISO3),function(iso3){
  tryCatch(impies%>%filter(ISO3==iso3 &
                           haz_Ab=="FL" & 
                           imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                             ev_sdate>1975 & !is.na(imp_value))%>%
             impRP_calc()%>%cbind(data.frame(ISO3=iso3,impact="Deaths",imp_src_db="All")),
           error=function(e) data.frame(impRP=NA,N=NA,ISO3=iso3,impact="Deaths",imp_src_db="All"))
}))%>%filter(!is.na(N))

# Extract Global ADM
ADM <- rworldmap::getMap(resolution='low')
ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
ADM@data%<>%left_join(deathsRP,by="ISO3")

ADM$impRP[ADM$N<15]<-NA
# ADM$impRP<-1e5*ADM$impRP/ADM$Population

# Change the projection
crs_mappy <- "+proj=wintri +x_0=-74 +y_0=0 +datum=WGS84 +no_defs +over" # TRIPEL PROJECTION
# crs_mappy <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=0" # Lambert azimuthal equal-area projection
projdat <- lwgeom::st_transform_proj(st_as_sf(ADM), crs = crs_mappy)
# Add the grid layout to the plot (graticule)
grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  lwgeom::st_transform_proj(crs = crs_mappy)
# To add the blue background to the map
lats <- c(90:-90, -90:90, 90)
# longs <- c(rep(c(180-74, -180-74), each = 181), 180-74)
longs <- c(rep(c(180, -180), each = 181), 180)
# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  lwgeom::st_transform_proj(crs = crs_mappy)

# Plot it out!
q<-ggplot()+
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = "black", alpha=0.5) +
  geom_sf(data = grat_wintri, color = "gray30", linewidth = 0.05, alpha=0.1) +
  geom_sf(data=projdat,aes(fill=impRP), color = "grey30", linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Expected Deaths 5-Year Return Period")+
  scale_fill_gradient(name = "Total Deaths",guide="legend", trans = "log10",
                      low="magenta4",high="magenta",n.breaks=6);q
ggsave("FL_spatial_deaths_5Yr-RP.png",q,path="./Plots/GCDB_Workshop/",width = 10, height=7)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# Economic cost RP too!
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

costRP<-do.call(rbind,lapply(unique(impies$ISO3),function(iso3){
  tryCatch(impies%>%filter(ISO3==iso3,
                           imp_subcats%in%c("impecotot","impecodirtot") & imp_type=="imptypcost" & 
                             haz_Ab=="FL",
                             ev_sdate>1975 & !is.na(imp_value))%>%
             impRP_calc()%>%cbind(data.frame(ISO3=iso3,impact="Deaths",imp_src_db="All")),
           error=function(e) data.frame(impRP=NA,N=NA,ISO3=iso3,impact="Deaths",imp_src_db="All"))
}))%>%filter(!is.na(N))

# Extract Global ADM
ADM <- rworldmap::getMap(resolution='low')
ADM@data%<>%transmute(ISO3=ISO_A3,Population=POP_EST,GDP=GDP_MD_EST)
ADM@data%<>%left_join(costRP,by="ISO3")

ADM$impRP[ADM$N<15]<-NA

ADM$impRP<-ADM$impRP/1e6

# Change the projection
crs_mappy <- "+proj=wintri +x_0=-74 +y_0=0 +datum=WGS84 +no_defs +over" # TRIPEL PROJECTION
# crs_mappy <- "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=0" # Lambert azimuthal equal-area projection
projdat <- lwgeom::st_transform_proj(st_as_sf(ADM), crs = crs_mappy)
# Add the grid layout to the plot (graticule)
grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  lwgeom::st_transform_proj(crs = crs_mappy)
# To add the blue background to the map
lats <- c(90:-90, -90:90, 90)
# longs <- c(rep(c(180-74, -180-74), each = 181), 180-74)
longs <- c(rep(c(180, -180), each = 181), 180)
# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  lwgeom::st_transform_proj(crs = crs_mappy)

# Plot it out!
q<-ggplot()+
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = "black", alpha=0.5) +
  geom_sf(data = grat_wintri, color = "gray30", linewidth = 0.05, alpha=0.1) +
  geom_sf(data=projdat,aes(fill=impRP), color = "grey30", linewidth=0.1) + #, inherit.aes = FALSE) +
  coord_sf(datum = NULL) + #ylim(c(-80,80)) +
  # coord_sf(datum = crs_mappy, xlim = c(-100,-40), ylim=c(-30,30), expand = FALSE) + #ylim(c(-80,80)) + 
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="white",
                                         linetype="solid", 
                                         colour ="black"),
        plot.title = element_text(hjust = 0.5,face="bold",size=18),
        legend.position = c(0.1, 0.1))+
  ggtitle("Expected Economic Cost Assoc. 5-Year Return Period")+
  scale_fill_gradient(name = "Total Cost (Millions USD-2011)",guide="legend", trans = "log10",
                      high="chartreuse",low="chartreuse4",n.breaks=6);q
ggsave("FL_spatial_cost_5Yr-RP.png",q,path="./Plots/GCDB_Workshop/",width = 10)











#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% DESINVENTAR SUB-NATIONAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

sapply(seq_along(centrams),function(i){
  
  print(centrams[i])
  
  iso3<-centrams[i]
  
  cntimps<-impies%>%filter(imp_src_db=="Desinventar" & ISO3==iso3)
  
  filer<-paste0("./CleanedData/SocioPoliticalData/Desinventar/",
                stringr::str_to_lower(iso3),
                "/ADM_",stringr::str_to_lower(iso3),
                ".geojson")
  
  if(!file.exists(filer)) return(F)
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  if(iso3%in%c("NPL","LKA")){
    ADM<-ADM[nchar(ADM$ADMcode)==9,]
  } else if(iso3=="PAK"){
    ADM<-ADM[nchar(ADM$ADMcode)==3,]
  } 
  
  ADM$Allrecords<-sapply(ADM$ADMcode,function(codie){
    sum(grepl(codie,cntimps$imp_spat_ID,ignore.case = T))
  })
  
  ADM$deathsRP<-sapply(ADM$ADMcode,function(codie){
    output<-tryCatch(cntimps%>%filter(imp_det=="impdetallpeop" & imp_type=="imptypdeat" &
                      ev_sdate>1975 & !is.na(imp_value) &
                      grepl(codie,imp_spat_ID,ignore.case = T))%>%
      impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
    ifelse(output$N>30,output$impRP,NA)
  })
    
  ADM$costRP<-sapply(ADM$ADMcode,function(codie){
    output<-tryCatch(cntimps%>%filter(imp_subcats%in%c("impecotot","impecodirtot") & imp_type=="imptypcost" & 
                                        ev_sdate>1975 & !is.na(imp_value) &
                                        grepl(codie,imp_spat_ID,ignore.case = T))%>%
                       impRP_calc(),error=function(e) data.frame(impRP=NA,N=NA))
    ifelse(output$N>30,output$impRP/1e6,NA)
  })  
    
  # Extract the bounding box of the admin boundaries
  # bbox<-expandBbox(unlist(unname(ExtractBBOXpoly(ADM)[1,2:5])),3)

  mad_map <- get_stamenmap(expandBbox(ADM@bbox,3),source = "stamen",maptype = "terrain",zoom=5)

  p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
  
  q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=deathsRP), color = "grey30", linewidth=0.1, inherit.aes = FALSE) +
    scale_fill_gradient("Exp. No. Deaths 5Yr RP",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("DeathsRP_Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=costRP), color = "grey30", linewidth=0.1, inherit.aes = FALSE) +
    scale_fill_gradient("Exp. Cost 5Yr RP [Millions Local Curr]",high="chartreuse",low="chartreuse4",trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("CostRP_Allrecords_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  sapply(lhaz,function(hazzie){
    
    ADM$records<-sapply(ADM$ADMcode,function(codie){
      sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
    })
    
    q<-ADM[ADM$ADMlevel==min(max(ADM$ADMlevel),2),]%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+
      scale_fill_gradient(paste0("No. Records - ",hazzie), high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM2_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    q<-ADM[ADM$ADMlevel==min(ADM$ADMlevel),]%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM1_",iso3,"_Dessie.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    return(T)},simplify = T)
  
  return(T)},simplify = T)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%% EM-DAT SUB-NATIONAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

sapply(seq_along(centrams),function(i){
  
  iso3<-centrams[i]
  
  cntimps<-impies%>%filter(imp_src_db=="EM-DAT" & ISO3==iso3)
  
  filer<-paste0("./CleanedData/SocioPoliticalData/EMDAT/",
                iso3,"/ADM_",iso3,
                ".geojson")
  
  if(!file.exists(filer)) return(F)
  
  ADM<-geojsonio::geojson_read(filer, what = "sp")
  
  ADM1<-aggregate(ADM, by = "ADM1_CODE")
  ADM2<-aggregate(ADM, by = "ADM2_CODE")
  
  ADM2$Allrecords<-sapply(ADM2$ADM2_CODE,function(codie){
    sum(grepl(codie,cntimps$imp_spat_ID,ignore.case = T))
  })
  
  q<-ADM2%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM2_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  ADM1$Allrecords<-sapply(ADM1$ADM1_CODE,function(codie){
    sum(grepl(codie,cntimps$imp_spat_ID,ignore.case = T))
  })
  
  q<-ADM1%>%st_as_sf()%>%ggplot()+
    geom_sf(aes(fill=Allrecords), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
    scale_fill_gradient("No. Records",low="magenta4", high="magenta", trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
  ggsave(paste0("Allrecords_ADM1_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
  
  
  sapply(lhaz,function(hazzie){
    
    ADM2$records<-sapply(ADM2$ADM2_CODE,function(codie){
      sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
    })
    
    q<-ADM2%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM2_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    ADM1$records<-sapply(ADM1$ADM1_CODE,function(codie){
      sum(grepl(codie,cntimps$imp_spat_ID[cntimps$haz_Ab==hazzie],ignore.case = T))
    })
    
    q<-ADM1%>%st_as_sf()%>%ggplot()+
      geom_sf(aes(fill=records), color = "grey30", linewidth=0.1)+ #, inherit.aes = FALSE) +
      scale_fill_gradient(paste0("No. Records - ",hazzie),high=pal[names(pal)==hazzie], trans = "log10",na.value = "black");q #, inherit.aes = FALSE) +
    ggsave(paste0(hazzie,"_records_ADM1_",iso3,"_EMDAT.png"),q,path="./Plots/GCDB_Workshop/Sub-national/",width = 10)  
    
    return(T)},simplify = T)
  
  return(T)},simplify = T)








q<-impies%>%filter(!is.na(Continent) & Continent!="Not Classified" &
                  imp_src_db!="GO-FR")%>%
  group_by(imp_src_db,Year,Continent)%>%reframe(Counts=length(imp_value))%>%
  ggplot(aes(Year,Counts,group=Continent))+geom_point(aes(colour=Continent))+
  scale_y_log10()+xlim(c(1975,2022))+
  geom_smooth(aes(colour=Continent,fill=Continent))+
  ylab("No. of Impact Records")+facet_wrap(~imp_src_db)
ggsave("Temporal_Bias_Continents-databases.png",q,path="./Plots/",width = 12)  











































